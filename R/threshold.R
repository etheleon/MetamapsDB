#' dynamicThreshold tries to identify the lower bound converage value in order to remove low quality contigs.
#' Number of contigs tend to stablize at a value when removing reads below a certain readnum and we try to identify the min read required for that region of stability
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param maxLimit maximum cutoff
#' @param increment incrememnt
#' @param contigLengths the length of the contigs
#' @param mode two options either readnum or coverage; coverage is experimental
#'
#' @export
dynamicThreshold = function(rs, maxLimit = 100, increment=1, contigLengths, mdrContigs, mode="readnum"){

#if(mode == 'readnum'){
    thresholds = seq(1, maxLimit, increment)
    keptDF = lapply(thresholds, contigsSurvive.repeats, rs) %>% do.call(rbind,.)

    myFunc = function(vec){ abs(vec[2] - vec[1]) }

    windowMEAN = data.frame(
        #the mean repeats
        rollingMean = keptDF %$% zoo::rollapply(repeats, width=2, by=1, FUN = myFunc) %>% zoo::rollapply(width = 10, by = 5, FUN= mean),
        meanDepth = keptDF %$% zoo::rollapply(thresholds[-1], width = 10, by = 5, FUN= mean)
    )
    threshold = filter(windowMEAN, rollingMean < 0.5) %>%
                filter(meanDepth == min(meanDepth)) %$% meanDepth %>% ceiling
    return(list(thres = threshold, repeats = keptDF))
#} else if (mode == 'coverage'){
    #thresholds = seq(0, maxLimit, increment)
    #keptDF = lapply(thresholds, contigsSurvive.repeats.coverage, rs) %>% do.call(rbind,.)
    #myFunc = function(vec){ abs(vec[2] - vec[1]) }

	#cois = paste0('K00927',":", (as.character(unique(rs$X5..Contig))))
	#coverageDF = lapply(cois, function(coi){dbquery("MATCH (c:contigs{contig : {coi}}) RETURN c.contig as ID, toFloat(c.readnum) / toFloat(c.length) as coverage", params=list(coi = coi), verbose=T) }) %>% do.call(rbind,.)

    #windowMEAN = data.frame(
        #rollingMean = keptDF %$% zoo::rollapply(repeats, width=2, by=1, FUN = myFunc) %>% zoo::rollapply(width = 10, by = 5, FUN= mean),
        #meanDepth = keptDF %$% zoo::rollapply(thresholds[-1], width = 10, by = 5, FUN= mean)
    #)
    #threshold = filter(windowMEAN, rollingMean < 0.5) %>%
                #filter(meanDepth == min(meanDepth)) %$% meanDepth %>% ceiling
    #list(thres = threshold, repeats = keptDF)
#}

#contigsSurvive.repeats.coverage <- function(readDepth, coverageDF){
	#coverageDF$X5..Contig = strsplit(as.character(coverageDF$ID), ':') %>% lapply('[[', 2) %>% do.call(c,.)
	#coverageDF = unique(merge(coverageDF, rs, by="X5..Contig",all.y=T))
    #coverageDF %>% filter(coverage > readDepth) %>% group_by(readOrigin) %>% summarise(repeats = n())  %>% filter(repeats > 1) %$% sum(repeats) %>% data.frame(readDepth, repeats = .)
#}
}

#' contigsSurvive.repeats
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param readDepth the minimum depth
#'
contigsSurvive.repeats <- function(readDepth, rs){
    rs %>% group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% filter(count > readDepth) %>%
        group_by(readOrigin) %>% summarise(repeats = n())  %>%
        filter(repeats > 1) %$% sum(repeats) %>%
    data.frame(readDepth, repeats = .)
}


#' readStatus stores the assignment details of a simulation into the graphDB
#' </rootDir/ko/454ReadStatus.txt> from newbler
#'
#' @param root the root directory
#' @param koi the ko directory
#'
#' @export
readStatusReader <- function(root, koi, mdr=FALSE)
{
	readStatus = read.csv(sprintf("%s/%s/454ReadStatus.txt", root, koi), sep="\t") %>% as.data.table
	readStatus$Accno %<>% as.character
	regexstr = "^\\d+\\|(\\d+)"
	readAlloc = readStatus %$% regmatches(Accno, regexec(regexstr,Accno)) %>% sapply(`[`,2 )
	rs = readStatus %>% 
		select(Read.Status, X5..Contig) %>%
		mutate(readOrigin = readAlloc) %>%
		filter(Read.Status == 'Assembled')
	if(mdr){
		df = dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 RETURN c.contig as id", params = list(ko = gsub("^(ko:)*", "ko:", koi)))
		mdrContigs = df$id %>% as.character %>% strsplit(':') %>% lapply(function(i) i[[2]]) %>% do.call(rbind,.)
		rs %>% filter(X5..Contig %in% mdrContigs)
	}else{
		rs
	}
}
