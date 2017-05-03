#' superglue
#'
#' Based on NEWBLER's contig graph link contigsTogether
#'
#' @param koi
#' @param root
superglue <- function(root, koi){
    pairInfo = sprintf("%s/%s/454ContigGraph.txt", root, koi) %>% readLines %>%
        lapply(function(line) if(grepl("^P", line)) line) %>% do.call(c,.) %>% (function(x) x[!is.null(x)]) %>%
        textConnection %>% read.table %>% .[,-1] %>% setNames(c("contig", "five", "three"))

    padding = function(x) sprintf("contig%05d", as.integer(x))
    splitter = function(x, coi) {
        indivFunc = function(ele) {
            strsplit(ele, '/') %>% unlist %>% (function(x) { data.frame(connected2 = as.character(padding(x[[1]])), nread = as.integer(x[[2]]), gap = as.numeric(x[[3]]))})
        }
        if(x != '-'){
            strsplit(x, ';') %>% unlist %>% lapply(indivFunc) %>% do.call(rbind,.) %>% mutate(origin = coi)
        }else{
            NA
        }
}


mappingInfo <- function(blasdf, root, koi, outputDir, query, rangesFile, genomesFile, blastFile, doBlast=F, ...){
    cmd = paste(findPython(), root, koi, outputDir, query, rangesFile, genomesFile)
    if(doBlast){
    cmd %T>% message("reading the following into memory: ",.) %>% system
    blastFile = sprintf("%s/blast.tsv", outputDir)
    }else{
    blastdf = blastFile %>% read.table(sep="\t") %>% setNames(c("query","subject","perc.identity","alignment.length","mismatches","gap.openings","q.start","q.end","s.start","s.end","evalue","bitScore"))
    }
    blastdf$query %<>% as.character

    contigInfo = unique(blastdf$query) %>% as.character %>% lapply(function(contig){
    dbquery("MATCH (c:contigs{contig: {contigID}}) return c.contig, c.length, c.readnum", list(contigID = paste(koi, contig, sep=":")))
    }) %>% do.call(rbind,.)
    contigInfo$c.contig %<>% as.character %>% strsplit('\\:') %>% sapply(function(x) x[2])

    blastdf = merge(blastdf, contigInfo, by.x = "query", by.y="c.contig", all.x=T)
    #blastdf$subject %<>% as.character %>% strsplit('\\|') %>% sapply(function(x) x[2])
    blastdf$c.length %<>% as.character %>% as.integer
    blastdf$c.readnum %<>% as.character %>% as.integer

    suspicious = blastdf %>% 
        filter(
            perc.identity == 100, 
            alignment.length > c.length*0.50, 
            query %in% mdr, 
            c.readnum > dyn$thres) %>% 
        group_by(subject) %>% 
        summarise(n=n()) %>% 
        filter(n>1) %>% 
        arrange(desc(n)) %$% subject %>% as.character

    confirmRepeats = blastdf %>% 
        filter(
            perc.identity == 100, 
            alignment.length > c.length*0.50, 
            query %in% mdr, 
            c.readnum > dyn$thres, 
            subject %in% suspicious
        ) %$% query %>%
    lapply(function(coi){
        filter(readStatus, X5..Contig==coi)$Accno %>% as.character %>% strsplit('\\|') %>% sapply(function(x) x[2]) %>% table %>% data.frame %>% setNames(c("taxid", "count")) %>% mutate(total = sum(count), contig = coi) %>% mutate(perc = count/total) %>% filter(perc > 0.1)
    }) %>% do.call(rbind,.) %>% arrange(taxid)


    contig.identityVSalignment = unique(blastdf$query) %>% as.character %>% lapply(
        function(coi, threshold = 0.9){
            tryCatch(
                expr = {
                    blastresult = filter(blastdf, query == coi)
                    #did blast return a 1 to 1 match
                    isUnique = ifelse(nrow(blastresult) == 1, T, F)
                    if(isUnique){
                        chooseBest(blastresult, threshold, strict=FALSE, readStatus)
                    }else{
                        # 100% alignment + choose the longest alignment
                        blastresult = filter(blastdf, query == coi) %>% filter(perc.identity == 100) %>% arrange(desc(alignment.length)) %>% head(n=1)
                        chooseBest(blastresult, threshold, strict=FALSE, readStatus) #will fail if the above returns 0 rows
                    }
                },
                error = function(e){
                    message(sprintf("%s is probably chimeric", coi))
                }
            )
    }) %>% do.call(rbind,.) %>% tbl_df

}


edgelist = pairInfo %>% apply(1, function(row){
    coi = padding(as.integer(row["contig"]))
    rbind(splitter(row["five"], coi), splitter(row["three"], coi))
}) %>% do.call(rbind,.) %>% (function(x) x[complete.cases(x),])

edgelist %>% select(origin, connected2, nread, gap)

}
#' dynPlot Diagnostic plots after applying thresholds
#'
#' @param dyn output from dynamicThreshold
#' @param type type of threshold to use defaults to readnum, coverage is experimental
#'
#' @examples
#'\dontrun{
#' dyn = dynamicThreshold("rpk", root="~/simulation_fr_the_beginning/reAssemble/everybodyelse/data/newbler/", koi = "K00927")
#' dynPlot(dyn, type="readnum"
#'
dynPlot = function(dyn, type="readnum"){

    params = list(ko = koi, thres = dyn$thres)
    findContig = function(i){as.character(i) %>% strsplit(':') %>% unlist %>% .[[2]]}
    kodf              = dbquery("MATCH (k:ko{ko:{ko}}) return k.ko as ko, k.definition as definition, k.simulated as simulated", params)
    simulated         = dbquery("match (k:ko{ko:{ko}})-[sim:SIMULATED]-(g:genus) return g.taxid as genusID, sim.genes as ngenes", params) %>% make.data.frame
    mdrContigs        = dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 RETURN c.contig as ID", params) %$% sapply(ID, findContig)
    #round1: based on "coverage"
    thresholded       = dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 AND toFloat(c.readnum) > {thres} RETURN c.contig as ID", params) %$% sapply(ID, findContig) 

    #summary values
    totalthresholded  = length(thresholded)
    totalMDRContigs   = length(mdrContigs)
    totalSimulated = sum(as.integer(simulated$ngenes))

    {{filter(rs, X5..Contig %in% mdrContigs) %$% readOrigin} %in% simulated$genusID} %>% (function(x) sum(x)/length(x))
    {{filter(rs, X5..Contig %in% thresholded) %$% readOrigin} %in% simulated$genusID} %>% (function(x) sum(x)/length(x))


    thresholded = 
dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 AND toFloat(c.readnum) / toFloat(c.length) > 0.3 RETURN count(c) as thresholded", params = list(ko = koi, thres = threshold))

dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 AND toFloat(c.readnum) / toFloat(c.length) > 0.355 RETURN c.contig as thresholded", params = list(ko = koi, thres = threshold))



}


#' dynamicThreshold tries to identify the lower bound converage value in order to remove low quality contigs.
#' Number of contigs tend to stablize at a value when removing reads below a certain readnum and we try to identify the min read required for that region of stability
#'
#' @param mode two options either readnum or coverage; coverage is experimental, use readnum
#' @param root folder to find
#' @param koi KO of interest
#' @importFrom zoo rollapply
#' @export
dynamicThreshold <- function(mode="readnum", root, koi){
    rs = readStatusReader(root, koi, mdr=TRUE)
    if(mode == 'readnum'){
        repeatsDF = contigsSurvive.repeats.readNum(rs)
    } else if (mode == 'rpk'){
        repeatsDF = contigsSurvive.repeats.rpk(rs, koi=koi)
    }
        threshold = rolling(repeatsDF) %>%
            filter(rollingMean < 0.5) %>%
            filter(meanDepth == min(meanDepth)) %$% meanDepth
        return(list(thres = ifelse(mode=='readnum', ceiling(threshold), threshold), repeats = repeatsDF, koi = koi))
}

#' rolling chooses a dynamic threshold for the killing redundant contigs
#'
#' outputs a data.frame with 2 columns 1.) rollingMean 2.) meanDepth
#' @param keptDF dataframe for the #repeats left after thresholding, columnnames thresholds, repeats
#'
rolling <- function(keptDF, wind=15, interval = 10){
        data.frame(
            #should rename to rolling diff basically the averaged gradient change
            #in the number of repeats
            rollingMean = keptDF %$% rollapply(repeats, width=2, by=1, FUN = function(e){ abs(e[2] - e[1]) }) %>% rollapply(width = wind, by = interval, FUN = mean), 
            #the mean no. of #reads / coverage to keep in that 10 base window
            meanDepth   = keptDF %$% rollapply(threshold[-1], width = wind,  by = interval, FUN= mean)
        )
}

#' contigsSurvive.repeats.readNum
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param readDepth the minimum depth
#'
contigsSurvive.repeats.readNum <- function(rs, thresholds = seq(1, 100, 1)){
    thresholds %>% lapply(function(readDepth){
        rs %>% filter(count > readDepth) %>% group_by(readOrigin) %>% 
        summarise(repeats = n())  %>% filter(repeats > 1) %$% sum(repeats) %>% 
        data.frame(threshold = readDepth, repeats = .)
    }) %>% do.call(rbind,.)
}

#' contigsSurvive.repeats.rpk
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param rpk reads per kilo
#' @param koi the KO of interest
#'
contigsSurvive.repeats.rpk <- function(rs, rpk = seq(0, 2, 0.01), koi='K00927'){
    query = "
        MATCH
            (c:contigs{bin:{koi}})
        WHERE
            c.mdr = 1
        RETURN
            c.contig as ID,
            toFloat(c.readnum) / toFloat(c.length) as rpk
    "
    coverageDF = dbquery(query, params=list(koi = gsub("^(ko:)*", "ko:", koi)))
    coverageDF$X5..Contig = as.character(coverageDF$ID) %>% strsplit(':') %>% lapply('[[', 2) %>% do.call(c,.)
    coverageDF = merge(coverageDF, rs, by="X5..Contig",all.y=T)

    rpk %>% lapply(function(threshold) {
        coverageDF %>% filter(rpk > threshold) %>%
        #true repeats not paralogs!!!!
        group_by(readOrigin) %>% summarise(repeats = n())  %>% filter(repeats > 1) %$% data.frame(threshold, repeats = sum(repeats))
    }) %>% do.call(rbind,.)
}


#' readStatus stores the assignment details of a simulation into the graphDB
#' </rootDir/ko/454ReadStatus.txt> from newbler. 
#' We remove stray reads from other taxa if it only attributes to less than 10% of the contig
#'
#'
#' @param root the root directory
#' @param koi the ko directory
#' @importFrom data.table as.data.table
#' @export
readStatusReader <- function(root, koi, mdr=FALSE)
{
    readStatus = as.data.table(read.csv(sprintf("%s/%s/454ReadStatus.txt", root, koi), sep="\t"))
    readStatus$Accno %<>% as.character
    regexstr = "^\\d+\\|(\\d+)"
    readAlloc = readStatus %$% regmatches(Accno, regexec(regexstr,Accno)) %>% sapply(`[`,2 )
    rs = readStatus %>%
        select(Read.Status, X5..Contig) %>%
        mutate(readOrigin = readAlloc) %>%
        filter(Read.Status == 'Assembled')
    #removes readOrigins which account for 10% of the contig
    #there's contigs with same genome (readOrigin) but diff geneloc
    rs %>% group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% ungroup %>% group_by(X5..Contig) %>% mutate(total = sum(count)) %>% mutate(perc = count/total) %>% filter(perc > 0.1)
    if(mdr){
        df = dbquery("MATCH (c:contigs{bin:{ko}}) WHERE c.mdr = 1 RETURN c.contig as id", params = list(ko = gsub("^(ko:)*", "ko:", koi)))
        mdrContigs = df$id %>% as.character %>% strsplit(':') %>% lapply(function(i) i[[2]]) %>% do.call(rbind,.)
        rs %>% filter(X5..Contig %in% mdrContigs) %>%
        group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% ungroup %>% group_by(X5..Contig) %>% mutate(total = sum(count)) %>% mutate(perc = count/total) %>% filter(perc > 0.1)
    }else{
        rs
    }
}
