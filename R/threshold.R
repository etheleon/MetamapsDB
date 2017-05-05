#' dynPlot Diagnostic plots
#'
#' @param dyn output from dynamicThreshold
#'
#' import ggplot from ggplot2
#' import grid.arrange from gridExtra
#' @examples
#' \dontrun{
#' newblerDir = "~/simulation_fr_the_beginning/reAssemble/everybodyelse/data/newbler/"
#' dynList = c("ko:K00927","ko:K02316","ko:K02520") %>%
#'     gsub("ko:", "", .) %>% #folders dont begin with ko:
#'     lapply(dynamicThreshold, root=newblerDir)
#' something = dynPlots(dynList)
#' }
dynPlots = function(dynList){
    kos = lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .)
    #readDistribution_plot = kos %>% lapply(function(scg) { contigInfo }) %>% do.call(rbind,.)
    repeatDF = mapply(function(df, oneSCG, cutoff){
        message(sprintf("Processing KO of interest: %s", oneSCG))
        contigs = contigInfo(ko=oneSCG) %>% make.data.frame %>% filter(MDR == '1')
        contigs$readnum %<>% as.character %>% as.integer
        simulated = dbquery("match (k:ko{ko:{ko}})-[sim:SIMULATED]-(g:genus) return k.ko as ko,g.taxid as taxid, sim.genes as Freq", list(ko=gsub("^(ko:)*", "ko:", oneSCG))) %>% make.data.frame
        simulated$Freq %<>% as.character %>% as.integer
        #simulated = read.csv("~/simulation_fr_the_beginning/reAssemble/scg/script/simulatedInfo.csv")
        simNum = sum(filter(simulated, ko == gsub("^(ko:)*", "ko:", oneSCG))$Freq)
        #browser()
        df %<>% mutate( ko = oneSCG, cutoff = cutoff, simulated = simNum, remaining.cutoff = nrow(filter(contigs, readnum > cutoff)))
        df$remaining = sapply(df$threshold, function(x) nrow(filter(contigs, readnum > x)))

        thecutoff = cutoff
        p1 = ggplot(df)                                           +
                geom_line(aes(threshold, repeats, group=ko))      +
                geom_vline(aes(xintercept = cutoff), color="red") +
                ggtitle(koname(oneSCG)$ko.definition)             +
                ylab("#genes (redundant)")

        p2 = ggplot(df)                                                                                  +
            geom_line(aes(threshold, remaining, group=ko))                                               +
            geom_hline(aes(yintercept=simulated))                                                        +
            geom_hline(aes(yintercept = remaining.cutoff), color="red")                                  +
            geom_text(aes(0, simulated, label = "Simulated", vjust = -1))                                +
            geom_text(aes(100, remaining.cutoff, label = "After thresholding", vjust = -1), color="Red") +
            #ylim(c(0,400))                                                                              +
            ggtitle(koname(oneSCG)$ko.definition)                                                        + 
            ylab("#genes (total)")
        combined = grid.arrange(p1,p2)
        print(combined)
        df
    },
        df        =  lapply(dynList, function(x) x$repeats),# %>% .[3],# %>% head(n=2),
        oneSCG    =  lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .),# %>% .[3],#%>% head(n=2),
        cutoff =  lapply(dynList, function(x) x$thres),
        SIMPLIFY  =  FALSE
    ) %>% do.call(rbind,.)
    #Summary
    errorTable.after = repeatDF %>% select(ko, simulated, remaining.cutoff) %>% unique %>% mutate(diff = abs(remaining.cutoff - simulated))
    mae = sum(errorTable.after$diff)/nrow(errorTable.after)
    message(sprintf("Mean Absolute Error (MAE): %s", mae))
    list(repeatDF, mae)
}

#' dynamicThreshold tries to identify the lower bound converage value in order to remove low quality contigs.
#' Number of contigs tend to stablize at a value when removing reads below a certain readnum and we try to identify the min read required for that region of stability
#'
#' @param koi KO of interest
#' @param mode two options either readnum or coverage; coverage is experimental, use readnum
#' @param thresholding which type of thresholding method to use
#' @param root folder to find
#' @importFrom zoo rollapply
#' @export
dynamicThreshold <- function(koi, mode=c("readnum", "rpk"), thresholding=c("rle", "rolling"), root){
    message(sprintf("Processing %s", koi))
    rs = readStatusReader(root, koi, mdr=TRUE)
    #browser()
    if(mode == 'readnum'){
        repeatsDF = contigsSurvive.repeats.readNum(rs)
    } else if (mode == 'rpk'){
        repeatsDF = contigsSurvive.repeats.rpk(rs, koi=koi)
    }
    if(thresholding=='rolling'){
        threshold = rolling(repeatsDF) %>%
            filter(rollingMean < 0.5) %>%
            filter(meanDepth == min(meanDepth)) %$% meanDepth
    } else if (thresholding == 'rle'){
        threshold = simpleThres(repeatsDF)
    }
    thres = if(mode=='readnum'){ceiling(threshold)}else{threshold}
    return(list(thres = thres, repeats = repeatsDF, koi = koi))
}

#' rolling A dynamic threshold for the killing redundant contigs
#'
#' outputs a data.frame with 2 columns 1.) rollingMean 2.) meanDepth
#' @param keptDF dataframe for the #repeats left after thresholding, columnnames thresholds, repeats
#' @param wind window size
#' @param interval interval size
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


#' simpleThres
#'
#' internal cluster
# 
#' @param keptDF internal function
simpleThres = function(keptDF){
    #browser()
    myRLE = keptDF %>% filter(threshold <=50) %$% rollapply(repeats, width=2, by=1, FUN = function(e){ abs(e[2] - e[1]) }) %>% rle %>% .$length
    ceiling(mean(which(rep(myRLE == max(myRLE), myRLE))))
}

#' contigsSurvive.repeats.readNum
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param thresholds the threshold window
#'
contigsSurvive.repeats.readNum <- function(rs, thresholds = seq(1, 50, 1)){
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


#' readStatus stores the assignment details of each read from the simulation and assigns the contig a genome of origin
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
        df = contigInfo(ko=koi) %>% make.data.frame %>% filter(MDR == '1')
        mdrContigs = df$contig %>% as.character %>% strsplit(':') %>% lapply(function(i) i[[2]]) %>% do.call(rbind,.)
        rs %>% filter(X5..Contig %in% mdrContigs) %>%
        group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% ungroup %>% group_by(X5..Contig) %>% mutate(total = sum(count)) %>% mutate(perc = count/total) %>% filter(perc > 0.1)
    }else{
        rs
    }
}
