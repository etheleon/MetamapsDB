#' superglue
#'
#' Based on NEWBLER's contig graph link plot contig-graph
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
            strsplit(as.character(x), ';') %>% unlist %>% lapply(indivFunc) %>% do.call(rbind,.) %>% mutate(origin = coi)
        }else{
            NA
        }
    }
    #browser()
    fiveENDS = pairInfo %>% apply(1, function(row){
        splitter(row['five'], padding(row['contig']))
    }) %>% do.call(rbind,.)
    threeENDS = pairInfo %>% apply(1, function(row){
        splitter(row['three'], padding(row['contig']))
    }) %>% do.call(rbind,.)
    combinedEdgelist = rbind(fiveENDS, threeENDS)
    combinedEdgelist = combinedEdgelist[complete.cases(combinedEdgelist),]
    combinedEdgelist %<>% select(origin, connected2, nread, gap)
    g = graph_from_data_frame(combinedEdgelist)
    E(g)$color='grey'
    E(g)$weight= 
someval = (E(g)$nread/(E(g)$gap+1e-5))
(someval / sd(someval)) + mean(someval)

    reps = rs %>% group_by(readOrigin) %>% summarise(repeats = n()) %>% filter(repeats > 1) %$% readOrigin
    trueReps = filter(rs, readOrigin %in% reps) %>% arrange(readOrigin) %>% as.data.frame

    pairFxn = function(x){
    #browser()
        matrix(t(combn(x$X5..Contig, 2)), ncol=2) %>% as.data.frame %>% setNames(c("to", "from"))
    }
    realRepeats = filter(rs, readOrigin %in% reps) %>% arrange(readOrigin)  %>% group_by(readOrigin) %>% do(pairFxn(.))

    realRepeats %>% apply(1, function(row){
        inGraph = row['to'] %in% V(g)$name & row['from'] %in% V(g)$name
        if(inGraph){
        #browser()
        connected = are.connected(g, which(V(g)$name == row['to']), which(V(g)$name == row['from']))
        if(connected){
            get.edge.ids(g, c(which(V(g)$name == row['to']), which(V(g)$name == row['from'])))
            theEdge = E(g)[get.edge.ids(g, c(which(V(g)$name == row['to']), which(V(g)$name == row['from'])))]
            message(theEdge)
            E(g)[get.edge.ids(g, c(which(V(g)$name == row['to']), which(V(g)$name == row['from'])))]$color<<-'red'
        }
        }
    })
    pdf("contigGraph.pdf", h=10, w=10)
    plot(g, vertex.size=1, edge.width=sqrt(E(g)$gap))
    dev.off()
}

#' mappingInfo Maps contigs back onto origin gene (SIMULATION only)
#' And 
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
}

#' dynPlot Diagnostic plots
#'
#' @param dyn output from dynamicThreshold
#'
#' @examples
#' \dontrun{
#' newblerDir = "~/simulation_fr_the_beginning/reAssemble/everybodyelse/data/newbler/"
#' c("ko:K00927","ko:K02316","ko:K02520") %>%
#'     gsub("ko:", "", .) %>% #folders dont begin with ko:
#'     lapply(dynamicThreshold(root=newblerDir)) %>% 
#'     dynPlot(dynList)
#' }
dynPlots = function(dynList){
    kos = lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .)
    #readDistribution_plot = kos %>% lapply(function(scg) { contigInfo }) %>% do.call(rbind,.)
    repeatDF = mapply(function(df, oneSCG, cutoff){
        message(sprintf("Processing KO of interest: %s", oneSCG))
        contigs = contigInfo(ko=oneSCG) %>% make.data.frame %>% filter(MDR == '1')
        contigs$readnum %<>% as.character %>% as.integer
        #simulated = dbquery("match (k:ko{ko:{ko}})-[sim:SIMULATED]-(g:genus) return g.taxid as genusID, sim.genes as ngenes", params)  %>% make.data.frame
        #simulated = read.csv("~/simulation_fr_the_beginning/reAssemble/scg/script/simulatedInfo.csv")
        simNum = sum(filter(simulated, ko == gsub("^(ko:)*", "ko:", oneSCG))$Freq)

        df %<>% mutate( ko = oneSCG, cutoff = cutoff, simulated = simNum, remaining.cutoff = nrow(filter(contigs, readnum > cutoff)))
        df$remaining = sapply(df$threshold, function(x) nrow(filter(contigs, readnum > x)))

        thecutoff = cutoff
        p1 = ggplot(df) +
                geom_line(aes(threshold, repeats, group=ko)) +
                geom_vline(aes(xintercept = cutoff), color="red") +
                ggtitle(koname(oneSCG)$ko.definition) +
                ylab("#genes (redundant)")

        p2 = ggplot(df) + 
            geom_line(aes(threshold, remaining, group=ko)) + 
            geom_hline(aes(yintercept=simulated))+ 
            geom_hline(aes(yintercept = remaining.cutoff), color="red")+ 
            geom_text(aes(0, simulated, label = "Simulated", vjust = -1)) + 
            geom_text(aes(100, remaining.cutoff, label = "After thresholding", vjust = -1), color="Red") + 
            #ylim(c(0,400))+ 
            ggtitle(koname(oneSCG)$ko.definition) + ylab("#genes (total)")
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
#' @param mode two options either readnum or coverage; coverage is experimental, use readnum
#' @param root folder to find
#' @param koi KO of interest
#' @importFrom zoo rollapply
#' @export
dynamicThreshold <- function(koi, mode=c("readnum", "rpk"), thresholding=c("rle", "rolling"), root){
    message(sprintf("Processing %s", koi))
    rs = readStatusReader(root, koi, mdr=TRUE)
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
    return(list(thres = ifelse(mode=='readnum', ceiling(threshold), threshold), repeats = repeatsDF, koi = koi))
}

#' rolling A dynamic threshold for the killing redundant contigs
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


#' simpleThres
simpleThres = function(keptDF){
    #browser()
    myRLE = keptDF %>% filter(threshold <=50) %$% rollapply(repeats, width=2, by=1, FUN = function(e){ abs(e[2] - e[1]) }) %>% rle %>% .$length
    ceiling(mean(which(rep(myRLE == max(myRLE), myRLE))))
}

#' contigsSurvive.repeats.readNum
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param readDepth the minimum depth
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
