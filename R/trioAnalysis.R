#' findTrios searches valid three-KO reactions resticting by the center KO and reports reaction clusters
#'
#'
#' findTrios uses K-means clustering to identify reaction groups/clusters and the GAP statistic by Ryan Tibshirani to identity the best k implemented in the library cluster
#' Clustering algorithm uses the input matrix cmoposed of the the KS statistics of the KOs flanking the KO of interest. 
#'
#' The KS statistics is calculated based on the comparison of each KO's gene/contig expression distribution against the 'null' ie. 
#' empirical distribution against all genes in all contigs
#'
#' In addition it also identifies reactions where flanking KOs have high gene diversity, given by low KS (<= 0.5)
#'
#'
#' @param KOI KOs-of-interest ie KOs above a selected amount of expression ie. highly expressed KOs and with D-statistics above that of the desired threshold
#' @param ks  Precalculated data.frame output from ksCal fxn containing all KO's gene distribution  KS statistic when compared with whole sample's empirical gene distribution
#' @param toPrint conditional to print the results of the classification plots
#' @param outputFile the txt file to write the results of findTrios to
#' @param plotDir the location to save the diagnostics plots to
#' @return data.frame of all reactions, corresponding clusters and the KS statistics for each KO and the selected Cluster
#'
#' @importFrom magrittr "%>%" "%<>%" "%$%"
#' @importFrom dplyr arrange filter group_by select n
#' @importFrom tidyr spread
#' @importFrom grDevices colorRampPalette dev.off pdf
#' @importFrom stats median
#' @export
findTrios <- function(KOI, ks, toPrint = TRUE, outputFile, plotDir){
    .='shutup'
    cluster = NULL
    trio = NULL
    rxntype = NULL
    ko = NULL
    rxnNum = NULL
    d = NULL
    k = NULL
    before.median = NULL
    after.median = NULL
    value = NULL
   file.remove(outputFile) 
    lapply(KOI, function(midKO){
        trioDF = trio(midKO)
        if(is.na(trioDF)){
               message(sprintf("%s is not a metabolic KO", midKO))
            data.frame(
                    rxnNum    =  integer(),
                    before.x  =  numeric(),
                    middle.x  =  numeric(),
                    after.x   =  numeric(),
                    cluster   =  integer(),
                    before.y  =  character(),
                    middle.y  =  character(),
                    after.y   =  character(),
                    selected  =  integer()
                       )
        }else{
            trioDF %<>% mutate(rxnNum = 1:n())
            lineLong = trioDF %>% tidyr::gather(rxntype, ko, -rxnNum) %>% merge(ks, all.x   = T)
            #removes rxns where the KOs have no expression
            NArxns = lineLong[lineLong %>% apply(1, function(x) is.na(x) %>% sum ) > 0 ,]$rxnNum
            lineLong = filter(lineLong, !rxnNum %in% NArxns)
            #input matrix of KS values of the before and after KOs for to cluster the reactions.
            m        =  lineLong                                %>%
                        select(rxnNum, rxntype, d)       %>%
                        spread(key = rxntype, value = d)
            clusteredM = findK(theMatrix = m, ko=midKO)


            if(toPrint){
                pdf(sprintf("%s/%s.pdf", plotDir, midKO))
                clusteredM %$% plotClassification(matrix, ko = midKO) %>% print
                dev.off()
            }

            within.ksDF <-   1:clusteredM$k %>% lapply(function(cl){
                    df = filter(clusteredM$matrix, cluster == cl)
                    data.frame(
                       k             = df$cl,
                       value         = ks.test(df$before, df$after, alternative = "two.sided")$statistic,
                       before.median = median(df$before),
                       after.median  = median(df$after)
                       )
            })  %>%
            do.call(rbind,.) %>% arrange(k)

            #arbitary threshold
            selectedCluster = filter(within.ksDF, before.median <= 0.5, after.median <= 0.5, value == min(value))

            if(nrow(selectedCluster) > 1 ){
                sprintf("%s had more than 1 cluster meeting the criteria %s", midKO, nrow(selectedCluster)) %>% message
                finalM  = cbind(m , 
                data.frame(
                    before.ks = NA,
                    middle.ks = NA,
                    after.ks  = NA,
                    cluster   = NA,
                    selected  = NA
                    ))
                write.table(finalM, file=outputFile, append=T, row.names=F, quote=F)
                finalM
            }else if(nrow(selectedCluster) == 0){
                sprintf("%s had no clusters %s", midKO, nrow(selectedCluster)) %>% message
                finalM  = cbind(m , 
                data.frame(
                    before.ks = NA,
                    middle.ks = NA,
                    after.ks  = NA,
                    cluster   = NA,
                    selected  = NA
                    ))
                write.table(finalM, file=outputFile, append=TRUE, row.names=F, quote=F)
                finalM
            }else{
                finalM = clusteredM$matrix %>% merge(trioDF, by="rxnNum")
                finalM$selected = selectedCluster$k
                finalM %<>% setNames(
                    c("rxnNum",
                      "before.ks",
                      "middle.ks",
                      "after.ks",
                      "cluster",
                      "before.ko",
                      "middle.ko",
                      "after.ko", 
                      "selected"
                      )
                )
                write.table(finalM, file=outputFile, append=TRUE, row.names=F, quote=F)
                finalM
        }
    }
})
}

#' findK to find the optimum number of Ks
#'
#' using cluster::clusGap
#'
#' @param theMatrix    two column data.frame with KS values of the before and after KOs in the reactions
#' @param kmax         the max number of Ks to test for; defaults to 10
#' @param ko           the ko of interest
#'
#' @importFrom magrittr "%>%" "%<>%" "%$%"
#' @importFrom stats kmeans
#' @keywords internal
#' @return optimum number of Ks to choose
findK <- function(theMatrix, kmax = 10, ko){
    gap = NULL
    before = NULL
    after = NULL
    Tab = NULL
    SE.sim = NULL
    cluster = NULL

        clusTab = theMatrix %>% dplyr::select(before, after) 
        n = nrow(clusTab)
        if(n < kmax){
            message(sprintf("%s has less than 10 reactions running through", ko))
            kmax = n - 1
        }
        clusTab %<>%
                    cluster::clusGap(kmeans, B=100, K.max=kmax, iter.max=1000,nstart=100) %$%
                    Tab %>% as.data.frame

        winning = clusTab %$% gap[gap[-kmax] > (gap[-1] - SE.sim[-1])] %>% min
        optiK = which(clusTab$gap == winning)
        theMatrix$cluster = theMatrix %>% select(before, after) %>% 
        kmeans(centers = optiK, iter.max=1000,nstart=100) %$% cluster
        list(k = optiK, matrix = theMatrix, num = n)
}


#' Plot clustering
#'
#' clusters the graph
#'
#' @param matrix    input matrix for plotting needs before and after column
#' @param ko        the koID for printing the KO name and ID to the diagnostic plot
#' @keywords internal
plotClassification = function(matrix, ko){
    . = 'shutup'
    middle = NULL
    rxnNum = NULL
    rxntype = NULL
    ks = NULL
    cluster = NULL
    tm =
    matrix                               %>%
    select(-middle, -rxnNum)             %>%
    tidyr::gather(rxntype, ks, -cluster)

    #check rows
    checkDF = unique(tm$cluster) %>%
    lapply(function(c){
           data.frame(cluster  =  c,
                      rxns     =  nrow(filter(tm, cluster == c))
               )
    }) %>% do.call(rbind,.)

    p1 = tm %>% ggplot(aes(ks, group=rxntype, color=rxntype))    +
        geom_density()                                           +
        ggtitle(sprintf("%s - %s",ko, koname(ko)$ko.definition))

    if(sum(checkDF$rxns < 3)){
        p1
    }else{
    p1 + facet_wrap(~cluster)
    }
}

#' ksCal generates KS statistics for aKO given the base distribution
#'
#' runs a "two.sided" KS test and reports the KS and p value
#'
#'
#' @param contigDF          data.frame with count frequency and rpkm of ALL KOs' contigs for both gDNA and cDNA obtained from query
#' @param baseDistribution  the 
#' @param cores             number of cores to use
#'
#' @return data.frame with columns: ko, p.value and KS statistic (D)
#' @importFrom stats ks.test
#' @export
#' @keywords internal
ksCal <- function(contigDF, baseDistribution, cores){
    . = 'shutup'
    ko = NULL
    group1 = baseDistribution$rpkm_cDNA %>% as.numeric

    contigDF %$%
    ko       %>%
    unique   %>%
    parallel::mclapply(
         function(koid){
             group2  =  filter(contigDF, ko == koid)$rpkm_cDNA %>% as.numeric
             ks      =  ks.test(group1, group2, alternative="two.sided")
             data.frame(ko   =  koid,
                    p.value  =  ks$p.value,
                    d        =  ks$statistic)
    },mc.cores = cores) %>%
    do.call(rbind,.)
}
