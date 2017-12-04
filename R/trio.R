#' Find all trios surrounding the KO of interest
#'
#' Searches graphDB for all cases of trios surrounding the KO of interest
#'
#' @param KOI the ko id (for remote)
#' @param koi the ko id (for local)
#' @param toUnique  if to return only unique (only applicable for remote)
#' @param withDetails with details (only applicable for remote)
#' @param local for doing the search locally, faster
#' @param contracted to contract anot (only applicable for local)
#' @param ... the other args for dbquery
#' @importFrom magrittr "%>%" "%<>%" "%$%"
#' @importFrom dplyr anti_join filter select arrange group_by
#' @importFrom igraph as_ids
#' @importFrom utils combn
#' @importFrom stats complete.cases
#' @importFrom igraph shortest_paths "%->%"
#'
#' @export
allTrios <- function(
    KOI         = 'ko:K00001',
    koi         = 'ko:K00001',
    toUnique    = TRUE,
    withDetails = FALSE,
    local = TRUE,
    contracted = TRUE,
    ...
){
    . = 'shutup'
    if(local){
##################################################
        #If you have the full metabolic graph
        #surrNODES('ko:K00401', wholeMetabolism) %>% sapply(surrNODES, graph=wholeMetabolism) %>% do.call(c,.) %>% unique %>% grepgraph
        oriG = grepgraph(koi)
        if(vcount(oriG) == 0){
            message(sprintf("%s not found", koi))
        }else{
            oriG2 = grepgraph_cpd(V(oriG)$name %>% grep("cpd", ., value=T))
            meta = grepgraph(V(oriG2)$name %>% grep("ko", ., value=T))
            if(contracted){
                message("contracting graph")
                meta = contractMetab(meta)
            }

        koID = findV(koi, meta)
        surrCPDs    = surrNODES(koi, meta)
        surrCPD_id  =  sapply(surrCPDs, findV, g=meta)
        surrKOs     = surrNODES(koi, meta) %>% lapply(surrNODES, g = meta) %>% do.call(c,.) %>% unique
        surrKOs %<>% grep(koi, ., value=T, invert=T) #remove itself
        surrKOs_id <- sapply(surrKOs, findV, g=meta)

        #Shortest Paths which passes through the KO of Interest
        spaths = surrKOs %>% grep(koi, ., invert=T, value=T) %>%
                 combn(2) %>%
                 apply(2, function(pair){
                    alist = list(
                       one = findV(pair[[1]], meta),
                       two = findV(pair[[2]], meta)
                       )
                    lapply(shortest_paths(meta, from = alist$one, to = alist$two)$vpath,function(x){
                        x = as.integer(x)
                    #there may be multiple paths
                    isInside = koID %in% x
                    if(isInside){
                        matrix(V(meta)$name[x] %>% do.call(c,.), nrow=1)
                    }else{
                        NULL
                    }
                   })
        }) %>% do.call(c,.)
        if(is.null(do.call(c,spaths))){
            sprintf("%s is not on any shortest path", koi) %>% message
        }else{
            sprintf("%s is on the shortest path", koi) %>% message
            #shortestPathsDF
            spaths = setNames(as.data.frame(do.call(rbind,spaths)), c("Kminus", "Cminus", "K", "Cplus", "Kplus"))
        }

        #Find ALL trios
        allTrios= lapply(surrKOs_id,
            function(ko1){
                #ko1 -a-> cpd1
                igraph::E(meta)[ko1 %->% surrCPD_id] %>% extractFromPath %>%
                #ko1 -a-> cpd1 -b-> koi
                lapply(function(cpd1){
                       isC2K = igraph::E(meta)[cpd1 %->% koID] %>% as_ids %>% length > 0 #valid
                       if(isC2K){
                #ko1 -> cpd1 -> koi -> cpd2
                           igraph::E(meta)[koID %->% surrCPD_id] %>% extractFromPath %>%
                            #ko1 -> cpd1 -> koi -> cpd2 -> ko2
                           lapply(function(cpd2){
                                    isC2K2 = igraph::E(meta)[cpd2 %->% surrKOs_id] %>% length  > 0
                                    if(isC2K2){
                                    ko2 = igraph::E(meta)[cpd2 %->% surrKOs_id] %>% extractFromPath(type="ko")
                                    setNames(data.frame(ko1, cpd1, koID, cpd2, ko2), c("Kminus","Cminus","K", "Cplus","Kplus"))
                                  }else{
                                      message("Failed")
                                  }
                                }) %>% do.call(rbind,.)
                       }else{
                        message("Failed")
                       }
                       }) %>% do.call(rbind,.)
            }) %>% do.call(rbind,.)
        Kminus = NULL
        Kplus = NULL
        Cminus= NULL
        Cplus= NULL
        allTrios = filter(allTrios, Kminus != Kplus & Cminus != Cplus)
        allTrios = apply(allTrios,1, function(x) matrix(unlist(V(meta)[x]$name), nrow=1)) %>% t %>% as.data.frame %>% setNames(colnames(allTrios))

        notshortest = anti_join(allTrios,spaths, by=colnames(allTrios))
        notshortest$type = "notshortest"
        spaths$type = "shortest"

        list(paths = rbind(notshortest, spaths), graph=meta)
        }
##################################################
    }else{
    KOI = gsub("^(ko:)*","ko:",KOI)
    #Round 1: Find the paths
    trioDF <- dbquery(query = "
        MATCH
            (ko1:ko)-->(:cpd)-->(inputko:ko {ko:{koid}})-->(:cpd)-->(ko3:ko)
        RETURN
            ko1.ko     AS before,
            inputko.ko AS middle,
            ko3.ko     AS after
    ", list(koid = KOI) ,,...)
    if(!is.na(trioDF)){
        trioDF$before %<>% as.character
        trioDF$middle %<>% as.character
        trioDF$after  %<>% as.character

        # not uturn type reactions (redundant KOs)
        before=NULL
        after=NULL
        trioDF = unique(filter(trioDF,before != after))
        if(toUnique){
            #remove reverse rxns
            trioDF = trioDF[!duplicated(
                        trioDF                             %>%
                        apply(1, function(row){
                            c(row["before"], row["after"]) %>%
                            sort                           %>%
                            paste0(collapse="")
                        })
            ),]
            #remove redundant
        }

    #Round 2: Find the contigs information
    if(withDetails){
        kos = with(trioDF, c(before, after,middle) %>% unique)
        contigInfo <- dbquery(
        query = "
            UNWIND
                { kos } AS koss
            MATCH
                (inputko:ko {ko : koss.ko})
            RETURN
                inputko.ko          AS koID,
                inputko.contigCount AS ContigCount,
                inputko.expression  AS Expression
        ",
        params = lapply(kos,function(x) list(ko=x)) %>% list(kos=.), ...) %>% make.data.frame
        contigInfo <- contigInfo[complete.cases(contigInfo),]
        list(trioDF, contigInfo)
    }else{
        trioDF
    }
    }else{
    NA
    }
}
}
