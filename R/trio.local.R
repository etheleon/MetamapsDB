#' trio.local
#'
#' `trio.local` finds all trios surrounding a given KO
#'
#' @param koi              the KO of interest
#' @param contracted        conditional for contracting and simplifying graph ie. merging subunits
#' @return data.frame containing all reactions
#' @importFrom magrittr "%>%" "%$%"
#' @examples
#' /dontrun{
#'
#' }
trio.local <- function(koi, contracted=FALSE){

    #If you have the full metabolic graph
    #surrNODES('ko:K00401', wholeMetabolism) %>% sapply(surrNODES, graph=wholeMetabolism) %>% do.call(c,.) %>% unique %>% grepgraph

    oriG = grepgraph(koi)
    if(vcount(oriG) == 0){
        message(sprintf("%s not found", koi))
    }else{
        oriG2 = grepgraph_cpd(V(oriG)$name %>% grep("cpd", ., value=T))
        meta = grepgraph(
                         V(oriG2)$name %>% grep("ko", ., value=T)
                         )

        if(contracted){
            message("contracting graph")
            meta = contractMetab(meta)
        }

    koID = findV(koi, meta)

        surrCPDs    = surrNODES(koi, meta)
        surrCPD_id <- sapply(surrCPDs, findV, g=meta)

        surrKOs     = surrNODES(koi, meta) %>% lapply(surrNODES, g = meta) %>% do.call(c,.) %>% unique

    #if(contracted){
        #keyDF = do.call(rbind,
        #lapply(surrKOs, function(x) {
        #data.frame(name=x, components=unlist(stringr::str_extract_all(x, pattern="(ko:K\\d{5})")))
        #}))
    #}

        surrKOs %<>% grep(koi, ., value=T, invert=T) #remove itself
        surrKOs_id <- sapply(surrKOs, findV, g=meta)

    #Shortest Paths which passes through the KO of Interest
    spaths = surrKOs %>% grep(koi, ., invert=T, value=T) %>% 
             combn(2) %>%
             apply(2, function(pair){
                list(
                   one = findV(pair[[1]], meta),
                   two = findV(pair[[2]], meta)
                   ) %$%
                shortest_paths(meta, from = one, to = two) %$%
                lapply(vpath ,function(x){
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
            E(meta)[ko1 %->% surrCPD_id] %>% extractFromPath %>%
            #ko1 -a-> cpd1 -b-> koi
            lapply(function(cpd1){
                   isC2K = E(meta)[cpd1 %->% koID] %>% as_ids %>% length > 0 #valid
                   if(isC2K){
            #ko1 -> cpd1 -> koi -> cpd2
                       E(meta)[koID %->% surrCPD_id] %>% extractFromPath %>%
                        #ko1 -> cpd1 -> koi -> cpd2 -> ko2
                       lapply(function(cpd2){
                                isC2K2 = E(meta)[cpd2 %->% surrKOs_id] %>% length  > 0
                                if(isC2K2){
                                ko2 = E(meta)[cpd2 %->% surrKOs_id] %>% extractFromPath(type="ko")
                                data.frame(
                                     Kminus = ko1,
                                     Cminus = cpd1,
                                     K      = koID,
                                     Cplus  = cpd2,
                                     Kplus = ko2
                                     )
                              }else{
                                  message("Failed")
                              }
                            }) %>% do.call(rbind,.)
                   }else{
                    message("Failed")
                   }
                   }) %>% do.call(rbind,.)
        }) %>% do.call(rbind,.)
    allTrios %<>% filter(Kminus != Kplus & Cminus != Cplus)
    allTrios = apply(allTrios,1, function(x) matrix(unlist(V(meta)[x]$name), nrow=1)) %>% t %>% as.data.frame %>% setNames(colnames(allTrios))

    notshortest = anti_join(allTrios,spaths, by=colnames(allTrios))
    notshortest$type = "notshortest"
    spaths$type = "shortest"

    list(paths = rbind(notshortest, spaths), graph=meta)
    }
}



#' extractFromPath
#'
#' given a set of edgelists extract out the CPD / KO node ids
#'
#' @param edges the edgelist
#' @param type the type of nodes to extract
#' @importFrom magrittr "%>%"
#' @keywords internal
#' @export
extractFromPath <- function(edges, type='cpd') {
    if(type == 'cpd'){
        edges %>% as_ids %>% stringr::str_extract_all("(cpd:C\\d{5})") %>% sapply(findV, g=meta)
    }else{
        edges %>% as_ids %>% gsub("cpd:C\\d+\\|", "", .) %>% sapply(findV, g=meta)
    }
}


#' findV finds the vertixID in the graph given its name
#'
#' given the original graph find the vertiex ID of given name
#
#' @param name name of the vertex
#' @param g igraph object
#'
#' @export
#' @keywords internal
findV = function(name, g){
    which(sapply(V(g)$name, function(x) sum(grepl(name, x))) > 0)
}

#' surrNODES finds nodes which are surrounding the given node 
#'
#' @param noi       node of interest; the compound/ko ID not the vertex ID of the node in the graph
#' @param graph     igraph object
#' @param all       conditional to return all nodes regardless of direction
#' @importFrom magrittr "%>%"
#'
#' @return vector of surrounding nodes if all is TRUE, if all is FALSE returns list of in and out nodes
surrNODES = function(noi, graph, all=TRUE){
    pat = ifelse(grepl("ko:", noi), "ko:", "cpd:")
    koi = findV(noi, graph)

    inNODE = neighborhood(graph,
                          mode  = "in",
                          order = 1,
                          nodes = koi) %>% do.call(c,.) %>% unique %>% names %>%
    grep(pat, . , invert=T, value=T)

    outNODE = neighborhood(graph,
                           mode = "out",
                           order=1,
                           nodes=koi) %>% do.call(c,.) %>% unique %>% names %>%
    grep(pat, . , invert=T, value=T)

    if(all==FALSE){
        list(inn =inNODE, outt=outNODE)
    }else{
        unique(c(outNODE, inNODE))
    }
}
