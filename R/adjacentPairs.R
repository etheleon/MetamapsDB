#' Given a igraph object, adjacentPairs finds 
#' adjacent pairs of KOs 
#'
#' @param g graph object
#' @param ko2ko kos to find adjacent pairs for
#'
#' @importFrom magrittr "%>%"
#'
#' @export
adjacentPairs<- function (g, ko2ko =  FALSE, ...){
        kos         =  grepl("ko:", V(g)$name) %>% which
        #TODO:  non-ko2ko direction is still not configured
        do.call(rbind,lapply(c("out","in"), function(direction){
            kopairs     =   neighborhood(graph=g, nodes=kos, order=1, mode = direction)   #immediate pairs
            kopairs     =   kopairs[sapply(kopairs, length) != 1]

        #function to find the connected KOs given intermediate cpds
        pairDF = do.call(rbind,lapply(kopairs, function(ko){
            if(ko2ko){
                findNextKO(ko[-1], g, ko[1])
            }else{
                data.frame(ko  = as_ids(ko[1]), # ko nodes
                           cpd = as_ids(ko[-1])  # Cpd nodes
                           )
            }
        #there should be one more step where repeat pairs are removed
        }))
        pairDF[
        apply(pairDF, 1, function(x) paste0(sort(x), collapse=""))                         %>%
        duplicated                                                                                %>%
        ifelse(FALSE, TRUE),]                                           # this inverses the list
        }))
}

#' FindNextKO 
#'
#' Given a specied KO, one is suppose to find the next adjacent KO.
#'
#' @param cpd the intermediary cpd
#' @param graph metabolic graph
#' @param originalKO KO of interest
#' @param direction the direction in which to find
#'
findNextKO = function(cpd, graph, originalKO){
    connectedKO = neighborhood(graph = graph, nodes = cpd, order = 1, mode=direction)
    #removes nodes without outgoing/incoming connections
    connectedKO = connectedKO[sapply(connectedKO, length) != 1]

    do.call(rbind,lapply(connectedKO,function(cpd){
                         data.frame(
                                    ko1       = as_ids(originalKO),
                                    ko2       = as_ids(cpd[-1]),
                                    direction = direction
                                    )
        }))
}

