#' Given a igraph object, adjacentPairs finds
#' adjacent pairs of KOs
#'
#' @param g graph object
#' @param ko2ko if true returns data.frame of ko pairs, if false returns the compound which its connected with
#' @param ... ellipise pass in other arguments which are required
#' @importFrom magrittr "%>%"
#' @importFrom igraph V neighborhood
#'
#' @export
#' @examples
#' # gives ko 2 ko pairs
#' adjacentPairs(nitrogenMetab, ko2ko=TRUE)
#' #        ko1       ko2 direction
#' #1 ko:K18246 ko:K01725       out
#' #2 ko:K18245 ko:K01725       out
#' #3 ko:K17877 ko:K17877       out
#' #4 ko:K17877 ko:K15876       out
#' #5 ko:K17877 ko:K15371       out
#' #6 ko:K17877 ko:K10946       out
adjacentPairs<- function (g, ko2ko =  FALSE, ...){
        kos         =  grepl("ko:", V(g)$name) %>% which

        do.call(rbind,lapply(c("out","in"), function(direction){
            kopairs     =   neighborhood(graph=g, nodes=kos, order=1, mode = direction)   #immediate pairs
            kopairs     =   kopairs[sapply(kopairs, length) != 1]
        #function to find the connected KOs given intermediate cpds
        pairDF = do.call(rbind,lapply(kopairs, function(ko){
            if(ko2ko){
                findNextKO(ko[-1], g, ko[1], direction)
            }else{
                data.frame(ko  = as_ids(ko[1]), # ko nodes
                           cpd = as_ids(ko[-1])  # Cpd nodes
                           )
            }
        #there should be one more step where repeat pairs are removed
        }))
        pairDF[
        apply(pairDF, 1, function(x) paste0(sort(x), collapse=""))                         %>%
        duplicated                                                                         %>%
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
#' @importFrom igraph neighborhood
#' @export
findNextKO = function(cpd, graph, originalKO, direction){
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
