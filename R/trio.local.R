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
    .='shutup'
    meta = NULL
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
#' @importFrom igraph neighborhood
#' @return vector of surrounding nodes if all is TRUE, if all is FALSE returns list of in and out nodes
surrNODES = function(noi, graph, all=TRUE){
    .='shutup'
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
