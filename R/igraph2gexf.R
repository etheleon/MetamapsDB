#' Converts igraph obj two gexf
#' Function for converting igraph 2 gexf
#'
#' @param mbgraph metabolic graph
#'
#' @importFrom magrittr "%>%"
#' @export
igraph2gexf <- function(mbgraph){
    gdata     = igraph::get.data.frame(mbgraph, what="both")
    tmpedges  = gdata$edges
    tmpnodes  = gdata$vertices
    if(sum(colnames(tmpnodes) %in% 'Definition')){
        tmpnodes$Definition %<>% sapply(function(x) gsub("[[:punct:]]", " ", x))
        rgexf::write.gexf(
            nodes     = tmpnodes %>% dplyr::select(name, Definition),
            edges     = tmpedges %>% dplyr::select(from:to),
            nodesVizAtt = list(
              color     =   cbind(
                                igraph::V(mbgraph)$color %>% grDevices::col2rgb %>% t %>% data.frame,
                                ifelse(is.null(igraph::V(mbgraph)$opacity), "100", igraph::V(mbgraph)$opacity)
                                ) %>%
                            setNames(c("R", "G", "B", "A")), #%>% head
              position  =   cbind(
                                data.frame(mbgraph$layout), 
                                0) %>%
                            setNames(c("X","Y","Z")), #%>% head
              size = igraph::V(mbgraph)$size
               )
    )
    }else{
        message("Metabolic Graph needs to include definition")
    }
}
