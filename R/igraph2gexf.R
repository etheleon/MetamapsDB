#' Converts igraph obj two gexf
#' Function for converting igraph 2 gexf
#'
#' @param mbgraph metabolic graph
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
igraph2gexf <- function(mbgraph){
    gdata     = igraph::get.data.frame(mbgraph, what="both")
    tmpedges  = gdata$edges
    tmpnodes  = gdata$vertices
    if(colnames(tmpnodes) %in% 'Definition'){
        tmpnodes$Definition %<>% sapply(function(x) gsub("[[:punct:]]", " ", x))
        write.gexf(
            nodes     = tmpnodes %>% select(name, Definition),
            edges     = tmpedges %>% select(from:to),
      nodesVizAtt = list(
              color     =   cbind(
                                V(mbgraph)$color %>% col2rgb %>% t %>% data.frame,
                                ifelse(is.null(V(mbgraph)$opacity), "100", V(mbgraph)$opacity)
                                ) %>%
                            setNames(c("R", "G", "B", "A")), #%>% head
              position  =   cbind(
                                data.frame(mbgraph$layout), 
                                0) %>%
                            setNames(c("X","Y","Z")), #%>% head
              size = V(mbgraph)$size
               )
    )
    }else{
        message("Metabolic Graph needs to include definition")
    }
}
