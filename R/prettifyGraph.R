#' returns metabolic graph with all the ornaments set
#' Adds details into the igraph object
#' @param g igraph Object
#' @param vsize the vertex size
#' @param vcolor length 2 vector for KO and compounds
#' @param withText Conditional to include labels in
#' @param vtext two vector text valuevsize
#' @param layout Whether to calculate the layoutvcolor

#' @importFrom igraph vcount layout.fruchterman.reingold
#' @export
#' @examples
#' p = nitrogenMetab %>% prettifyGraph %>% ig2ggplot(., dfOnly=FALSE)
#' p
prettifyGraph<- function
(g,
vsize    = c(1,2),
vcolor   = c("grey", "red"),
withText = TRUE,
vtext    = c(0.5, 1),
layout   = FALSE
){
    if(layout | nrow(g$layout) != vcount(g))
        g$layout = layout.fruchterman.reingold(g)
    #Size
    igraph::V(g)$size = rep(vsize[1], vcount(g))
    igraph::V(g)$size[grep("ko", igraph::V(g)$name)] = vsize[2]

    #Color
    igraph::V(g)$color=rep(vcolor[1], vcount(g))
    igraph::V(g)$color[grep("ko", igraph::V(g)$name)] = vcolor[2]

    #Label
    if(withText){
        igraph::V(g)$label.cex                        = rep(vtext[1], vcount(g))
        igraph::V(g)$label.cex[grep("ko", igraph::V(g)$name)] = vtext[2]
        igraph::V(g)$label                            = igraph::V(g)$Definition
    }

    igraph::V(g)$frame.color="#FFFFFF00"    #Colorless
    igraph::E(g)$arrow.size = 0.1
    g
}
