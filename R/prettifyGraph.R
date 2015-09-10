#' returns metabolic graph with all the ornaments set
#' Adds details into the igraph object
#' @param g igraph Object
#' @param vsize the vertex size
#' @param vcolor length 2 vector for KO and compoundsprettifyGraph<- function
#' @param withText Conditional to include labels in plot(g,                          
#' @param vtext two vector text valuevsize    = c(1,2),           
#' @param layout Whether to calculate the layoutvcolor   = c("grey", "red"), 
#'
#' @export
prettifyGraph<- function
(g,                          
vsize    = c(1,2),           
vcolor   = c("grey", "red"), 
withText = TRUE,             
vtext    = c(0.5, 1),        
layout   = FALSE,            
...
){
    if(layout)
        g$layout = layout.fruchterman.reingold(g)
    #Size
    V(g)$size = rep(vsize[1], vcount(g))
    V(g)$size[grep("ko", V(g)$name)] = vsize[2]

    #Color
    V(g)$color=rep(vcolor[1], vcount(g))
    V(g)$color[grep("ko", V(g)$name)] = vcolor[2]

    #Label
    if(withText){
        V(g)$label.cex=rep(vtext[1], vcount(g))
        V(g)$label.cex[grep("ko", V(g)$name)] = vtext[2]
        V(g)$label = V(g)$Definition
    }

    V(g)$frame.color="#FFFFFF00"    #Colorless
    E(g)$arrow.size = 0.1
    g
}
