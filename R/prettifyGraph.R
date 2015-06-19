prettifyGraph<-structure(
function    #Plots a standard metabolic graph
### Adds details into the igraph object
(g,                          ##<< GraphObject
vsize    = c(1,2),           ##<< the vertex size
vcolor   = c("grey", "red"), ##<< length 2 vector for KO and compounds
withText = TRUE,             ##<< Conditional to include labels in plot
vtext    = c(0.5, 1),        ##<< two vector text value
layout   = FALSE,            ##<< Whether to calculate the layout
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
}, ex=function(x){
    #data(top500kos)
    #g= grepgraph(top500kos)
    #g = prettifyGraph(g)
    })
