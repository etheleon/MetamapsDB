igraph2gexf <- structure( ##Function for converting igraph 2 gexf obj
function(mbgraph,  #igraph Obj
         opacity=1 #Opacity of the nodes
         ){
write.gexf(
      gdata = get.data.frame(mbgraph, what="both")
      tmpedges <- gdata$edges
      tmpnodes <- gdata$vertices
      nodes =   tmpnodes %>% select(name, Definition),
      edges =   tmpedges %>% select(from:to),
      nodesVizAtt = list(
              color     =   cbind(
                                V(mbgraph)$color %>% col2rgb %>% t %>% data.frame,
                                opacity) %>%
                            setNames(c("R", "G", "B", "A")), #%>% head

              position  =   cbind(
                                data.frame(mbgraph$layout), 
                                opacity) %>%
                            setNames(c("X","Y","Z")) #%>% head
               )
    )
}, ex=function(){
...
})

