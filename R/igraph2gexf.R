igraph2gexf <- structure( 
<<<<<<< HEAD
function(mbgraph  ##<<Function for converting igraph 2 gexf obj
### Converts a igraph object into gexf format
){
=======
function#Function for converting igraph 2 gexf obj
### Converts a igraph object into gexf format
         (mbgraph  ##<< the graph object
         ){
>>>>>>> refs/remotes/origin/master
    gdata     = igraph::get.data.frame(mbgraph, what="both")
    tmpedges  = gdata$edges
    tmpnodes  = gdata$vertices
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
}, ex=function(){
# 
})

