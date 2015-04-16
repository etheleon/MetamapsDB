ig2ggvis<-structure(function #Convert igraph to ggvis object
    (g, #graph object
    ...
    ){
        layoutDF    = setNames(data.frame(layout.norm(g$layout, xmax=1, xmin=0, ymin=0, ymax=1)), c("x", "y"))
        vertexDF    = data.frame(id = V(g)$name, name = V(g)$label)
        edgeDF      = get.edgelist(g)
        vertexDF2   = do.call(rbind,lapply(1:nrow(edgeDF), function(row){
                     v1 = edgeDF[row,][[1]]
                     v2 = edgeDF[row,][[2]]
                     rbind(
                            data.frame(layoutDF[which(V(g)$name == v1),],
                                      row    =  row,
                                      name =  v1,
                                      label  =  subset(vertexDF, id == v1)$name
                                      ),
                            data.frame(layoutDF[which(V(g)$name == v2),],
                                      row    =  row,
                                      name =  v2,
                                      label  =  subset(vertexDF, id == v2)$name
                                      )
                           )
        }))
        #graphDF = cbind(layoutDF, vertexDF)
        vertexDF2$id = 1:nrow(vertexDF2)
        vertexDF2 %>% 
            ggvis(~x, ~y, key:= ~id)                                                                                                                                                %>%
            layer_points()                                                                                                                                                          %>%
            group_by(row)                                                                                                                                                           %>%
            layer_paths()                                                                                                                                                           %>%
            add_axis("x", title = "", properties = axis_props(axis = list(strokeWidth = 0), grid = list(strokeWidth = 0),ticks = list(strokeWidth = 0), labels = list(fontSize=0))) %>%
            add_axis("y", title = "", properties = axis_props(axis = list(strokeWidth = 0), grid = list(strokeWidth = 0),ticks = list(strokeWidth = 0), labels = list(fontSize=0))) %>%
            add_tooltip(function(data){paste0("ID: ", data$name, "<br>", "label: ", vertexDF2[vertexDF2$id == data$id, ]$label   )})
    }, ex= function(){
        ...
    })
