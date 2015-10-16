#' Convert igraph to ggvis object
#'
#' Converts a igraph obj into a ggvis plot
#' @param g igraph object
#' @param dfOnly outputs the data.frame and not draw using ggvis 
#' @param ... additional dbquery parameters
#' @export
ig2ggvis <- function(g, dfOnly = TRUE, ...){
        layoutDF    = setNames(data.frame(layout.norm(g$layout, xmax=1, xmin=0, ymin=0, ymax=1)), c("x", "y"))
        vertexDF    = data.frame(id = V(g)$name, name = V(g)$label)
        edgeDF      = get.edgelist(g)
        edgeDF = edgeDF[!duplicated(do.call(c,lapply(1:nrow(edgeDF), function(x) paste(sort(edgeDF[x,]), collapse="")))),]
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
        vertexDF2$id = 1:nrow(vertexDF2)
        vertexDF2$type = do.call(c,lapply(strsplit(x=as.character(vertexDF2$name), split=":"), function(x) x[[1]]))
        if(dfOnly){
            vertexDF2
        }else{
            all_values <- function(x) {
                if(is.null(x)) return(NULL)
                paste0(names(x), ": ", format(x), collapse = "<br />")
            }
            vertexDF2                                                                                                                                                                    %>%
                ggvis(~x, ~y)                                                                                                                                                           %>%
                group_by(row)                                                                                                                                                           %>%
                layer_paths()                                                                                                                                                           %>%
                layer_points(size= ~type, fill=~type)                                                                                                                                   %>%
                add_axis("x", title = "", properties = axis_props(axis = list(strokeWidth = 0), grid = list(strokeWidth = 0),ticks = list(strokeWidth = 0), labels = list(fontSize=0))) %>%
                add_axis("y", title = "", properties = axis_props(axis = list(strokeWidth = 0), grid = list(strokeWidth = 0),ticks = list(strokeWidth = 0), labels = list(fontSize=0))) %>%
                scale_ordinal("size", range=c(20,100))                                                                                                                                  %>%
                scale_ordinal("fill", range=c("grey","red"))                                                                                                                            %>%
                add_tooltip(all_values, "hover")
        }
    }
