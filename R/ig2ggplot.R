#' Convert igraph to ggplot2 object
#'
#' Converts a metabolic igraph obj into a ggplot plot
#'
#' @param g igraph object
#' @param dfOnly outputs the data.frame and not draw using ggvis 
#' @param labels to show labels or not
#' @param ... additional dbquery parameters
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot
#' @export
ig2ggplot <- function(g, dfOnly = TRUE, labels=FALSE, metab = TRUE, ...){
        layoutDF    = setNames(data.frame(layout.norm(g$layout, xmax=1, xmin=0, ymin=0, ymax=1)), c("x", "y"))
        vertexDF    = data.frame(id = unlist(V(g)$name), name = unlist(V(g)$label))
        edgeDF      = get.edgelist(g)
        edgeDF = as.matrix(data.frame(edgeDF[,1] %>% unlist, edgeDF[,2] %>% unlist))
        edgeDF = edgeDF[!duplicated(1:nrow(edgeDF) %>% lapply(function(x) paste(sort(edgeDF[x,]), collapse="")) %>% do.call(c,.)),]
        vertexDF2   = 1:nrow(edgeDF) %>% lapply(function(row){
                        v1 = edgeDF[row,1]
                        v2 = edgeDF[row,2]
                        before = data.frame(layoutDF[which(V(g)$name == v1),], row    =  row, name =  v1, label  =  subset(vertexDF, id == v1)$name)
                        after = data.frame(layoutDF[which(V(g)$name == v2),], row    =  row, name =  v2, label  =  subset(vertexDF, id == v2)$name)
                     rbind(before, after)
        }) %>% do.call(rbind,.)

        vertexDF2$id = 1:nrow(vertexDF2)
        if(metab){
            vertexDF2$type = do.call(c,lapply(strsplit(x=as.character(vertexDF2$name), split=":"), function(x) x[[1]]))
        }else{
            vertexDF2$type = 1
            vertexDF2$type = as.factor(vertexDF2$type)
        }
        edgelists = 1:nrow(edgeDF) %>% lapply(function(row) {
            rbind(
            vertexDF2 %>% dplyr::select(x,y,name) %>% unique %>% dplyr::filter(name == edgeDF[row,1]) %>% dplyr::select(x,y) %>% dplyr::mutate(group=row),
            vertexDF2 %>% dplyr::select(x,y,name) %>% unique %>% dplyr::filter(name == edgeDF[row,2]) %>% dplyr::select(x,y) %>% dplyr::mutate(group=row)
            )
        }) %>% do.call(rbind,.)
        if(dfOnly){
            list(vertexDF2, edgelists)
        }else{
            p = ggplot(vertexDF2, aes(x, y))                                                                     +
                geom_line(data=edgelists, aes(x,y, group=group), color="grey", alpha=0.5)                         +
                geom_point(data=vertexDF2, aes(color=type, size=type, text=label)) +
                theme_void()
            if(labels){
            p + geom_text(aes(label=label))
            }else{
            p
            }
        }
}
