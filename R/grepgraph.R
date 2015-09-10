#' Returns the metabolic graph given vector of KOs
#'
#' Takes in a vector list of KO ids and generates a metabolic graph as a igraph object
#'
#' @param kos vector of kos
#' @param fullGraph to output the full metabolic graph (yet to be implemented)
#'
#' @export
grepgraph <- function(kos,fullGraph=FALSE,...){
if(fullGraph){

}else{
        kos = gsub("^(ko:)*","ko:",kos)
        params = kos %>% lapply(function(x) list(ko=x)) %>% list(kos=.)

    query_cpd2ko = "
    UNWIND 
        { kos } AS koss
    OPTIONAL MATCH
        (ako:ko {ko : koss.ko})<--(cpd:cpd)
    RETURN 
        cpd.cpd        AS child,
        ako.ko         AS parent,
        cpd.name       AS childName,
        ako.definition AS parentName"

    df1 = dbquery(query_cpd2ko, params, ...)
    df1 %<>% make.data.frame
    df1 = df1[complete.cases(df1),]

    query_ko2cpd = "
    UNWIND
        { kos } AS koss
    OPTIONAL MATCH
        (ako:ko {ko : koss.ko})-->(cpd:cpd)
    RETURN 
        ako.ko         AS child,
        cpd.cpd        AS parent,
        ako.definition AS childName,
        cpd.name       AS parentName"

    df2 = dbquery(query_ko2cpd, params, ...)
    df2 %<>% make.data.frame
    df2 = df2[complete.cases(df2),]

    fulldata2 = rbind(df1, df2)

    vertex.data <- with(fulldata2,
         setNames(unique(data.frame(
            id = c(as.character(child), as.character(parent)),
            name = c(as.character(childName), as.character(parentName))
            )), c("Vertex","Definition"))
         )
    g=simplify(graph.data.frame(d=unique(fulldata2[,1:2]),vertices=vertex.data))
    g$layout = layout.fruchterman.reingold(g)
    g
    }
}
