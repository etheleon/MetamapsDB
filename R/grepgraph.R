#' Returns the metabolic graph given vector of KOs
#'
#' Takes in a vector list of KO ids and generates a metabolic graph as a igraph object
#'
#' @param kos vector of kos
#' @param fullGraph to output the full metabolic graph (yet to be implemented)
#' @param ... additional dbquery parameters
#' @import igraph
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
grepgraph <- function(kos,fullGraph=FALSE,...){
if(fullGraph){

}else{
        kos = gsub("^(ko:)*","ko:",kos)
        params = list(kos = kos %>% lapply(function(x) list(ko=x)))

    query_cpd2ko = "
    UNWIND 
        { kos } AS koss
    OPTIONAL MATCH
        (ako:ko {ko : koss.ko})<--(cpd:cpd)
    RETURN 
        cpd.cpd        AS child,
        ako.ko         AS parent,
        ako.definition AS parentName,
        ako.name       AS parentSym,
        cpd.name       AS childName,
        cpd.name       AS childSym"

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
        cpd.name       AS parentName,
        cpd.name       AS parentSym,
        ako.definition AS childName,
        ako.name       AS childSym"

    df2 = dbquery(query_ko2cpd, params, ...)
    df2 %<>% make.data.frame
    df2 = df2[complete.cases(df2),]

    fulldata2 = rbind(df1, df2)

    vertex.data <- with(fulldata2,
         setNames(unique(data.frame(
            id = c(as.character(child), as.character(parent)),
            name = c(as.character(childName), as.character(parentName)),
            sym = c(as.character(childSym), as.character(parentSym))
            )), c("Vertex","Definition", "Symbol"))
         )
    g=igraph::simplify(igraph::graph.data.frame(d=unique(fulldata2[,1:2]),vertices=vertex.data))
    g$layout = igraph::layout.fruchterman.reingold(g)
    g
    }
}
