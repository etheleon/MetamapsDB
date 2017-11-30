#' Returns the metabolic graph given vector of KOs
#'
#' Takes in a vector list of cpd ids and generates a subset of metabolic graph as a igraph object
#'
#' @param cpds vector of cpds
#' @param fullGraph to output the full metabolic graph (yet to be implemented)
#' @param ... additional dbquery parameters
#' @importFrom igraph V simplify graph.data.frame layout.fruchterman.reingold
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
grepgraph.cpd <-function(cpds, fullGraph=FALSE, ...){
    . = 'shutup'
if(fullGraph){

}else{
    cpds = ifelse(grepl("cpd:", cpds), cpds, gsub("^", "cpd:",cpds))
    params = cpds %>% lapply(function(x) list(cpd=x)) %>% list(cpds=.)

    query_cpd2ko = "
    UNWIND 
        { cpds } AS cpdss
    OPTIONAL MATCH
        (acpd:cpd {cpd : cpdss.cpd})<--(ko:ko)
    RETURN 
        ko.ko           AS child,
        acpd.cpd AS parent,
        ko.definition AS childName,
        acpd.name AS parentName"

    df1 = dbquery(query_cpd2ko, params, ...)
    df1 %<>% make.data.frame
    df1 = df1[complete.cases(df1),]

    query_ko2cpd = "
    UNWIND
        { cpds } AS cpdss
    OPTIONAL MATCH
        (acpd:cpd {cpd : cpdss.cpd})-->(ko:ko)
    RETURN 
        acpd.cpd AS child,
        ko.ko AS parent,
        acpd.name AS childName,
        ko.definition AS parentName"

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
