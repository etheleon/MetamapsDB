#' List Pathways
#' Lists all metabolic pathways
#' @param ... additional parameters for dbquery
#' @export
pathways<-function(...){
setNames(
         dbquery(query='match (pathway:pathway) return pathway.pathwayname, pathway.pathway', ...), 
         c("pathwayName", "pathwayID")
         )
}
