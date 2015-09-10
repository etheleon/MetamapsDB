#' Finds all KOs in a given pathway
#' Finds all KOs belonging to a Pathways
#'
#' @param pathway The pathway ID
#'
#' @export
path2ko<-function(pathway='path:ko00010',  ...){
    params = list (pathwayID = pathway)
    query = "
    START
        p=node:pathwayid(pathway={pathwayID})
    MATCH 
        (ko:ko)-[:pathwayed]-(p) 
    RETURN 
        ko.ko as KO"
    dbquery(query=query, params=params, ...)
}
