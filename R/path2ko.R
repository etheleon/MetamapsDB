#' Finds all KOs in a given pathway
#' Finds all KOs belonging to a Pathways
#'
#' @param pathway The pathway ID
#' @param ... additional paramters for dbquery
#' @importFrom magrittr "%>%"
#' @export
path2ko<-function(pathway='path:ko00010',  ...){
    params = list (pathwayID = pathway)
    query = "
    MATCH
        (ko:ko)-[:pathwayed]-(p:pathway {pathway : {pathwayID}})
    RETURN 
        ko.ko as KO"
    dbquery(query=query, params=params, ...)
}

#' ko2path Finds all pathways related to the KO
#' Finds all associated pathways, and returns a data.frame with ko and pathway details
#' 
#' @param ko the ko ID int the form eg. K00001
#' @param ... the extra params to pass to dbquery
#' @export
ko2path = function (ko = "K00001", ...)
{
    ko = gsub("^(ko:)*", "ko:", ko)
    query = "
    UNWIND
        { koname } AS KOSS
    MATCH
        (ko:ko {ko : KOSS.ko})-[:pathwayed]-(p:pathway)
    RETURN
        ko.ko, ko.name, ko.definition, p.pathway, p.pathwayname
    "
    params <- ko %>% lapply(function(x){list(ko=x)}) %>% list(koname=.)
    df = dbquery(query = query, params = params, ...)
    df
}
