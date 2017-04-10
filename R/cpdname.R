#' Finds the details of the CPD when given its ID
#' @param cpd the compound ID with or without the prefix is accepted either as a single or vector accepts with or without prefix
#' @param ... allows for additional arguments use for dbquery
#' @importFrom magrittr "%>%"
#' @export
cpdname<- function (cpd='C00022', ... ){
    cpd = gsub("^(cpd:)*","cpd:",cpd)
    params = cpd %>% lapply(function(x) list(cpdname=x)) %>% list(cpds=.)
    query = "
    UNWIND
        { cpds } AS eachcpd
    OPTIONAL MATCH
        (thecpd:cpd {cpd : eachcpd.cpdname})
    RETURN
        thecpd.cpd as cpd,
        thecpd.name as name,
        thecpd.exactMass as mass,
        thecpd.molWeight as weight"
    df = dbquery(query=query, params=params, ...)
    df
}
