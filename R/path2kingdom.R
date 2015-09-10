#' List all intermediaries between taxa and the superkingdom it belongs to 
#'
#' Finds the superkingdom given the taxid
#' @param taxID NCBI taxonomic id
#' @export
path2kingdom<-function(taxID = '79255', ...){
query = "
START
    basetaxa=node:ncbitaxid(taxid={taxID})
MATCH
    path = basetaxa-[:childof*]->(king:superkingdom)
RETURN
    extract(n in nodes(path)| n.name) AS name,
    extract(n in nodes(path)| n.taxid) AS taxid,
    extract(n in nodes(path)| head(labels(n))) AS rank 
"
params = list(taxID = taxID)
listquery(query=query, params = params, ...)
}
