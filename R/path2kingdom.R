#' List all intermediaries between taxa and the superkingdom it belongs to 
#'
#' Finds the superkingdom given the taxid
#' @param taxID NCBI taxonomic id
#' @param ... additional dbquery paramaters
#' @export
path2kingdom<-function(taxID = '79255', ...){
query = "
MATCH
    path = (:Taxon { taxid: {taxID}   })-[:childof*]->(king:superkingdom)
RETURN
    extract(n in nodes(path)| n.name) AS name,
    extract(n in nodes(path)| n.taxid) AS taxid,
    extract(n in nodes(path)| head(labels(n))) AS rank 
"
params = list(taxID = taxID)
dbquery(query=query, params = params, ...)
}


