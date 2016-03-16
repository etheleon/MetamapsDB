#' List all intermediaries between taxa and the superkingdom it belongs to 
#'
#' Finds the superkingdom given the taxid
#' @param taxID NCBI taxonomic id
#' @param ... additional dbquery paramaters
#' @export
path2kingdom<-function(taxID = '79255', ...){
    query = "
    OPTIONAL MATCH 
    (nn:Taxon { taxid: {taxID}   })
    OPTIONAL MATCH 
    p = (nn)-[:childof*]->(king:superkingdom)
    RETURN
    CASE WHEN p is null THEN nn.name    ELSE extract(n in nodes(p) | n.name)    END AS name,
    CASE WHEN p is null THEN nn.taxid   ELSE extract(n in nodes(p) | n.taxid)   END AS taxid,
    CASE WHEN p is null THEN filter(x IN labels(nn) where x <> 'Taxon' ) ELSE extract(n in nodes(p) | filter(x IN labels(n) where x <> 'Taxon' )) END AS rank
    "

    params = list(taxID = as.character(taxID))
    df = dbquery(query=query, params=params, ...) 
    df
}


