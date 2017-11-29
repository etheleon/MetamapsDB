#' List all intermediaries between taxa and the superkingdom it belongs to 
#'
#' Finds the superkingdom given the taxid
#' @param taxID NCBI taxonomic id
#' @param ... additional dbquery paramaters
#' @examples
#' \dontrun{
#' path2kingdom(taxidID='79255')
#' Sending query
#'
#' Query Completed
#'
#'               name   taxid         rank
#' 1         Gordonia   79255        genus
#' 2         Theaceae   27065       family
#' 3         Ericales   41945        order
#' 4         asterids   71274     subclass
#' 5     Pentapetalae 1437201      no rank
#' 6       Gunneridae   91827      no rank
#' 7   eudicotyledons   71240      no rank
#' 8  Mesangiospermae 1437183      no rank
#' 9    Magnoliophyta    3398      no rank
#' 10   Spermatophyta   58024      no rank
#' 11   Euphyllophyta   78536      no rank
#' 12    Tracheophyta   58023      no rank
#' 13     Embryophyta    3193      no rank
#' 14  Streptophytina  131221      no rank
#' 15    Streptophyta   35493       phylum
#' 16   Viridiplantae   33090      kingdom
#' 17       Eukaryota    2759 superkingdom
#'
#' }
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
    CASE WHEN p is null THEN filter(x IN labels(nn) where not x in ['Taxon', 'simTaxa'] ) ELSE extract(n in nodes(p) | filter(x IN labels(n) where not x in ['Taxon','simTaxa'] )) END AS rank
    "

    params = list(taxID = as.character(taxID))
    df = dbquery(query=query, params=params, ...) 
    df
}


