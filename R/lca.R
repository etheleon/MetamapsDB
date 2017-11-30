#' Finds the lowest common ancestor
#'
#' @param taxon1 First taxon NCBI tax id
#' @param taxon2 Second taxon NCBI tax id
#' @param recurse to keep moving up to find the LCA
#' @param ... additional dbquery parameters
#' @export
lca <- function(   taxon1='10090', taxon2='9096',  recurse=TRUE,   ... ){
    params = list(taxonOne= as.character(taxon1), taxonTwo = as.character(taxon2))

    query = "
    MATCH 
        (:Taxon {taxid:{taxonOne}})-[:childof*]->(pretax1)-[:childof]->(common)<-[:childof]-(pretax2)<-[:childof*]-(:Taxon {taxid:{taxonTwo}})
    RETURN
        common.name    AS name,
        common.taxid   AS taxid,
        labels(common) AS rank"

    result = listquery(query=query, params=params, ...)

    if(length(result$data)>0){
        df = dbquery(query=query, params=params, ...)
        if(recurse){return(df$taxid)}else{return(df)}
    }else{
        #Find out who's the head
        query = "
        OPTIONAL MATCH
            (tax1:Taxon {taxid: {taxonOne}})-[:childof*]->(tax2:Taxon {taxid: {taxonTwo}})
        RETURN 
            count(p) as taxOneChildOfTwo,
            tax1.name,
            tax1.taxid,
            labels(tax1) as rank1,
            tax2.name,
            tax2.taxid,
            labels(tax2) as rank2"

        altResult = dbquery(query=query, params=params, ...)
        if(altResult$taxOneChildOfTwo>0){
            df = data.frame(name = altResult$tax2.name, taxid = altResult$tax2.taxid, rank = altResult$rank2)
            if(recurse){return(df$taxid)}else{return(df)}
        }else{
            df = data.frame(name = altResult$tax1.name, taxid = altResult$tax1.taxid, rank = altResult$rank1)
            if(recurse){return(df$taxid)}else{return(df)}
        }
    }
}

lca2 <- function(){
query = "
MATCH
    (t1:Taxon {taxid: '51875'})-[:childof*]->(p1:Taxon)
MATCH
    (:Taxon {taxid: '37579'})-[:childof*]->(p2:Taxon)
WHERE
    p1.taxid = p2.taxid
MATCH
    path = (t1)-[:childof*]->(p1)
RETURN p1.taxid
ORDER BY length(path)
LIMIT 1
"
dbquery(query)
}

