#' Finds the lowest common ancestor
#'
#' @param taxon1 First taxon NCBI tax id
#' @param taxon2 Second taxon NCBI tax id
#' @param recurse to keep moving up to find the LCA
#'
#' @export
lca <- function(   taxon1='10090', taxon2='9096',  recurse=TRUE,   ... ){
    params = list(taxonOne= as.character(taxon1), taxonTwo = as.character(taxon2))

    query = "
    START 
    tax1=node:ncbitaxid(taxid={taxonOne}),
    tax2=node:ncbitaxid(taxid={taxonTwo})
    MATCH 
    p = (tax1)-[:childof*]->(pretax1)-[:childof]->(common)<-[:childof]-(pretax2)<-[:childof*]-(tax2)
    RETURN 
    common.name AS name, common.taxid AS taxid, head(labels(common)) as rank"

    result = listquery(query=query, params=params)

    if(length(result$data)>0){
        df = dbquery(query=query, params=params, ...)
        if(recurse){return(df$taxid)}else{return(df)}
    }else{
        #Find out who's the head
        query = "
        START
        tax1=node:ncbitaxid(taxid={taxonOne}),
        tax2=node:ncbitaxid(taxid={taxonTwo})
        OPTIONAL MATCH
        p = (tax1)-[:childof*]->(tax2)
        RETURN 
        count(p) as taxOneChildOfTwo, 
        tax1.name, tax1.taxid, head(labels(tax1)) as rank1,
        tax2.name, tax2.taxid, head(labels(tax2)) as rank2"
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
