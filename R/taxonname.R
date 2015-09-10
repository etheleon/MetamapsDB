#' Lists the taxonomic id's details
#'
#' @param taxon The default is Aspergillus oryzae, accepts both name as well as
#' @param name  to show the name
#' @export
taxname<-function(taxon='5062',name=FALSE,... ){
    params=list(taxonname=taxon)
    if(name)
    {
        query = "MATCH (taxa:Taxon {name:{taxonName}}) RETURN taxa.taxid AS taxID, taxa.name AS name, head(labels(taxa)) as rank"
        params = list(taxonName = taxon)
        dbquery(query=query, params=params)
    }else{
    query = "START 
                taxa=node:ncbitaxid(taxid={taxonname}) 
            RETURN 
                taxa.name, taxa.taxid, head(labels(taxa)) as rank"
    dbquery(query=query, params=params, ...)
    }
}
