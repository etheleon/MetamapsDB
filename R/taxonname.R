#' Lists the taxonomic id's details
#'
#' @param taxon The default is Aspergillus oryzae, accepts both name as well as
#' @param name  search by name
#' @param ... additional parameters for dbquery
#' @export
taxname<-function(taxon=5062,name=FALSE,... ){
    params=list(taxonname=as.character(taxon))
    if(name)
    {
        query = "MATCH (taxa:Taxon {name:{taxonName}}) RETURN taxa.taxid AS taxID, taxa.name AS name, labels(taxa) as rank"
        params = list(taxonName = taxon)
        dbquery(query=query, params=params) %>% dplyr::filter(!rank %in% 'Taxon')
    }else{
    query = "MATCH 
                (taxa:Taxon {taxid:{taxonname}})
            RETURN 
                taxa.name,
                taxa.taxid,
                labels(taxa) as rank"
    dbquery(query=query, params=params, ...) %>% dplyr::filter(!rank %in% 'Taxon')
    }
}
