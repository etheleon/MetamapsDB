#' Lists the taxonomic id's details
#'
#' @param taxon     The default is Aspergillus oryzae, accepts both name as well as
#' @param name      search by name
#' @param ...       additional parameters for dbquery
#' @export
taxname<-function(taxon=5062,name=FALSE,... ){
#    params=list(taxonname=as.character(taxon))  
params <- taxon %>% 
            lapply(function(x){list(name=as.character(x))}) %>% list(taxonname =.)
tryCatch({
    if(name)
    {
        query = "
        UNWIND
            { taxonname } AS taxons
        MATCH
            (taxa:Taxon {name: taxons.name})
        RETURN
            taxa.taxid      AS taxID,
            taxa.name       AS name,
            labels(taxa)    AS rank"
        
        df = dbquery(query=query, params=params)
        if(!(is.na(df))){
            dplyr::filter(df, !rank %in% 'Taxon')
        }
    }else{
        query = 
        "
        UNWIND
            { taxonname } AS taxons
        MATCH
            (taxa:Taxon {   taxid :  taxons.taxid  })
        RETURN
            taxa.name AS name,
            taxa.taxid AS taxID,
            labels(taxa) AS rank"


        df = dbquery(query=query, params=params, ...) 
        if(!(is.na(df))){
            dplyr::filter(df, !rank %in% 'Taxon')
        }
    }
    }, interrupt = function(ex) {
          cat("Interruptted\n");
      print(ex);
    }, error = function(ex) {
          cat("Error\n");
      print(ex);
    }, finally = {
        cat("Query Completed\n")
    })
}
