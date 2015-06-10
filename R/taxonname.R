taxname<-structure(function #List Pathways
    (taxon='5062', ##<< The default is Aspergillus oryzae, accepts both name as well as 
     name=F,
    ... ){
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
    }, ex= function(){
   Asperigillus= taxname(taxon="5062");
   })
