lca<-structure(function #Finds the lowest common ancestor
    (taxon1='10090', ##<< The first taxon
    taxon2='9096', ##<< The second taxon
    ... ){
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
        return(df = dbquery(query=query, params=params))
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
            altResult = dbquery(query=query, params=params)
            if(altResult$taxOneChildOfTwo>0){
                return(data.frame(name = altResult$tax2.name, taxid = altResult$tax2.taxid, rank = altResult$rank2))
            }else{
                return(data.frame(name = altResult$tax1.name, taxid = altResult$tax1.taxid, rank = altResult$rank1))
            }
        }
    }, ex= function(){
        df = lca(taxon1="10090", taxon2='9096');
    })
