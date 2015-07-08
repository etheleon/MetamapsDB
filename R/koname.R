koname<-structure(function #List Pathways
### Gives KO details when supplied with KO id
    (ko='K00001', ##<< the koid
     minimal=TRUE,##<< The default is K00001, accepts both with or without the ko: prefix
    ...
    ){
        ko = gsub("^(ko:)*","ko:",ko)
        if(!minimal){
            query = "
            UNWIND
                { koname } AS KOSS
            MATCH
                (ko:ko {ko : KOSS.ko})
            RETURN 
                ko.ko, ko.name, ko.definition, ko.pathway, ko.`pathway.name`
            "
        }else{
            query = "
            UNWIND
                { koname } AS KOSS
            MATCH
                (ko:ko {ko : KOSS.ko})
            RETURN 
                ko.ko, ko.name, ko.definition
            "
        }

        params <- ko %>% lapply(function(x){list(ko=x)}) %>% list(koname=.)
        df     <- dbquery(query=query, params=params, ...)
                         # cypherurl= "http://metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
        df
    }
    , ex= function(){
   #pathwayDF = koname(ko="K00001");
   })
