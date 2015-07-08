koname<-structure(function #List Pathways
### Gives KO details when supplied with KO id
    (ko='K00001', ##<< the koid
     minimal=TRUE,##<< The default is K00001, accepts both with or without the ko: prefix
    ...
    ){
        ko = gsub("^(ko:)*","ko:",ko)
        query = "
        UNWIND
            { koname } AS KOSS
        MATCH
            (ko:ko {ko : koss.ko})
        RETURN 
            ko.ko, ko.name, ko.definition, ko.pathway, ko.`pathway.name`
        "
        params <- ko %>% lapply(function(x){list(ko=x)}) %>% list(koname=.)
        df     <- dbquery(query=query, params=params, ...)

        if(minimal){
            df %>% select(ko.ko:ko.definition)
        }else{
            df
        }
    }, ex= function(){
   #pathwayDF = koname(ko="K00001");
   })
