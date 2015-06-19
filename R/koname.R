koname<-structure(function #List Pathways
### Gives KO details when supplied with KO id                  
    (ko='K00001', 
     minimal=TRUE,##<< The default is K00001, accepts both with or without the ko: prefix
    ... ){
    if(!grepl("ko:",ko)) ko = gsub("^","ko:",ko)
    params=list(koname=ko)
    if(minimal){
        query = "START 
        ko=node:koid(ko={koname}) 
        RETURN 
        ko.ko, ko.name, ko.definition"
    }else{
    query = "START 
        ko=node:koid(ko={koname}) 
    RETURN 
        ko.ko, ko.name, ko.definition, ko.pathway, ko.`pathway.name`"
    }
    dbquery(query=query, params=params, ...)
    }, ex= function(){
   #pathwayDF = koname(ko="K00001");
   })
