koname<-structure(function #List Pathways
    (ko='K00001', ##<< The default is K00001, accepts both with or without the ko: prefix
    ... ){
    if(!grepl("ko:",ko)) ko = gsub("^","ko:",ko)
    params=list(koname=ko)
    query = "START 
        ko=node:koid(ko={koname}) 
    RETURN 
        ko.name, ko.definition, ko.pathway, ko.`pathway.name`"
    dbquery(query=query, params=params)
    }, ex= function(){
   pathwayDF = koname(ko="K00001");
   })
