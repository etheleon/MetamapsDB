cpdname<-structure(function #List Pathways
    (cpd='C00022',
     minimal=TRUE,##<< The default is K00001, accepts both with or without the ko: prefix
    ... ){
    if(!grepl("cpd:",cpd)) cpd = gsub("^","cpd:",cpd)
    params=list(cpdname=cpd)
        query = "START
        cpd=node:cpdid(cpd={cpdname}) 
        RETURN 
        cpd.cpd, cpd.name"
    dbquery(query=query, params=params, ...)
    }, ex= function(){
   cpdname(cpd="C00022");
   })
