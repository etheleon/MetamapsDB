cpdname<-structure(function #List Pathways
### Finds the details of the CPD when given its ID
    (cpd='C00022',##<< the cpd ID; either as a single or vector accepts with or without prefix
     minimal=TRUE,##<< The default is K00001, accepts both with or without the ko: prefix
    ... ){
    if(!grepl("cpd:",cpd)) cpd = gsub("^","cpd:",cpd)

    #params=list(cpdname=cpd)
    params = cpd %>% lapply(function(x) list(cpdname=x)) %>% list(cpds=.)
    query = "
    UNWIND
        { cpds } AS eachcpd
    OPTIONAL MATCH
        (thecpd:cpd {cpd : eachcpd.cpdname})
    RETURN
        thecpd.cpd, thecpd.name"
    dbquery(query=query, params=params, ...)
    }, ex= function(){
   #cpdname(cpd="C00022");
   })
