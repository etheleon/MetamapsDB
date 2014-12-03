dbquery<-structure(function(#Function for querying metamaps DB
###Takes variable queries with table output ie. no nodes no rels
query,	##<< Cypher Query, rmbr to escape double quotes if any 
params = FALSE, ##<< a list object. If no parameters, let params = False.
cypherurl = "192.168.100.1:7474/db/data/cypher", ## The address of the graph database eg. metamaps.scelse.nus.edu.sg:7474
...
){
#Checks if the params is given
if(is.list(params)){
        post = toJSON(list(query = query, params = params))
}else{
        post = toJSON(list(query = query))
}

result = fromJSON(getURL(cypherurl, customrequest = "POST", httpheader = c(`Content-Type` = "application/json"), postfields = post))
if(length(result$data)>1){
setNames(data.frame(do.call(rbind,lapply(result$data, function(x) matrix(x, nrow=1)))), result$columns)
}else{
setNames(data.frame(lapply(result$data[[1]], function(x){
if(length(x) == 0){matrix(NA, ncol=1)}else{matrix(x, ncol=1)}
})), make.names(result$columns))
}
}, ex=function(x){ 
    output.df <- dbquery(
     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
     params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
})
