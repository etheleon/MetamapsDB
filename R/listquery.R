listquery<-structure(function(#Function for querying metamaps DBj
###Takes variable queries with table output ie. no nodes no rels (simpler version of DBquery gives a list)
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
}, ex=function(x){ 
    output.df <- dbquery(
     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
     params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
})
