connect <- structure(function #Settings for connecting to neo4j database
### Function sets parameters such as the REST API URL, username and passwords
(
url = "192.168.100.1", ##<< URL hosting the neo4j database
port = 7474            ##<< Port
){
### NULL
    cacheEnv$cypher = paste0(url, ":", port, "/db/data/cypher")
}, ex = function(){
    connect("localhost", 7474)
})
