connect <- structure(function #Settings for connecting to neo4j database
### Function sets parameters such as the REST API URL, username and passwords
### In the newer versions of neo4j default requires username and password
(
url = "192.168.100.1", ##<< URL hosting the neo4j database
port = 7474,            ##<< Port
username = 'neo4j',   ##<< authorization username and password
password = 'neo4j' ##<< authorization username and password
){
### NULL
    cacheEnv$cypher   <- paste0(url, ":", port, "/db/data/cypher")
    cacheEnv$user     <- username
    cacheEnv$password <- password
}, ex = function(){
    connect("localhost", 7474, 'username', 'password')
})
