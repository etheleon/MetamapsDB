#' Settings for connecting to neo4j database
#' Function sets parameters such as the REST API URL, username and passwords
#' In the newer versions of neo4j default requires username and password
#' 
#'
#' @param url url to graphDB
#' @param port port
#' @param username username
#' @param password password
#'
#' @export
connect <- function(
url = "192.168.100.1", ##<< URL hosting the neo4j database
port = 7474,            ##<< Port
username = 'neo4j',   ##<< authorization username and password
password = 'neo4j' ##<< authorization username and password
){
    assign("cacheEnv",new.env(), env = baseenv())
    cacheEnv$cypher   <- paste0(url, ":", port, "/db/data/cypher")
    cacheEnv$user     <- username
    cacheEnv$password <- password
}
