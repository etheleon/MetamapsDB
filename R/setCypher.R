#' Settings for connecting to neo4j database
#' Function sets parameters such as the REST API URL, username and passwords
#' In the newer versions of neo4j default requires username and password
#' 
#'
#' @param url url to graphDB
#' @param port port
#' @param username username
#' @param password password
#' @param test to show connection success or failure
#'
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
#' @export
connect <- function(
url = "192.168.100.1", ##<< URL hosting the neo4j database
port = 7474,            ##<< Port
username = 'neo4j',   ##<< authorization username and password
password = 'neo4j', ##<< authorization username and password
test = TRUE
){
    assign("cacheEnv",new.env(), envir = baseenv())
    cacheEnv$cypher   <- paste0(url, ":", port, "/db/data/cypher")
    cacheEnv$user     <- username
    cacheEnv$password <- password
    if(test){
        message("Sending test query: Searching for K00001")
        connected = suppressMessages(koname('K00001')) %>% pull(ko.name) == 'E1.1.1.1, adh'
        ifelse(connected, "Connection Successful ✅", "Connection Unsuccessful ❌") %>% message
    }
}
