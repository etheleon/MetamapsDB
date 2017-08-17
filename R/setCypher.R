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
#' @importFrom dplyr pull bind_rows
#' @importFrom magrittr %>%
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @importFrom base64enc base64encode
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
        #Check for indices
        if(connected){
            key         = base64encode(charToRaw(paste(user, password, sep=":")))
            indexURL    = paste0(url, ":", port, "/db/data/schema/index/")
            header      = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key))
            indices     = getURL(indexURL, customrequest = "GET", httpheader = header)
            if(indices == ""){
                warning("No indices found; Database is not indexed")
                yn = readline(prompt="Do you want to index now?")
                if(yn == 'y'){
                    mapply(Metamaps::index, label = c("ko", "contigs", "contigs", "Taxon", "cpd"), property = c("ko", "contig", "bin", "taxid", "cpd"))
                }else if (yn == 'n'){
                    message("OK its your choice, the queries will be slower")
                }else{
                    message("I do not recognise the command")
                }
            }else{
                message("Found these indices:")
                fromJSON(indices) %>% purrr::map(as.data.frame) %>% bind_rows
            }
        }
    }
}


#' Indexes the database for faster retrieval
#'
#' indexes nodes based on the property
#'
#' @export
#' @example
#' \dontrun{
#' f = future({
#'     dbquery(query="CREATE INDEX ON :contigs(contig)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :contigs(bin)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :Taxon(taxid)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :cpd(cpd)", justPost = TRUE)
#'     message
#' }) %plan% multiprocess
#' }
index <- function(label, property){
    post = RJSONIO::toJSON(list(property_keys= "contig"))
    key         = base64encode(charToRaw(paste(cacheEnv$user, cacheEnv$password, sep=":")))
    header      = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key))
    addindex = paste(gsub("cypher", "schema/index", cacheEnv$cypher), label, sep="/")
    getURL(addindex, customrequest = "POST", httpheader = header, postfields = post) %>% fromJSON
}
