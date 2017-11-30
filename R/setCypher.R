#' Connecting to NEO4J Graph Database.
#'
#' `connect` sets params such as the REST API URL, port, db username and password,
#' In the newer versions of neo4j default requires username and password. If this 
#' is the first time connecting to a new neo4jDB, please index the nodes else the queries
#' will be very slow.
#'
#' @param url url to running instance of neo4j
#' @param port port
#' @param username username
#' @param password password
#' @param test run test to show connection success or failure
#'
#' @importFrom dplyr pull bind_rows
#' @importFrom magrittr %>%
#' @importFrom RCurl getURL
#' @importFrom purrr map
#' @importFrom RJSONIO fromJSON
#' @importFrom base64enc base64encode
#' @export
#' @examples
#' \dontrun{
#' connect(url="127.0.0.1", port=7474, username="neo4j", password="neo4j")
#'
#' If this is your first time connecting to omics DB please index the nodes, using `index`
#' else query speed will be very slow.
#'
#' Sending test query: Searching for K00001
#' #A successful connection will give the following
#' Connection Successful
#'
#' Found these indices:
#'   property_keys   label
#' 1        contig contigs
#' 2           bin contigs
#' 3           cpd     cpd
#' 4            ko      ko
#' 5         taxid   Taxon
#' }
connect <- function(
    url = "127.0.0.1", ##<< URL hosting the neo4j database
    port = 7474,            ##<< Port
    username = 'neo4j',   ##<< authorization username and password
    password = 'neo4j', ##<< authorization username and password
    test = TRUE
){
	ko.name = NULL
    assign("cacheEnv",new.env(), envir = baseenv())
    cacheEnv$cypher   <- paste0(url, ":", port, "/db/data/cypher")
    cacheEnv$user     <- username
    cacheEnv$password <- password
    if(test){
        message("Sending test query: Searching for K00001")
        connected = suppressMessages(koname('K00001')) %>% pull(ko.name) == 'E1.1.1.1, adh'
        ifelse(connected, "Connection Successful", "Connection Unsuccessful") %>% message
        #Check for indices
        if(connected){
            key         = base64encode(charToRaw(paste(username, password, sep=":")))
            indexURL    = paste0(url, ":", port, "/db/data/schema/index/")
            header      = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key))
            indices     = getURL(indexURL, customrequest = "GET", httpheader = header)
            if(indices == ""){
                warning("No indices found; Database is not indexed")
                yn = readline(prompt="Do you want to index now?")
                if(yn == 'y'){
                    mapply(index, label = c("ko", "contigs", "contigs", "Taxon", "cpd"), property = c("ko", "contig", "bin", "taxid", "cpd"))
                }else if (yn == 'n'){
                    message("OK its your choice, the queries will be slower")
                }else{
                    message("I do not recognise the command")
                }
            }else{
                message("Found these indices:")
                fromJSON(indices) %>% map(as.data.frame) %>% bind_rows
            }
        }
    }
}


#' Indexes the database for faster retrieval
#'
#' `index` indexes nodes based on the given property. Lets you do fast search 
#' for a node using a property field.
#'
#' @param label label of the nodes you want to index
#' @param property the property key on which you would like to index on
#' @param cacheEnv stores details
#' @export
#' @examples
#' \dontrun{
#' f = future({
#'     dbquery(query="CREATE INDEX ON :contigs(contig)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :contigs(bin)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :Taxon(taxid)", justPost = TRUE)
#'     dbquery(query="CREATE INDEX ON :cpd(cpd)", justPost = TRUE)
#'     message
#' }) %plan% multiprocess
#' }
index <- function(label, property="contig", cacheEnv){
    post = RJSONIO::toJSON(list(property_keys= property))
    key         = base64encode(charToRaw(paste(cacheEnv$user, cacheEnv$password, sep=":")))
    header      = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key))
    addindex = paste(gsub("cypher", "schema/index", cacheEnv$cypher), label, sep="/")
    getURL(addindex, customrequest = "POST", httpheader = header, postfields = post) %>% fromJSON
}
