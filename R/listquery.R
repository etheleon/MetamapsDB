#' Function for querying metamaps DB
#'
#' Takes variable queries with table output ie. no nodes no rels (simpler version of DBquery gives a list)
#'
#' @param query
#' @param params string containing cypher query
#' @param cypherurl address of database, inclusive of ports
#' @param user database username
#' @param password database password
#'
#' @export
listquery<-function(query,params = FALSE, cypherurl = cacheEnv$cypher, user = cacheEnv$user, password = cacheEnv$password,...){
    #Checks if the params is given
    if(is.list(params)){
        post = RJSONIO::toJSON(list(query = query, params = params))
    }else{
        post = RJSONIO::toJSON(list(query = query))
    }

    result = RJSONIO::fromJSON(getURL(cypherurl, customrequest = "POST", httpheader = c(`Content-Type` = "application/json"), postfields = post))
}
