#' Function for querying metamaps DB
#'
#' Takes variable queries with table output ie. no nodes no rels
#' @param query char string with cypher Query, rmbr to escape double quotes if any; read LUCENE
#' @param params a list object. ie params required by a cypher query, false if query requires no parameters; default
#' @param cypherurl address of database, inclusive of ports
#' @param user database username
#' @param password database password
#' @param justPost its just a POST request
#' @param verbose to print details out
#' @param test for testing, function takes a given json response in the form of a character string provided in argument jsonresponse
#' @param jsonresponse string equivalent to the json response from querying the database
#' @param ... allows for additional arguments to be passed into dbquery
#'
#' @export
dbquery <- function(query,
                    params = FALSE,
                    cypherurl =NULL,
                    user = NULL,
                    password = NULL,
                    justPost=FALSE,
                    verbose=FALSE,
                    test = FALSE,
                    jsonresponse = '',
                    ...){
    if(test){
        result = RJSONIO::fromJSON(jsonresponse)
        if(length(result$data)>=2){
            setNames(data.frame(do.call(rbind,lapply(result$data, function(x) matrix(x, nrow=1)))), result$columns)
        }else if(length(result$data)==1){
            setNames(data.frame(lapply(result$data[[1]], function(x){
                                       if(length(x) == 0){
                                           matrix(NA, ncol=1)
                                       }else{
                                           matrix(x, ncol=1)
                                       }
            })), make.names(result$columns))
        }else{
            return(NA)
        }
    }else{
    if(is.null(cypherurl)){
        cypherurl  =  as.environment("package:base")$cacheEnv$cypher
        user       =  as.environment("package:base")$cacheEnv$user
        password   =  as.environment("package:base")$cacheEnv$password
    }
    #Checks if the params is given
    tryCatch({
        message("Sending query\n")
    if(is.list(params)){
        post = RJSONIO::toJSON(list(query = query, params = params))
        message("POST:")
        message(post)
    }else{
        post = RJSONIO::toJSON(list(query = query))
        message("POST:")
        message(post)
    }

    key <- base64enc::base64encode(charToRaw(paste(user, password, sep=":")))

    if(justPost){
        results = RCurl::getURL(
                 cypherurl,
                 customrequest = "POST",
httpheader = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key)),
                 postfields = post,
                 .opts = list(verbose = verbose)
                 )
        message("RESULT:")
        message(results)
        RJSONIO::fromJSON(results)
    }else{
        results = RCurl::getURL(
             cypherurl,
             customrequest = "POST",
             httpheader = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key)),
             postfields = post,
             .opts = list(verbose = verbose)
         )
        message("RESULT:")
        message(results)
        result = RJSONIO::fromJSON(results)
        if(length(result$data)>=2){
            setNames(data.frame(do.call(rbind,lapply(result$data, function(x) matrix(x, nrow=1)))), result$columns)
        }else if(length(result$data)==1){
            setNames(data.frame(lapply(result$data[[1]], function(x){
                                       if(length(x) == 0){
                                           matrix(NA, ncol=1)
                                       }else{
                                           matrix(x, ncol=1)
                                       }
            })), make.names(result$columns))
        }else{
            return(NA)
        }
    }
    },
    interrupt = function(ex) {
          warning("Interruptted\n");
      print(ex);
    }, error = function(ex) {
          warning("Error\n");
      print(ex);
    }, finally = {
        message("Query Completed\n")
    })
    }
    }
