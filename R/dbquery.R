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
#' @param ... allows for additional arguments to be passed into dbquery
#'
#' @export
dbquery <- function(query, params = FALSE, cypherurl, user, password, justPost=FALSE, verbose=FALSE,...){
    #Checks if the params is given
    tryCatch({
        message("Sending query\n")
    if(is.list(params)){
        post = RJSONIO::toJSON(list(query = query, params = params))
        #cat(post)
    }else{
        post = RJSONIO::toJSON(list(query = query))
        #cat(post)
    }

    key <- base64enc::base64encode(charToRaw(paste(user, password, sep=":")))

    if(justPost){
        RJSONIO::fromJSON(RCurl::getURL(
                 cypherurl,
                 customrequest = "POST",
httpheader = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key)),
                 postfields = post,
                 .opts = list(verbose = verbose)
                 ))
    }else{
        result = RJSONIO::fromJSON(
        RCurl::getURL(
                                                 cypherurl,
                                                 customrequest = "POST",
                                                 httpheader = c('Content-Type' = 'application/json', 'Authorization' = paste('Basic', key)),
                                                 postfields = post,
                                                 .opts = list(verbose = verbose)
                                                 ))

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
