#' Gives KO details when supplied with KO id
#'
#' @param ko the koid 
#' @param minimal return minimal details
#' @param ... additional dbquery parameters
#'
#' @importFrom magrittr "%>%"
#' @export
koname <- function(ko='K00001', minimal=TRUE,...){
        ko = gsub("^(ko:)*","ko:",ko)
        if(!minimal){
            query = "
            UNWIND
                { koname } AS KOSS
            MATCH
                (ko:ko {ko : KOSS.ko})
            RETURN 
                ko.ko, ko.name, ko.definition, ko.pathway, ko.`pathway.name`
            "
        }else{
            query = "
            UNWIND
                { koname } AS KOSS
            MATCH
                (ko:ko {ko : KOSS.ko})
            RETURN 
                ko.ko, ko.name, ko.definition
            "
        }

        params <- ko %>% lapply(function(x){list(ko=x)}) %>% list(koname=.)
        df     <- dbquery(query=query, params=params, ...)
                         # cypherurl= "http://metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
        df
    }
