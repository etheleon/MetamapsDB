#' Gives KO details when supplied with KO id
#'
#' @param ko the koid 
#' @param minimal return minimal details
#' @param test if it should be run as a example
#' @param ... additional dbquery parameters
#'
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' koname(test=TRUE)
#' # JSON response from DB
#' # {
#' #  "columns" : [ "ko.ko", "ko.name", "ko.definition"  ],
#' #    "data" : [ [ "ko:K00001", "E1.1.1.1, adh", "alcohol dehydrogenase [EC:1.1.1.1]"  ]  ]
#' #
#' #}
#' #
#' #       ko.ko       ko.name                      ko.definition
#' # 1 ko:K00001 E1.1.1.1, adh alcohol dehydrogenase [EC:1.1.1.1]
koname <- function(ko='K00001', minimal=TRUE, test = FALSE, ...){
    .='shutup'
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
        if(test){
        jsonresponse = '
        {
          "columns" : [ "ko.ko", "ko.name", "ko.definition"  ],
            "data" : [ [ "ko:K00001", "E1.1.1.1, adh", "alcohol dehydrogenase [EC:1.1.1.1]"  ]  ]

        }
        '
        df     <- dbquery(query=query, params=params, test=T, jsonresponse=jsonresponse,...)
        }else{
        df     <- dbquery(query=query, params=params, ...)
        }
        df
    }
