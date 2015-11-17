#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
#' @usage lhs \%>\% rhs
NULL


#' Double Pipe operator
#'
#' @name %<>%
#' @rdname doublepipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @export
#' @usage lhs \%<>\% rhs
NULL

#' with Pipe operator
#'
#' @name %$%
#' @rdname dollarpipe
#' @keywords internal
#' @importFrom magrittr %$%
#' @export
#' @usage lhs \%$\% rhs
NULL

#' findType finds KOs/compounds ID in metabolic graph
#'
#' @param graph graph object
#' @param type either find Cpds or KOs
#'
#' @return vector which nodes are of the certain type
#' @export
findtype <- function(graph, type=c("c", "k")){
    if(type == "c"){
        which(grepl("cpd", V(graph)$name))
    }else{
        which(grepl("ko", V(graph)$name))
    }
}
