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

#' findpython finds the path to the executable
#'
#' Code taken from R package gdata
#'
#' @param python full path to python executable
#' @param verbose print extra information
#' @export
findPython <- function(python, verbose = "FALSE")
{
  errorMsg <- "python executable not found. Use python= argument to specify the correct path."

  if (missing(python))
    {
      python = "python"
    }

  python = Sys.which(python)
  if (python=="" || python=="python")
    stop(errorMsg)

  if (.Platform$OS == "windows") {
    if (length(grep("rtools", tolower(python))) > 0) {
      python.ftype <- shell("ftype python", intern = TRUE)
      if (length(grep("^python=", python.ftype)) > 0) {
        python <- sub('^python="([^"]*)".*', "\\1", python.ftype)
      }
    }
      }

  if (verbose) cat("Using python at", python, "\n")

  python
}
