#' taxname.sql
#'
#' faster way of querying the taxid names using structured SQL then querying the graphDB
#'
#' @param taxids vector of integers/name representing the taxa of interest
#' @param byID search by ID
#' @param taxrank any of the common taxonomic ranks eg. genus
#' @import RSQLite
#' @import DBI
#' @importFrom magrittr "%>%"
#' @export
taxnam.sql <- function(taxids, byID = TRUE, taxrank = "genus")
{
    .='shutup'
    dbloc   =   find.package('MetamapsDB') %>%
                    file.path('taxonomy')  %>%
                    file.path('taxidSQL')
    con     <-  DBI::dbConnect(SQLite(), dbname=dbloc)

   output = lapply(taxids, function(taxid){
    if(byID){
            query = sprintf("select * from taxon where taxid = %s", taxid)
    }else{
            query = sprintf("select * from taxon where name like \'%s\' AND rank like \'%s\'", taxid, taxrank)
    }
      unique(taxid) %>% lapply(function(taxid){
            result = DBI::dbSendQuery(conn = con, query)
            df = fetch(result)
            DBI::dbClearResult(result)
            df
        }) %>% do.call(rbind,.)
    }) %>% do.call(rbind,.)
    dbDisconnect(con)
    output
}

#' findPerl finds the path to the executable
#'
#' Code taken from R package gdata
#'
#' @param perl full path to perl executable
#' @param verbose print extra
#' @keywords internal
findPerl <- function(perl, verbose = "FALSE")
{
  errorMsg <- "perl executable not found. Use perl= argument to specify the correct path."

  if (missing(perl))
    {
      perl = "perl"
    }

  perl = Sys.which(perl)
  if (perl=="" || perl=="perl")
    stop(errorMsg)

  if (.Platform$OS == "windows") {
    if (length(grep("rtools", tolower(perl))) > 0) {
      perl.ftype <- shell("ftype perl", intern = TRUE)
      if (length(grep("^perl=", perl.ftype)) > 0) {
        perl <- sub('^perl="([^"]*)".*', "\\1", perl.ftype)
      }
    }
      }

  if (verbose) cat("Using perl at", perl, "\n")

  perl
}

#' findPython finds the path to the executable
#'
#' Code taken from R package gdata
#'
#' @param python full path to perl executable
#' @param verbose print extra
#' @keywords internal
findPython <- function(perl, verbose = "FALSE")
{
  errorMsg <- "Python executable not found. Use python= argument to specify the correct path."

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


