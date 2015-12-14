# taxname.sql
#
# faster way of querying the taxid names using structured SQL then querying the graphDB
#
# @param taxid vector of integers which represent NCBI's taxonomy unique identifier
# @export
taxnam.sql <- function(taxid)
{
    dbloc   =   find.package('MetamapsDB') %>% 
                    file.path('taxonomy') %>% 
                    file.path('taxidSQL')
    con     <-  RSQLite::dbConnect(SQLite(), dbname=dbloc)
    unique(taxid) %>% lapply(function(taxid){
        result = dbSendQuery(conn = con,
                    sprintf("select * from taxon where taxid == %s", taxid)
                    )
        fetch(result)
    }) %>% do.call(rbind,.)
}

#' findPerl finds the path to the executable
#'
#' Code taken from R package gdata
#'
#' @param perl full path to perl executable
#' @param verbose
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
