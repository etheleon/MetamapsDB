#' gi2rank
#'
#' Parse tabular blast format of contigs from DIAMOND for their respective genusIDs
#'
#' @param blastTab the tabular output file from DIAMOND
#' @param koi the KOs of interested eg K00001
#' @param sqlite3 the sqlite3 db
#'
#' @export
gi2rank <- function(blastTab, koi, sqlite3 = "sql3db")
{
    tempFile = paste0(getwd(), "/tempFile")
    #print(tempFile)
    koi = gsub("^ko:", "", koi) %>% paste(collapse=" ")

    parserLoc <- find.package('MetamapsDB') %>%
                    file.path('python')    %>%
                    file.path('gi2tax.py')

    cmd <- sprintf("%s %s --koi %s", parserLoc, blastTab, koi)
    if(sqlite3 == "sql3db")
    {
        cmd <- cmd %>% paste0(sprintf(" > %s", tempFile))
    }else
    {
        cmd <- cmd %>% paste0(sprintf(" --sqlite3 %s > %s", sqlite3, tempFile))
    }
    print(cmd)
    try(system(cmd))
    df = data.table::fread(tempFile)

}

#' findpython finds the path to the executable
#'
#' Code taken from R package gdata
#'
#' @param python full path to python executable
#' @param verbose print extra information
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
