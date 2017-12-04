#' taxname.sql
#'
#' Faster way of querying the taxid names using structured SQL then querying the graphDB.
#' Downloads a copy of NCBI's taxdump.tar.gz from ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz
#' and builds a taxid, name, rank table in sqlite3
#'
#' @param taxids vector of integers/name representing the taxa of interest
#' @param byID search by ID
#' @param taxrank any of the common taxonomic ranks eg. genus
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr left_join select filter
#' @importFrom httr GET write_disk
#' @importFrom purrr map_df
#' @export
#' @examples
#' \donttest{
#'  taxnam.sql(287)
#' }
taxnam.sql <- function(taxids, byID = TRUE, taxrank = "genus")
{
    .='shutup'
    dbloc   =   find.package('MetamapsDB') %>%
                    file.path('taxonomy')  %>%
                    file.path('taxidSQL')
    if(!file.exists(dbloc)){
        type=NULL
        tmpdir  = tempdir()
        url   =  "ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz"
        anon  =  function(x, cols) as.data.frame(t(strsplit(x, split='\t|\t')[[1]][cols]))
        file  =  paste(tmpdir, basename(url), sep="/")
        results = GET(url,write_disk(file, overwrite=TRUE))
        utils::untar(file,files=c("names.dmp", "nodes.dmp"), exdir=tmpdir)
        records = purrr::map_df(readLines(con=file(paste(tmpdir, "nodes.dmp", sep="/"))), anon, cols=c(1,5)) %>% setNames(c("id", "rank"))
        records$id %<>% as.integer
        records$rank %<>% as.factor
        records_names = map_df(readLines(con=file(paste(tmpdir, "names.dmp", sep="/"))), anon, cols=c(1,3,7))
        records_names %<>% setNames(c("id", "name", "type"))
        records_names  %<>% filter(type == 'scientific name') %>% select(-type)
        records_names$id %<>% as.integer
        grandRecord = left_join(records, records_names)
        grandRecord = setNames(grandRecord, c("taxid", "rank", "name"))
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname=dbloc)
        DBI::dbSendQuery(conn = con,
            "CREATE TABLE taxon(
                id integer PRIMARY KEY,
                name varchar(20),
                rank varchar(20)
            )")
        DBI::dbWriteTable(conn = con, name = "taxon", grandRecord, overwrite=T, row.names=FALSE)
        DBI::dbDisconnect(con)
    }
    con     <-  DBI::dbConnect(RSQLite::SQLite(), dbname=dbloc)
    output = lapply(taxids, function(taxid){
    if(byID){
            query = sprintf("select * from taxon where taxid = %s", taxid)
    }else{
            query = sprintf("select * from taxon where name like \'%s\' AND rank like \'%s\'", taxid, taxrank)
    }
      unique(taxid) %>% lapply(function(taxid){
            result = DBI::dbSendQuery(conn = con, query)
            df = DBI::fetch(result)
            DBI::dbClearResult(result)
            df
        }) %>% do.call(rbind,.)
    }) %>% do.call(rbind,.)
    DBI::dbDisconnect(con)
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


