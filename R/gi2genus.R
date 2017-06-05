#' gi2rank
#'
#' Parse tabular blast format of contigs from DIAMOND for their respective genusIDs
#'
#' @param blastTab the tabular output file from DIAMOND
#' @param koi the KOs of interested eg K00001
#' @param sqlite3 the sqlite3 db
#'
#' @importFrom magrittr "%>%"
#' @importFrom data.table "fread"
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
    df = fread(tempFile)

}


