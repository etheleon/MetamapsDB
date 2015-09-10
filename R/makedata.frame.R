#' Convert columns with nested list structures into plain vectors
#'
#' Certain dbquerys will return data.frames with columns having lists, this function reduces this
#'
#' @param df the data.frame output from dbquery
#' @param string2factor to store strings as factor
#'
#' @importFrom magrittr "%>%"
#'
#' @export
make.data.frame <- function(
df, string2factor = FALSE){
    1:ncol(df)               %>%
    lapply(function(column){
        sapply(df[,column], function(x) {ifelse(is.null(x), NA, as.character(x))})
    })                       %>%
    do.call(cbind,.)         %>%
    data.frame(stringsAsFactors=string2factor)               %>%
    setNames(colnames(df))
}
