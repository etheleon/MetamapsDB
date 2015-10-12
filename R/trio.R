#' Function to find trios.
#'
#' @param KOI the ko id
#' @param toUnique  if to return only unique
#' @param withDetails with details
#' @param ... the other args for dbquery
#'
#' @export
trio <- function( 
KOI         = 'ko:K00001', 
toUnique    = TRUE,
withDetails = FALSE,
...
){

        KOI = gsub("^(ko:)*","ko:",KOI)
#' Round 0: 
#' Run this after starting the db'
#query="
#    OPTIONAL MATCH
#        (ko:ko)--(c:contigs)
#    WITH 
#        ko,
#        count(DISTINCT c)                                                                 AS contigCount,
#        reduce(sum=0, FPKM in extract(n in collect(DISTINCT c) |n.cDNAFPKM) | sum + FPKM) AS FPKM
#    SET
#        ko.contigCount = contigCount,
#        ko.expression  = FPKM"

#Round 1: Find the paths

query <- "
    MATCH
        (ko1:ko)-->(:cpd)-->(inputko:ko {ko:{koid}})-->(:cpd)-->(ko3:ko)
    RETURN
        ko1.ko     AS before,
        inputko.ko AS middle,
        ko3.ko     AS after
"

params <- list(koid = KOI)
trioDF <- dbquery(query,params,...)
if(!is.na(trioDF)){
trioDF$before %<>% as.character
trioDF$middle %<>% as.character
trioDF$after  %<>% as.character

# not uturn type reactions (redundant KOs)
trioDF %<>% dplyr::filter(before != after) %>% unique
if(toUnique){
    #remove reverse rxns
    trioDF = trioDF[!duplicated(
                trioDF                             %>%
                apply(1, function(row){
                    c(row["before"], row["after"]) %>%
                    sort                           %>%
                    paste0(collapse="")
                })
    ),]
    #remove redundant
}

#Round 2: Find the contigs information
if(withDetails){
    kos = with(trioDF, c(before, after,middle) %>% unique)

    params  = kos %>% lapply(function(x) list(ko=x)) %>% list(kos=.)
    query   = "
        UNWIND
            { kos } AS koss
        MATCH
            (inputko:ko {ko : koss.ko})
        RETURN
            inputko.ko          AS koID,
            inputko.contigCount AS ContigCount,
            inputko.expression  AS Expression
    "

    contigInfo <- dbquery(query,params,...)
    contigInfo %<>% make.data.frame
    contigInfo <- contigInfo[complete.cases(contigInfo),]
    list(trioDF, contigInfo)
}else{
    trioDF
}
}else{
NA
}
}
