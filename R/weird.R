#' mappingInfo sequence analysis of contigs (for SIMULATION only)
#'
#' Used for mapping short reads against contigs using blast
#'
#' @param koi the KO of interest
#' @param doBlast read from file or carry out blast
#' @param ... root, outputDir, query, rangesFile, genomesFile, blastFile
#' @importFrom magrittr "%T>%"
#'
#' @examples
#' \dontrun{
#' blastfile = "~/simulation_fr_the_beginning/reAssemble/everybodyelse/out/simBlast/K00927/blast.tsv"
#' mappingInfo(blastfile, doBlast=FALSE)
#' }
#' @export
mappingInfo <- function(koi = 'K00927', doBlast=FALSE, ...){
    . = 'shutup'
    root = NULL
    outputDir = NULL
    query = NULL
    rangesFile  = NULL
    genomesFile = NULL
    alignment.length = NULL
    perc.identity = NULL
    alignPerc = NULL
    readnum = NULL
    subject = NULL
    if(doBlast){
        path <- paste(system.file(package="MetamapsDB"), "python", "simBlast.py", sep="/")
        cmd = paste(findPython(), path, root, koi, outputDir, query, rangesFile, genomesFile)
        cmd %T>% message("reading the following into memory: ",.) %>% system
        blastFile = sprintf("%s/blast.tsv", outputDir)
    }else{
        blastFile = list(...)$blastFile
    }
    blastdf = blastFile %>% read.table(sep="\t") %>% setNames(c("query","subject","perc.identity","alignment.length","mismatches","gap.openings","q.start","q.end","s.start","s.end","evalue","bitScore"))
    blastdf$query %<>% as.character
    contigDF = unique(blastdf$query) %>% as.character %>% lapply(function(contig){
        contigInfo(ko = koi, contig=contig)
    }) %>% do.call(rbind,.)
    contigDF$contig %<>% as.character %>% strsplit('\\:') %>% sapply(function(x) x[2])


    blastdf = merge(blastdf, contigDF, by.x = "query", by.y="contig", all.x=T)
    blastdf$length %<>% as.character %>% as.integer
    blastdf$readnum %<>% as.character %>% as.integer

    threshold = dbquery("match (k:ko{ko:{ko}}) return k.threshold as threshold", list(ko=gsub("^(ko:)*", "ko:", koi))) %$% threshold
    blastdf %<>% mutate(alignPerc = alignment.length / length)

    blastdf %>% filter(
        perc.identity == 100,
        alignPerc > 0.9,
        readnum > threshold) %>%
    arrange(desc(subject))
    blastdf
}
