#' map takes cDNA reads (fastQ format) and maps them onto contigs
#'
#' uses blat to map the mRNA to the contigs fasta and returns the tabular blast format
#' blat was chosen for its ability to show multiple hits
#' https://www.biostars.org/p/17613/
#'
#' @param reads Shortread object or path to short reads file in fastq format
#' @param contigs path to contig file in fa format
#'
#' @importFrom ShortRead writeFasta readFastq
#' @importFrom dplyr last
#' @importFrom lubridate now
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # newbler2/K00927
#' #    454AllContigs.fna
#' #    input
#' #        K00927.1.fq
#' #        K00927.2.fq
#'
#' lapply(kois, function(ko){
#'     read1 = sprintf("./newbler2/%s/input/%s.2.fq", ko, ko)
#'     if file.exists(read1)
#'         read1.fq = read1 %>% readFastq
#'     read2 = sprintf("./newbler2/%s/input/%s.2.fq", ko, ko)
#'     if file.exists(read2)
#'         read2.fq = read2 %>% readFastq
#'     combined = c(grep.cDNA(read1.fq),grep.cDNA(read2.fq))
#'     contigs = sprintf("./newbler2/%s/454AllContigs.fna", ko)
#'     output = sprintf("./blat/%s.m8", ko)
#'     blast8 = map(combined, contigs, output) %<>% tbl_df
#' })
#' }
#' @export
map <- function(reads, contigs){
    blatOutputFile = tempfile(fileext="m8")
    if(!"DNAStringSet" %in% class(reads)){
        filename = last(unlist(strsplit(reads, "/")))
        directory = paste(head(unlist(strsplit(reads, "/")), -1), collapse="/")
        reads = readFastq(directory, filname)
    }
    faFile = tempfile()
    message(sprintf("1. Converting fq to fa: %s", faFile))
    reads %>% writeFasta(faFile)
    message("2. Running blat")
    system(sprintf("blat -fastMap -out=blast8 %s %s %s", contigs, faFile, blatOutputFile))
    file.remove(faFile)
    colNames = c("query", "subject", "identity",
                   "alignLen", "mismatch", "gapOpenings",
                   "qstart", "qend", "sstart", "send",
                   "evalue", "bitscore")
    outputDF = read.table(blatOutputFile, sep="\t", h=F, comment.char="") %>%
    setNames(colNames) %>%
    mutate(
        newStart = ifelse(send - sstart > 0, sstart, send),
        newEnd   = ifelse(send - sstart > 0, send,sstart)
    ) %>% select(query:subject, qstart:send, bitscore:newEnd)
    unlink(blatOutputFile)
    outputDF
}

#' grep.cDNA
#'
#' takes the cDNA from the input file (repeat of grepReads)
#'
#' @param reads Shortreads object from bioconductor
#' @importFrom ShortRead id
#' @keywords internal
grep.cDNA = function(reads)
{
    whichIsMRNA = id(reads) %>% as.character %>% grepl("cDNA", .)
    r = reads[whichIsMRNA] %>% sread
    names(r) = id(reads[whichIsMRNA])
    r
}
