#' map takes cDNA reads (fastQ format) and maps them onto contigs
#'
#' uses blat to map the mRNA to the contigs fasta and returns the tabular blast format
#' blat was chosen for its ability to show multiple hits
#' https://www.biostars.org/p/17613/
#'
#' @param reads path to short reads file in fastq format
#' @param contigs path to contig file in fa format
#'
#' @importFrom ShortRead writeFasta, readFastq
#' @importFrom dplyr last
#' @importFrom lubridate now
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' 3
#' }
#' @export
map <- function(reads, contigs, blatOutputFile){
    filename = last(unlist(strsplit(reads, "/")))
    directory = paste(head(unlist(strsplit(reads, "/")), -1), collapse="/")

    faFile = sprintf("/dev/shm/shortReads-%s.fa", as.integer(now()))
    message(sprintf("1. Converting fq to fa: %s", faFile))
    readFastq(directory, filname) %>% writeFasta(faFile)
    message("2. Running blat")
    system(sprintf("blat -fastMap -out=blast8 %s %s %s", contigs, faFile, blatOutputFile))

    colNames = c("query", "subject", "identity",
                   "alignLen", "mismatch", "gapOpenings",
                   "qstart", "qend", "sstart", "send",
                   "evalue", "bitscore")
    read.table(blatOutputFile, sep="\t", h=F, comment.char="") %>%
    setNames(colNames) %>%
    mutate(
        newStart = ifelse(send - sstart > 0, sstart, send),
        newEnd   = ifelse(send - sstart > 0, send,sstart)
    ) %>% select(query:subject, qstart:send, bitscore:newEnd)
}
