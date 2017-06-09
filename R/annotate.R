#' Adds taxonomic annotations of the contigs
#'
#' After loading into the graphDB contigs information we annotate contigs with their taxonomy sequences
#'
#'
#' @param csv FULL path pointing to outputfile from mapBlat package's blast2lcaIn fxn
#' @export
annotateContigs.taxonomy <- function(csv)
{
    inputfile = paste0("file://", csv)
    query = paste0('
    USING PERIODIC COMMIT 100000
    LOAD CSV WITH HEADERS FROM "', inputfile, '" AS mapping
    MATCH (c:contigs{contig:mapping.contigid}),(t:Taxon{taxid:mapping.taxid})
    MERGE (c)-[:taxomapped]->(t)
    ')
    message(query)
    dbquery(query, justPost=TRUE)
}
