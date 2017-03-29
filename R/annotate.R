#' Adds taxonomic annotations of the contigs
#'
#' After loading into the graphDB contigs information we annotate contigs with their taxonomy sequences
#'
#'
#' @param csv FULL path pointing to outputfile from mapBlat package's blast2lcaIn fxn
annotateContigs.taxonomy <- function(csv)
{
    inputfile = paste("file://", csv)
    query = paste('
    USING PERIODIC COMMIT 500
    LOAD CSV WITH HEADERS FROM ', inputfile, ' AS mapping
    CREATE UNIQUE
        (c:contigs{contig:mapping.contigid})-[:taxomapped]->(t:Taxon{taxid:mapping.taxid})
    ', sep="")
    message(query)
    dbquery(query, justPost=TRUE)
}
