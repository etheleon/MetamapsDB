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
    LOAD CSV WITH HEADERS FROM ', inputfile, ' AS mapping
    CREATE (c:contigs{contig:mapping.contID})-[:taxomapped {score: mapping.score}]->(t:Taxon{taxid:mapping.taxid})
    ', sep="")
    dbquery(query)
}
