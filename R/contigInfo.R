#' contigInfo
#'
#' contigInfo function greps the contig related info such as whether its in the MDR
#'
#' @param contig the contig
#' @param ko the ko bin which the contig was assembled from
#'
#' @export
contigInfo <- function(contig, ko, withAnnotation=F){
    if(!withAnnotation){
        query = sprintf("
        MATCH 
            (contig:contigs{contig:'%s:%s'})
        WHERE 
            contig.mdr = 1 or contig.mdr is NULL
        RETURN 
            contig.bin as bin,
            contig.contig as contig,
            contig.mdr as MDR
        ", ko, contig)
    }else{
        query = sprintf("
        MATCH 
            (contig:contigs{contig:'%s:%s'})--(t:Taxon)
        WHERE 
            contig.mdr = 1 or contig.mdr is NULL
        RETURN 
            t.name as assignment,
            t.taxid as taxid,
            contig.bin as bin,
            contig.contig as contig,
            contig.mdr as MDR,
            filter(x in labels(t) where x <> 'Taxon') as Rank
        ", ko, contig)
    }
    dbquery(query)
}
