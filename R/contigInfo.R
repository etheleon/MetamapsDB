#' contigInfo
#'
#' contigInfo function greps the contig related info such as whether its in the MDR
#'
#' @param ko the ko bin which the contig was assembled from
#' @param contig contigID (optional)
#' @param withAnnotation
#'
#' @export
contigInfo <- function(ko, withAnnotation=FALSE, contig){
    if(missing(contig)){
        query = sprintf("
            MATCH
                (c:contigs{bin:'%s'})
            WHERE
                c.mdr = 1 or c.mdr is NULL
            RETURN
                c.mdr as MDR,
                c.bin as bin,
                c.contig as contig,
                c.length as length
                c.readnum as readnum", gsub("^(ko:)*", "ko:", ko))
    }else{
        if(!withAnnotation){
            query = sprintf("
            MATCH 
                (contig:contigs{contig:'%s:%s'})
            WHERE 
                contig.mdr = 1 or contig.mdr is NULL
            RETURN 
                contig.bin as bin,
                contig.contig as contig,
                contig.mdr as MDR,
                contig.readnum as readnum,
                contig.length as length
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
                contig.readnum as readnum,
                contig.length as length,
                contig.mdr as MDR,
                filter(x in labels(t) where x <> 'Taxon') as Rank
            ", ko, contig)
        }
    }
    dbquery(query)
}
