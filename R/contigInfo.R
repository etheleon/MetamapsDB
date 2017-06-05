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
            RETURN
                c.mdr as MDR,
                c.spanning as spanning,
                c.bin as bin,
                c.contig as contig,
                c.length as length,
                c.readnum as readnum", gsub("^(ko:)*", "ko:", ko))
    }else{
        if(!withAnnotation){
            query = sprintf("
            MATCH 
                (contig:contigs{contig:'%s:%s'})
            WHERE 
                contig.mdr = 1 or contig.mdr is NULL or contig.spanning = 1 or contig.spanning is NULL
            RETURN 
                contig.bin as bin,
                contig.contig as contig,
                contig.mdr as MDR,
                contig.spanning as spanning,
                contig.readnum as readnum,
                contig.length as length
            ", ko, contig)
        }else{
            query = sprintf("
            MATCH 
                (contig:contigs{contig:'%s:%s'})--(t:Taxon)
            WHERE 
                contig.mdr = 1 or contig.mdr is NULL or contig.spanning = 1 or contig.spanning is NULL
            RETURN 
                t.name as assignment,
                t.taxid as taxid,
                contig.bin as bin,
                contig.contig as contig,
                contig.readnum as readnum,
                contig.length as length,
                contig.mdr as MDR,
                contig.spanning as spanning,
                filter(x in labels(t) where x <> 'Taxon') as Rank
            ", ko, contig)
        }
    }
    df = dbquery(query) %>% make.data.frame
    df$readnum %<>% as.character %>% as.integer
    df
}
