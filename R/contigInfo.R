#' contigInfo
#'
#' contigInfo function greps the contig related info such as whether its in the MDR
#'
#' @param ko the ko bin which the contig was assembled from
#' @param contig contigID (optional)
#' @param withAnnotation 0j/
#' @examples
#' \dontrun{
#' To query a specific contig
#' contigInfo(ko = "K00001", contig="contig00001")
#' # Sending query
#' # Query Completed
#'
#' #        bin             contig MDR spanning readnum length
#' #1 ko:K00001 K00001:contig00001   1        1    1497   1236
#' With annotation
#'    assignment taxid       bin             contig readnum length MDR spanning
#' 1 Bacteroides   816 ko:K00001 K00001:contig00001    1497   1236   1        1
#' 2 Bacteroides   816 ko:K00001 K00001:contig00001    1497   1236   1        1
#'      Rank
#' 1   genus
#' 2 simTaxa
#'
#' To query all contigs associated with a KO
#' contigInfo(ko = "K00001") %>% head
#' #Sending query
#' #
#' #Query Completed
#' #
#' #   MDR spanning       bin             contig length readnum
#' #1 <NA>     <NA> ko:K00001 K00001:contig00168    297      31
#' #2 <NA>     <NA> ko:K00001 K00001:contig00167    298     118
#' #3    1        0 ko:K00001 K00001:contig00166    298      45
#' #4    1        0 ko:K00001 K00001:contig00164    299     236
#' #5    1        1 ko:K00001 K00001:contig00163    300     106
#' #6 <NA>     <NA> ko:K00001 K00001:contig00162    302     310
#' }
#' @export
contigInfo <- function(ko, contig, withAnnotation=FALSE){
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
