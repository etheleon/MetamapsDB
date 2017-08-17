#' findHomology searches the neo4j db for the KO binned contigs' homology assignments
#'
#' used after homology search data has been uploaded into the db
#'
#' @export
findHomology <- function(koi){
    koi = gsub("^(ko:)*", "ko:", koi)
    query = "
        OPTIONAL MATCH
            (k:ko{ko:{ko}})<-[:assignment]-(c:contigs)-[:taxomapped]->(t:Taxon)
        WHERE
            c.mdr=1 AND c.spanning=1 AND toInt(c.readnum) > k.threshold
        with
            t, c
        OPTIONAL MATCH
            (t)-[:childof*]->(t2:genus)
        RETURN
            c.contig as contigID,
            t.taxid as primary,
            CASE
            WHEN t:genus THEN t.taxid
            ELSE t2.taxid
            END AS genus
    "
    dbquery(query, list(ko = koi)) %>% make.data.frame %>% tbl_df %>% unique
}
