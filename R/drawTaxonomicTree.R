#' Taxonomic tree of the taxons
#'
#' `buildTree` Returns taxonomic tree as a igraphObject of the taxids up to phylum
#'
#' @param taxids vector of leave nodes
#' @param standardisedRanks the ranks to report. superkingdom phylum class order family genus species
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom purrr map_df
#' @importFrom igraph graph_from_data_frame
#' @examples
#' \dontrun{
#' standardisedRanks  = c("superkingdom", "phylum", "class", "order","family", "genus", "species")
#' buildTree(taxids = c(287, 280), standardisedRanks )
#' }
#' @export
buildTree <- function(
        taxids = c(287, 280),
        standardisedRanks  = c("superkingdom", "phylum", "class", "order","family", "genus", "species")
    ){
    .="shutup"
    taxid = NULL
    edgelists = taxids %>% lapply(function(x) path2kingdom(as.character(x)))
    rankInfo = data.frame(standard = standardisedRanks) %>% mutate(order=1:n())
    trueEdgelist = edgelists       %>%
        lapply(function(x){
           merge(rankInfo, x, all.x=T, by.y="rank", by.x="standard") %>%
               arrange(order) %>%
           .$taxid %>%
           as.character        %>%
           as.integer          %>%
           buildE})            %>%
        do.call(rbind,.) %>% as.data.frame %>% unique %>% as.matrix
    nodeDetails = as.vector(trueEdgelist) %>% map_df(taxnam.sql) %>% dplyr::arrange(taxid) %>% unique
    list(edgelist = trueEdgelist, nodeDetails = nodeDetails, graph=graph_from_data_frame(trueEdgelist, vertices=nodeDetails))
}



#' buildE sorts the tree into a data.frame
#'
#' @param path vector of integers representing taxid to common ancestor
#' @param ma matrix rbind with
#' @keywords internal
buildE <- function(path, ma){


    if(missing(ma)) ma = matrix(ncol=2, nrow=0)

    if(length(path) > 2){
        mm = matrix(path[1:2], ncol=2)
        mm = rbind(ma, mm)
        buildE(path[2:length(path)], mm)
    }else{
        mm = matrix(path[1:2], ncol=2)
        mm = rbind(ma, mm)
        mm
    }
}

