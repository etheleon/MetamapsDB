#' buildTree - a taxonomic tree display function
#'
#' draws the taxonomic tree of the given leave nodes
#'
#' @param taxids vector of leave nodes
#' @export
buildTree <- function(taxids){
    edgelists = taxids %>% lapply(function(x) path2kingdom(as.character(x)))

    #because the trees are of uneven length we deal with that
    #superkingdom phylum class order family genus species
    #"dear king phillip came over for good soup"

    standardisedRanks  = c("phylum", "class", "order","family", "genus", "species")

    es[1,]$rank == 'no rank'
    rankInfo = data.frame(standard = standardisedRanks)
    mDF = merge(rankInfo, es, all.x=T, by.y="rank", by.x="standard")
    which(is.na(mDF$name)) %>% sapply

    #whichever that's missing I merge together,


    es= edgelists[[1]]

    trueEdgelist = edgelists %>% 
        lapply(function(x) x$taxid %>%
               as.character %>%
               as.integer %>%
               buildE) %>%
        do.call(rbind,.)
    unique(as.data.frame(trueEdgelist)) %>% as.matrix
}



#' buildE sorts the tree into a data.frame
#'
#' @param path vector of integers representing taxid to common ancestor
#' @param ma matrix rbind with
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
