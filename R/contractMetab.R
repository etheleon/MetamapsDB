#' contractMetab shrinks a metabolic network's KOs into non-redundant units
#'
#' Constracts KOs which share the same cpd product and substrates.
#'
#' @param g metabolic graph, igraph object. vertices must have the Symbol property
#'
#' @return new non-redundant (gene) metabolic graph
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom stats "cutree" "hclust" "dist"
#' @export
contractMetab <- function(g){
    . = 'shutup'
    node = NULL
    cluster = NULL
    Symbol = NULL
#Cluster
    m                  = igraph::get.adjacency(g)
    clustersInfo       = cutree(hclust(dist(m)), h=0)
    clustersInfo       = as.data.frame(clustersInfo) %>% setNames("cluster")
    clustersInfo$node  = rownames(clustersInfo)

    #Merge only KOs
    clustersInfo = clustersInfo[which(grepl("ko", clustersInfo$node)),]

    clustersInfo$cluster  %<>% as.factor
    clustersInfo$cluster %<>% factor(labels = 1:as.integer(length(levels(clustersInfo$cluster))))
    clustersInfo$cluster %<>% as.character %>% as.integer

    starting  = max(as.integer(as.character(clustersInfo$cluster)))+1
    ending    = starting+length(grep("cpd", igraph::V(g)$name, value=T))-1

    cl = rbind(clustersInfo, 
        data.frame(
           cluster       = starting:ending,
           node          =  grep("cpd", igraph::V(g)$name, value=T)
        )
    )

    cl = igraph::V(g)$name %>% 
            lapply(function(x) dplyr::filter(cl, node == x)) %>% 
            do.call(rbind,.)

    cl$Symbol = igraph::V(g)$Symbol
    symDF = cl %>% dplyr::group_by(cluster) %>% dplyr::summarise(combSym = paste0(Symbol, collapse=" | "))

#Contract & Simplify
    g2 = igraph::contract.vertices(g, cl$cluster)
    igraph::V(g2)$Symbol = symDF$combSym
    g2 = igraph::simplify(g2)

#Sanitize 
    ##layout
    g2$layout = igraph::layout.fruchterman.reingold(g2)

    ## the definition
    igraph::V(g2)$Definition = igraph::V(g2)$name %>% sapply(function(x) {
    if(length(x) > 1){
        sapply(x, function(koid) { igraph::V(g)$Definition[igraph::V(g)$name == koid]}) %>% paste0(collapse='||')
    }else{
        igraph::V(g)$Definition[igraph::V(g)$name == x]
    }
    })

    ## the name
    igraph::V(g2)$components = igraph::V(g2)$name
    igraph::V(g2)$name %<>% sapply(paste0, collapse="_")
    g2
}
