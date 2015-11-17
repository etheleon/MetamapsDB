#' contractMetab shrinks a metabolic network's KOs into non-redundant units
#'
#' Constracts KOs which share the same cpd product and substrates.
#'
#' @param g metabolic graph, igraph object. vertices must have the Symbol property
#'
#' @return new non-redundant (gene) metabolic graph
#'
#' @export
contractMetab <- function(g){

#Cluster
    m                  = get.adjacency(g)
    clustersInfo       = cutree(hclust(dist(m)), h=0)
    clustersInfo       = as.data.frame(clustersInfo) %>% setNames("cluster")
    clustersInfo$node  = rownames(clustersInfo)

    #Merge only KOs
    clustersInfo = clustersInfo[which(grepl("ko", clustersInfo$node)),]

    clustersInfo$cluster  %<>% as.factor
    clustersInfo$cluster %<>% factor(labels = 1:as.integer(length(levels(clustersInfo$cluster))))
    clustersInfo$cluster %<>% as.character %>% as.integer

    starting  = max(as.integer(as.character(clustersInfo$cluster)))+1
    ending    = starting+length(grep("cpd", V(g)$name, value=T))-1

    cl = rbind(clustersInfo, 
        data.frame(
           cluster       = starting:ending,
           node          =  grep("cpd", V(g)$name, value=T)
        )
    )

    cl = V(g)$name %>% 
            lapply(function(x) dplyr::filter(cl, node == x)) %>% 
            do.call(rbind,.)

    cl$Symbol = V(g)$Symbol
    symDF = cl %>% dplyr::group_by(cluster) %>% dplyr::summarise(combSym = paste0(Symbol, collapse=" | "))

#Contract & Simplify
    g2 = contract.vertices(g, cl$cluster)
    V(g2)$Symbol = symDF$combSym
    g2 = simplify(g2)

#Sanitize 
    ##layout
    g2$layout = layout.fruchterman.reingold(g2)

    ## the definition
    V(g2)$Definition = V(g2)$name %>% sapply(function(x) {
    if(length(x) > 1){
        sapply(x, function(koid) { V(g)$Definition[V(g)$name == koid]}) %>% paste0(collapse='||')
    }else{
        V(g)$Definition[V(g)$name == x]
    }
    })

    ## the name
    V(g2)$components = V(g2)$name
    V(g2)$name %<>% sapply(paste0, collapse="_")
    g2
}
