adjacentPairs<-structure(function #Convert igraph to ggvis object
    (g, #graph object,
     ko2ko =  FALSE,
    ...
    ){
        kos      =  grepl("ko:", V(g)$name) %>% which
        kopairs  =  neighborhood(graph=g, nodes=kos, order=1)   #immediate pairs

        #function to find the connected KOs given intermediate cpds
findNextKO = function(VectorID, graph, originalKO){
    neighborhood(graph = graph, nodes = VectorID, order = 1)         %>% # connectedKOs
    lapply(function(cpd) data.frame(ko1 = originalKO, ko2= cpd[-1])) %>%
           do.call(rbind, .)
}
        pairDF = do.call(rbind,lapply(kopairs, function(ko){
            originKO = ko[[1]]
            if(ko2ko){
                findNextKO(ko[-1], g, originKO)
            }else{
                data.frame(ko= originKO, # ko nodes
                           cpd = ko[-1]  # Cpd nodes
                           )
            }
        #there should be one more step where repeat pairs are removed
        }))
    }, ex= function(){
        ...
    })
