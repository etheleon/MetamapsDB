adjacentPairs<-structure(function #Convert igraph to ggvis object
    (g, #graph object
    ...
    ){
        kos      =  grepl("ko:", V(g)$name) %>% which
        kopairs  =  neighborhood(graph=g, nodes=kos, order=1)   #immediate pairs

        pairDF = do.call(rbind,lapply(kopairs, function(ko){
            originKO = ko[[1]]
            data.frame(Pair1 = originKO,
                       Pair2 = ko[-1])
        #there should be one more step where repeat pairs are removed
        }))
    }, ex= function(){
        ...
    })
