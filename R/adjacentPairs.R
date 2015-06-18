adjacentPairs<-structure(function #Convert igraph to ggvis object
    (g, #graph object,
     ko2ko =  FALSE,
    ...
    ){
        kos      =  grepl("ko:", V(g)$name) %>% which

        #Finding true biological reactions
#TODO: non-ko2ko directin is still not configured
test = do.call(rbind,lapply(c("out","in"), function(direction){
        kopairs     =   neighborhood(graph=g, nodes=kos, order=1, mode = direction)   #immediate pairs
        kopairs     =   kopairs[sapply(kopairs, length) != 1]

        #function to find the connected KOs given intermediate cpds
            findNextKO = function(VectorID, graph, originalKO){
                connectedKO = neighborhood(graph = graph, nodes = VectorID, order = 1, mode=direction)
                #removes nodes without outgoing/incoming connections
                connectedKO = connectedKO[sapply(connectedKO, length) != 1]

                do.call(rbind,lapply(connectedKO,function(cpd){
                data.frame(
                        ko1       = originalKO,
                        ko2       = cpd[-1],
                        direction = direction
                        )
                    }))
            }

        pairDF = do.call(rbind,lapply(kopairs, function(ko){
            originKO = ko[[1]]
            if(ko2ko){
                findNextKO(ko[-1], g, originKO)
            }else{
                data.frame(ko  = originKO, # ko nodes
                           cpd = ko[-1]  # Cpd nodes
                           )
            }
        #there should be one more step where repeat pairs are removed
        }))
        pairDF[
        apply(pairDF, 1, function(x) paste0(sort(x), collapse=""))                         %>%
        duplicated                                                                                %>%
        ifelse(FALSE, TRUE),]                                           # this inverses the list
        }))
    }, ex= function(){
        ...
    })
