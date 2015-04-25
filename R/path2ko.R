path2ko<-structure(function(#Finds all KOs belonging to a Pathways
pathway='path:ko00010',  #The pathway name
...
){
    params = list (pathwayID = pathway)
    query = "START p=node:pathwayid(pathway={pathwayID})
    MATCH (ko:ko)-[:pathwayed]-(p) 
    RETURN ko.ko as KO"
    dbquery(query=query, params=params)
    },
ex = function(x) { 
kos.in.pathway = path2ko(pathway="path:K00982")
})
