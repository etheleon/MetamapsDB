grepgraph<-structure(function(#Generates metabolic network as IGRAPH object from input vector KOs
###Takes in a vector list of KO ids and generates a metabolic graph as a igraph object
kos, ##<< this will take the input KOs 
...){
##author<< Wesley Goi
#Processing
#1. Cypher Queries 
query_cpd2ko="
START 
    ko=node:koid(ko={koid}) 
OPTIONAL MATCH 
    ko<--(cpd:cpd) 
RETURN 
    cpd.cpd AS child, 
    ko.ko as parent, 
    cpd.name as childName, 
    ko.definition as parentName"

query_ko2cpd="
START 
    ko=node:koid(ko={koid}) 
OPTIONAL MATCH 
    ko-->(cpd:cpd) 
RETURN 
    ko.ko AS child,
    cpd.cpd AS parent, 
    ko.definition AS childName,
    cpd.name AS parentName"
fulldata = do.call(rbind, lapply(kos, function(ko)
    {
    if(!grepl("ko:",ko))
        ko = gsub("^", "ko:", ko)
        df = data.frame(child = as.character(), parent = as.character(), childName = as.character(), parentName= as.character())
        df1 = dbquery(query=query_cpd2ko, params = list(koid = ko))
        df2 = dbquery(query=query_ko2cpd, params = list(koid = ko))
        if(!is.na(df1))
            df = rbind(df,df1)
        if(!is.na(df2))
            df = rbind(df,df2)
                    }))
fulldata2 = make.data.frame(fulldata)
#fulldata2 = do.call(cbind,apply(fulldata, 2, function(x) unlist(x)))
fulldata2 = fulldata[complete.cases(fulldata2),]
#3. Vertices & Edgelist
vertex.data <- with(fulldata2,
     setNames(unique(data.frame(
        id = c(as.character(child), as.character(parent)),
        name = c(as.character(childName), as.character(parentName))
        )), c("Vertex","Definition"))
     )

g=simplify(graph.data.frame(d=unique(fulldata2[,1:2]),vertices=vertex.data))
g$layout = layout.fruchterman.reingold(g)
g
##<< Description 
##Takes in KO and outputs the metabolic graph with 
}, ex=function(x) { 
    data(top500kos)
    mbgraph<-grepgraph(top500kos)
    p1 = plot(
    mbgraph, 
    vertex.label=V(g)$name, 
    vertex.size = 1,
    edge.arrow.size=0.1,
    vertex.frame.color="#FFFFFF00", 
    vertex.color=c("grey","red")

)
})
