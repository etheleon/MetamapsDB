path2kingdom<-structure(function #List all intermediaries between taxa and the superkingdom it belongs to 
(taxID = '79255',
...){

query = "
START
    basetaxa=node:ncbitaxid(taxid={taxID})
MATCH
    path = basetaxa-[:childof*]->(king:superkingdom)
RETURN
    extract(n in nodes(path)| n.name) AS name,
    extract(n in nodes(path)| n.taxid) AS taxid,
    extract(n in nodes(path)| head(labels(n))) AS rank 
"
params = list(taxID = taxID)
listquery(query=query, params = params, ...)
}, ex=function() { 
df = path2kingdom(taxID='79255')
df2 = lapply(df$data[[1]], function(x) matrix(x, ncol=1))
setNames(data.frame(cbind(df2[[1]],df2[[2]],df2[[3]])), df$columns)
})
