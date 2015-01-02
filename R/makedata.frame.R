make.data.frame<-structure(function(    #Function for dealing with dbquery outputs which have lists 
df##<< the data.frame output from dbquery
){
setNames(data.frame(do.call(cbind,lapply(1:ncol(df), function(column) unlist(df[,column])))), colnames(df))
}, ex=function(x){
    output.df <- dbquery(
     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
     params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
make.data.frame(output.df)
})
