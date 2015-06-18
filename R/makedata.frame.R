make.data.frame<-structure(function(    #Function for dealing with dbquery outputs which have lists 
df##<< the data.frame output from dbquery
){
    1:ncol(df)               %>%
    lapply(function(column){
        sapply(df[,column], function(x) {ifelse(is.null(x), NA, x)})
    })                       %>%
    do.call(cbind,.)         %>%
    data.frame               %>%
    setNames(colnames(df))
}, ex=function(x){
    output.df <- dbquery(
     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
     params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
make.data.frame(output.df)
})
