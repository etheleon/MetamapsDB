trio <- structure(function( ##<< Function to find trios.
KOI = 'ko:K00001', ##<< the KO of interest
toFilter = FALSE
){
#' Run this after starting the db'
#query="
#    OPTIONAL MATCH
#        (ko:ko)--(c:contigs)
#    WITH 
#        ko,
#        count(DISTINCT c)                                                                 AS contigCount,
#        reduce(sum=0, FPKM in extract(n in collect(DISTINCT c) |n.cDNAFPKM) | sum + FPKM) AS FPKM
#    SET
#        ko.contigCount = contigCount,
#        ko.expression  = FPKM"

cypher =  "192.168.100.253:7474/db/data/cypher"

query="
    START
        inputko=node:koid(ko={koid})
    WITH 
        inputko,
        inputko.ko          AS middle,
        inputko.contigCount AS middleContigCount,
        inputko.expression  AS middleExpression
    MATCH
        (ko1:ko)-->(:cpd)-->(inputko)-->(:cpd)-->(ko3:ko)
    RETURN
        ko1.ko          AS before,
        ko1.contigCount as beforeContigCount,
        ko1.expression  as beforeExpression,
        middle,
        middleContigCount,
        middleExpression,
        ko3.ko          AS after,
        ko3.contigCount AS afterContigCount,
        ko3.expression  AS afterExpression
"

params = list(koid = KOI)
cypher="192.168.100.253:7474/db/data/cypher"

trioDF = dbquery(query,params, cypher)
trioDF %<>% make.data.frame
trioDF = trioDF[complete.cases(trioDF),]
trioDF %<>% filter(before != after)
if(toFilter){
    trioDF %>%  mutate( contig.rise     = as.numeric(beforeContigCount) - as.numeric(middleContigCount),
                        contig.fall     = as.numeric(afterContigCount) - as.numeric(middleContigCount),
                        expression.rise = as.numeric(beforeExpression) - as.numeric(middleExpression),
                        expression.fall = as.numeric(afterExpression) - as.numeric(middleExpression)
                        ) %>% 
    filter(contig.rise > 0 & contig.fall >0 & expression.rise <0 & expression.fall <0)
}else{
    trioDF
}
}, ex=function(){
...
})

