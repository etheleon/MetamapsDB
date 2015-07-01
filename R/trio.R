trio <- structure(function( ##<< Function to find trios.
### Finds trios
KOI = 'ko:K00001', ##<< the KO of interest
toUnique = TRUE,
... ##<< other arguments such as cypherurl
){

#' Round 0: 
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

#cypher =  "192.168.100.253:7474/db/data/cypher"

#query="
#    START
#        inputko=node:koid(ko={koid})
#    WITH 
#        inputko,
#        inputko.ko          AS middle,
#        inputko.contigCount AS middleContigCount,
#        inputko.expression  AS middleExpression
#    MATCH
#        (ko1:ko)-->(:cpd)-->(inputko)-->(:cpd)-->(ko3:ko)
#    RETURN
#        distinct ko1.ko          AS before,
#        distinct ko1.contigCount as beforeContigCount,
#        distinct ko1.expression  as beforeExpression,
#        middle,
#        middleContigCount,
#        middleExpression,
#        ko3.ko          AS after,
#        ko3.contigCount AS afterContigCount,
#        ko3.expression  AS afterExpression
#

#Round 1: Find the paths

query <- "
    START
        inputko=node:koid(ko={koid})
    WITH
        inputko,
        inputko.ko          AS middle,
    MATCH
        (ko1:ko)-->(:cpd)-->(inputko)-->(:cpd)-->(ko3:ko)
    RETURN
        ko1.ko          AS before,
        middle,
        ko3.ko          AS after,
"

params <- list(koid = KOI)
trioDF <- dbquery(query,params,...)

trioDF %<>% make.data.frame
trioDF = trioDF[complete.cases(trioDF),]

# not uturn type reactions
trioDF %<>% filter(before != after) %>% unique

if(toUnique){
    #remove reverse rxns
    trioDF = trioDF[!duplicated(
                trioDF                             %>%
                apply(1, function(row){
                    c(row["before"], row["after"]) %>%
                    sort                           %>%
                    paste0(collapse="")
                })
    ),]
    #remove redundant
}

#Round 2: Find the contigs information
kos = with(trioDF, c(before, after,middle) %>% unique)

params  = kos %>% lapply(function(x) list(ko=x)) %>% list(kos=.)
query   = "
    UNWIND
        { kos } AS koss
    MATCH
        (inputko:ko {ko : koss.ko})
    RETURN
        inputko.ko          AS koID,
        inputko.contigCount AS ContigCount,
        inputko.expression  AS Expression
"

contigInfo <- dbquery(query,params,...)
contigInfo %<>% make.data.frame
list(trioDF, contigInfo)

}, ex=function(){
#...
})

