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

}
#                    filter(fall > 0 & rise > 0) %$%
#                    filter(possibleCombis, idnum %in% id) %>% arrange(idnum, position)
#
#                FPKMdf %<>% make.data.frame
#                FPKMdf = FPKMdf[complete.cases(FPKMdf),]
#                peakExpr = FPKMdf %>%
#                    mutate(rise = middle - b4,  fall = middle - after) %>% 
#                    filter(fall > 0 & rise > 0) %$% 
#                    filter(possibleCombis, idnum %in% id) %>% arrange(idnum, position)
#
#                if(nrow(valleyCount) > 0 & nrow(peakExpr) > 0){
#                subset(possibleCombis, idnum %in% peakExpr$idnum[unique(peakExpr$idnum) %in% unique(valleyCount$idnum)])
#                }else{
#                print(sprintf("%s not found", KOI))
#                }
#
#
#valuesKout <- dbquery(query = query ,cypherurl = "192.168.100.253:7474/db/data/cypher")
#valuesKout %<>% make.data.frame
#
#someValue = valuesKout$KO %>% head(n=5) %>% 
#    lapply(function(KOI){
#           print(sprintf("Processing %s", KOI))
#           KOI_vertex <- which(V(wholeMetabolism)$name == KOI)
#
#            b4cpd    <- neighborhood(graph=wholeMetabolism, nodes = KOI_vertex, order=1, mode="in")[[1]][-1]
#            b4       <- neighborhood(graph=wholeMetabolism, nodes = b4cpd, order=1, mode="in") %>% 
#                            sapply(function(x) x[-1], simplify=F) %>%
#                            c(.,NA) %>%
#                            do.call(c,.) %>%
#                            unique %>%
#                            na.omit
#
#            aftercpd <- neighborhood(graph=wholeMetabolism, nodes = KOI_vertex, order=1, mode="out")[[1]][-1]
#            after    <- neighborhood(graph=wholeMetabolism, nodes = aftercpd, order=1, mode="out") %>% sapply(function(x) x[-1], simplify=F) %>%
#                            c(.,NA) %>%
#                            do.call(c,.) %>%
#                            unique %>%
#                            na.omit
#
#            possibleCombis =  lapply(b4, function(x){
#                        lapply(after, function(y){
#                            if(x!=y){
#                                data.frame(
#                                    b4     = V(wholeMetabolism)$name[x],
#                                    middle = V(wholeMetabolism)$name[KOI_vertex],
#                                    after  = V(wholeMetabolism)$name[y]
#                                    )
#                            }else{
#                                NULL
#                            }
#                        }) %>% do.call(rbind,.)
#                    }) %>% do.call(rbind,.) %>%
#            filter(as.character(b4)!=as.character(middle), as.character(middle)!=as.character(after))
#            if(nrow(possibleCombis)> 0){
#    #Removes
#                possibleCombis <-  possibleCombis[!possibleCombis %>% apply(1, function(x){
#                c(x["b4"], x["after"]) %>% unname %>% sort %>% paste0(collapse="")
#                }) %>% duplicated,]
#                possibleCombis %<>%
#                    mutate(idnum=1:nrow(possibleCombis)) %>% 
#                    gather(position, ko, -idnum) %>% 
#                    merge(valuesKout, by.x="ko", by.y="KO", all.x=T)
#    #ContigCount
#                contigCountsDF <- unique(possibleCombis$id) %>% 
#                lapply(function(theid, df){
#                        df                                                     %>%
#                        filter(idnum==theid)                                   %>%
#                        select(position, contigCount)                          %$%
#                        setNames(data.frame(t(matrix(contigCount))), position) %>%
#                        mutate(id=theid)
#                }, df = possibleCombis) %>% 
#                do.call(rbind,.) %>%
#                        select(b4, middle, after, id)
#                #remove trios where contigs only exist for the middle KO
#                if(toFilter){
#                contigCountsDF %<>% filter(b4 > 0 | after > 0)
#                }
#    #Expression
#                FPKMdf <- unique(possibleCombis$id) %>% 
#                lapply(function(theid, df){
#                        df                                                     %>%
#                        filter(idnum==theid)                                   %>%
#                        select(position, FPKM)                          %$%
#                        setNames(data.frame(t(matrix(FPKM))), position) %>%
#                        mutate(id=theid)
#                }, df = possibleCombis) %>% 
#                do.call(rbind,.) %>% select(b4, middle, after, id)
#
#    #Filtering
#                contigCountsDF %<>% make.data.frame
#                contigCountsDF = contigCountsDF[complete.cases(contigCountsDF),]
#
#                valleyCount = contigCountsDF %>%
#                    mutate(rise = b4 - middle,  fall = after - middle) %>% 
#                    filter(fall > 0 & rise > 0) %$%
#                    filter(possibleCombis, idnum %in% id) %>% arrange(idnum, position)
#
#                FPKMdf %<>% make.data.frame
#                FPKMdf = FPKMdf[complete.cases(FPKMdf),]
#                peakExpr = FPKMdf %>%
#                    mutate(rise = middle - b4,  fall = middle - after) %>% 
#                    filter(fall > 0 & rise > 0) %$% 
#                    filter(possibleCombis, idnum %in% id) %>% arrange(idnum, position)
#
#                if(nrow(valleyCount) > 0 & nrow(peakExpr) > 0){
#                subset(possibleCombis, idnum %in% peakExpr$idnum[unique(peakExpr$idnum) %in% unique(valleyCount$idnum)])
#                }else{
#                print(sprintf("%s not found", KOI))
#                }
#}
#})
