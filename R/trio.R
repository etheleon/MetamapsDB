trio <- structure(
trio = function(
    ...
){
query="
    START 
        inputko=node:koid('ko:\"ko:K00001\"') 
    MATCH
        (inputko)--(c1:contigs)
    WITH
        count(c1) as KO1count, inputko
    MATCH
        (inputko)-->(:cpd)-->(ko2:ko)-->(:cpd)-->(ko3:ko)
    WITH
        inputko.ko as KO1,
        KO1count,
        ko2,
        ko3
    MATCH
        (ko2)--(c2:contigs)
    WITH
        KO1,
        KO1count,
        ko2.ko as KO2,
        count(c2) as KO2count,
        ko3
    MATCH
        (ko3)--(c3:contigs)
    RETURN 
        KO1,
        KO1count,
        KO2,
        KO2count,
        ko3.ko     AS KO3,
        count(c3)  AS KO3count
"
params = 
    dbquery(query = query, ...)
}

, example(x){
    ...
})
    
