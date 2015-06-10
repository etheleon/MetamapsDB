trio <- structure(function( ##<< Function to find trios.
KOI = ko:K00001,    ##<< name of the KO with ko: prefix
    ...
){

KOI = deparse(substitute(KOI))
query="
    START 
        inputko=node:koid(ko={koid})
    MATCH
        (inputko)--(c1:contigs)
    WITH
        count(DISTINCT c1)                                                                 AS KO1count,
        reduce(sum=0, FPKM in extract(n in collect(DISTINCT c1) |n.cDNAFPKM) | sum + FPKM) AS C1FPKM,
        inputko
    MATCH
        (inputko)-->(:cpd)-->(ko2:ko)-->(:cpd)-->(ko3:ko)
    WITH
        inputko.ko                                                                          AS KO1,
        KO1count,
        C1FPKM,
        ko2,
        ko3
    MATCH
        (ko2)--(c2:contigs)
    WITH
        KO1,
        KO1count,
        C1FPKM,
        ko2.ko                                                                              AS KO2,
        reduce(sum=0, FPKM in extract(n in collect(DISTINCT c2) | n.cDNAFPKM) | sum + FPKM) AS C2FPKM,
        count(DISTINCT c2)                                                                  AS KO2count,
        ko3
    MATCH
        (ko3)--(c3:contigs)
    RETURN
        KO1,
        KO1count,
        C1FPKM,

        KO2,
        KO2count,
        C2FPKM,

        ko3.ko                                                                             AS KO3,
        count(DISTINCT c3)                                                                 AS KO3count,
        reduce(sum=0, FPKM in extract(n in collect(DISTINCT c3) |n.cDNAFPKM) | sum + FPKM) AS C3FPKM
"
dbquery(query = query,list(koid = KOI), ...)
}
, example(x){
    df <- trio(ko:K00001)

# Plot1: Number of genes for TRIO --------------------------------------#
newdf = lapply(1:nrow(df), function(counter, thedf){
    data.frame(
        ko     = c(thedf[counter,"KO1"],thedf[counter,"KO2"],thedf[counter,"KO3"]) %>% unlist %>% unname,
        counts = c(thedf[counter,"KO1count"], thedf[counter,"KO2count"], thedf[counter,"KO3count"]) %>% unlist %>% unname,
        group = counter,
        koid = c(1,2,3)
    )
}, thedf = df) %>% do.call(rbind,.)

    ggplot(newdf, aes(as.factor(koid), counts, color=as.factor(group))) +
        geom_point()                                                    +
        geom_path(aes(group=group))                                     +
        theme(legend.position="none")                                   +
        labs(x="Trios", y="Count")

# Plot2: Number of genes for TRIO --------------------------------------#
newdf2 = lapply(1:nrow(df), function(counter, thedf){
    data.frame(
        ko     = c(thedf[counter,"KO1"],thedf[counter,"KO2"],thedf[counter,"KO3"]) %>% unlist %>% unname,
        counts = c(thedf[counter,"C1FPKM"], thedf[counter,"C2FPKM"], thedf[counter,"C3FPKM"]) %>% unlist %>% unname,
        group = counter,
        koid = c(1,2,3)
    )
}, thedf = df) %>% do.call(rbind,.)

    ggplot(newdf2, aes(as.factor(koid), counts, color=as.factor(group))) +
        geom_point()                                                     +
        geom_path(aes(group=group))                                      +
        theme(legend.position="none")                                    +
        labs(x="Trios", y="FPKM")
})
