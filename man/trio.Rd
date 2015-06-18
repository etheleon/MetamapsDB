\name{trio}
\alias{trio}
\title{#<< Function to find trios.}

\usage{trio(KOI = ko:K00001, ...)}
\arguments{
  \item{KOI}{name of the KO with ko: prefix}
  \item{\dots}{
}
}



\author{uesu}




\examples{
    df <- trio(ko:K00001)

# Plot1: Number of genes for TRIO --------------------------------------#
newdf = lapply(1:nrow(df), function(counter, thedf){
    data.frame(
        ko     = c(thedf[counter,"KO1"],thedf[counter,"KO2"],thedf[counter,"KO3"]) \%>\% unlist \%>\% unname,
        counts = c(thedf[counter,"KO1count"], thedf[counter,"KO2count"], thedf[counter,"KO3count"]) \%>\% unlist \%>\% unname,
        group = counter,
        koid = c(1,2,3)
    )
}, thedf = df) \%>\% do.call(rbind,.)

    ggplot(newdf, aes(as.factor(koid), counts, color=as.factor(group))) +
        geom_point()                                                    +
        geom_path(aes(group=group))                                     +
        theme(legend.position="none")                                   +
        labs(x="Trios", y="Count")

# Plot2: Number of genes for TRIO --------------------------------------#
newdf2 = lapply(1:nrow(df), function(counter, thedf){
    data.frame(
        ko     = c(thedf[counter,"KO1"],thedf[counter,"KO2"],thedf[counter,"KO3"]) \%>\% unlist \%>\% unname,
        counts = c(thedf[counter,"C1FPKM"], thedf[counter,"C2FPKM"], thedf[counter,"C3FPKM"]) \%>\% unlist \%>\% unname,
        group = counter,
        koid = c(1,2,3)
    )
}, thedf = df) \%>\% do.call(rbind,.)

    ggplot(newdf2, aes(as.factor(koid), counts, color=as.factor(group))) +
        geom_point()                                                     +
        geom_path(aes(group=group))                                      +
        theme(legend.position="none")                                    +
        labs(x="Trios", y="FPKM")
}
