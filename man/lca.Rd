\name{lca}
\alias{lca}
\title{Finds the lowest common ancestor}

\usage{lca(taxon1 = "10090", taxon2 = "9096", recurse = TRUE, ...)}
\arguments{
  \item{taxon1}{The first taxon}
  \item{taxon2}{The second taxon}
  \item{recurse}{F if comparing just two}
  \item{\dots}{
}
}



\author{uesu}




\examples{
lcaTaxID <- lca(taxon1="10090", taxon2='9096', recurse=FALSE);
df <- lca(taxon1="10090", taxon2='9096', recurse=FALSE);

#recursion when you have 3 or more taxIDs
taxa = c(314146,9598,10090)
lcaTaxID <- Reduce(f=lca, x=taxa, init=taxa[1])
}
