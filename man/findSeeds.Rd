\name{findSeeds}
\alias{findSeeds}
\title{To find all seed compounds in the metabolic graph}
\description{Function finds strongly connected components within a bipartite metabolic network.
And selects for components with at least 1 outgoing edge and no inbound edges.}
\usage{findSeeds(mbgraph = grepgraph(top500kos))}
\arguments{
  \item{mbgraph}{
}
}

\value{Data.frame of source component, entites, linker connection to non-member}

\author{uesu}




\examples{
...
#possibleSCC \%>\% group_by(membership) \%>\% summarise(sum(outbound), sum(inbound))
}
