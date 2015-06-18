\name{findSeeds}
\alias{findSeeds}
\title{To find all seed compounds in the metabolic graph}
\description{Function finds strongly connected components within a bipartite metabolic network.
And selects for components with at least 1 outgoing edge and no inbound edges.}
\usage{findSeeds(mbgraph = grepgraph(top500kos))}
\arguments{
  \item{mbgraph}{the graph}
}

\value{Data.frame of source component, entites, linker connection to non-member KOs/CPDs in other components.}

\author{uesu}




\examples{
print("Still in progress")
#possibleSCC \%>\% group_by(membership) \%>\% summarise(sum(outbound), sum(inbound))
}
