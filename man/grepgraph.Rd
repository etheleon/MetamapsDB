\name{grepgraph}
\alias{grepgraph}
\title{Generates metabolic network as IGRAPH object from input vector KOs}
\description{Takes in a vector list of KO ids and generates a metabolic graph as a igraph object}
\usage{grepgraph(kos, ...)}
\arguments{
  \item{kos}{this will take the input KOs}
  \item{\dots}{
}
}



\author{Wesley Goi
uesu}




\examples{
data(top500kos)
mb_graph<-grepgraph(top500kos)
}
