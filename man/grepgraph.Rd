\name{grepgraph}
\alias{grepgraph}
\title{Generates metabolic network as IGRAPH object from input vector KOs}
\description{Takes in a vector list of KO ids and generates a metabolic graph as a igraph object}
\usage{grepgraph(kos, fullGraph = FALSE, ...)}
\arguments{
  \item{kos}{this will take the input KOs}
  \item{fullGraph}{to output the whole of metabolism or not}
  \item{\dots}{
}
}



\author{uesu}




\examples{
#    data(top500kos)
#    mbgraph<-grepgraph(top500kos)
#    p1 = plot(
#    mbgraph, 
#    vertex.label=V(g)$name, 
#    vertex.size = 1,
#    edge.arrow.size=0.1,
#    vertex.frame.color="#FFFFFF00", 
#    vertex.color=c("grey","red")

#)
}
