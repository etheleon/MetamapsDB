\name{sigmaGraph}
\alias{sigmaGraph}
\title{Takes mbgraph and outputs as a shinyApp Obj}
\description{Users should encode the values into the igraph object
Works with older version of IGraph, maybe someone can choose to upgrade this further}
\usage{sigmaGraph(igraphObj)}
\arguments{
  \item{igraphObj}{graph object after running prettifyGraph}
}



\author{uesu}




\examples{
data(nitrogenGraph)
app = nitrogenMetab \%>\% sigmaGraph
#app
}
