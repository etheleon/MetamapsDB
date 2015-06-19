\name{prettifyGraph}
\alias{prettifyGraph}
\title{Plots a standard metabolic graph}
\description{Adds details into the igraph object}
\usage{prettifyGraph(g, vsize = c(1, 2), vcolor = c("grey", "red"), 
    withText = TRUE, vtext = c(0.5, 1), layout = FALSE, ...)}
\arguments{
  \item{g}{GraphObject}
  \item{vsize}{the vertex size}
  \item{vcolor}{length 2 vector for KO and compounds}
  \item{withText}{Conditional to include labels in plot}
  \item{vtext}{two vector text value}
  \item{layout}{Whether to calculate the layout}
  \item{\dots}{
}
}



\author{uesu}




\examples{
#data(top500kos)
#g= grepgraph(top500kos)
#g = prettifyGraph(g)
}
