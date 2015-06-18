\name{path2kingdom}
\alias{path2kingdom}
\title{List all intermediaries between taxa and the superkingdom it belongs to }

\usage{path2kingdom(taxID = "79255", ...)}
\arguments{
  \item{taxID}{
}
  \item{\dots}{
}
}



\author{uesu}




\examples{
#df = path2kingdom(taxID='79255')
#df2 = lapply(df$data[[1]], function(x) matrix(x, ncol=1))
#setNames(data.frame(cbind(df2[[1]],df2[[2]],df2[[3]])), df$columns)
}
