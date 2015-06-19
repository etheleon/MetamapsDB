\name{make.data.frame}
\alias{make.data.frame}
\title{Function for dealing with dbquery outputs which have lists }
\description{Converts a data.frame where its columns exists as lists into a proper data.frame}
\usage{make.data.frame(df, string2factor = FALSE)}
\arguments{
  \item{df}{the data.frame output from dbquery}
  \item{string2factor}{
}
}



\author{uesu}




\examples{
    #output.df <- dbquery(
    # query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
    # params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    #cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
#make.data.frame(output.df)
}
