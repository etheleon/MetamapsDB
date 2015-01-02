\name{make.data.frame}
\alias{make.data.frame}
\title{Function for dealing with dbquery outputs which have lists }

\usage{make.data.frame(df)}
\arguments{
  \item{df}{the data.frame output from dbquery}
}



\author{uesu}




\examples{
    output.df <- dbquery(
     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
     params = FALSE, 
    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
make.data.frame(output.df)
}
