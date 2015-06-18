\name{listquery}
\alias{listquery}
\title{Function for querying metamaps DBj}
\description{Takes variable queries with table output ie. no nodes no rels (simpler version of DBquery gives a list)}
\usage{listquery(query, params = FALSE, cypherurl = "192.168.100.1:7474/db/data/cypher", 
    ...)}
\arguments{
  \item{query}{Cypher Query, rmbr to escape double quotes if any}
  \item{params}{a list object. If no parameters, let params = False.}
  \item{cypherurl}{
}
  \item{\dots}{
}
}



\author{uesu}




\examples{
#    output.df <- dbquery(
#     query = "START ko=node:koid('ko:\"ko:K00020\"') return ko.ko,ko.definition",
#     params = FALSE, 
#    #cypherurl = "metamaps.scelse.nus.edu.sg:7474/db/data/cypher")
#    cypherurl = "192.168.100.1:7474/db/data/cypher")    #internal within the server
}
