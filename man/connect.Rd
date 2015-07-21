\name{connect}
\alias{connect}
\title{Settings for connecting to neo4j database}
\description{Function sets parameters such as the REST API URL, username and passwords}
\usage{connect(url = "192.168.100.1", port = 7474)}
\arguments{
  \item{url}{URL hosting the neo4j database}
  \item{port}{Port}
}



\author{uesu}




\examples{
connect("localhost", 7474)
}
