\name{connect}
\alias{connect}
\title{Settings for connecting to neo4j database}
\description{Function sets parameters such as the REST API URL, username and passwords
In the newer versions of neo4j default requires username and password}
\usage{connect(url = "192.168.100.1", port = 7474, username = "neo4j", 
    password = "neo4j")}
\arguments{
  \item{url}{URL hosting the neo4j database}
  \item{port}{Port}
  \item{username}{authorization username and password}
  \item{password}{authorization username and password}
}



\author{uesu}




\examples{
connect("localhost", 7474, 'username', 'password')
}
