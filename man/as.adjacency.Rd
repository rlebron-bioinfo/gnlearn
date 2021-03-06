% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{as.adjacency}
\alias{as.adjacency}
\title{Convert Graph To Adjacency Matrix}
\usage{
as.adjacency(
  g,
  from = c("auto", "adjacency", "edges", "graph", "igraph", "bnlearn")
)
}
\arguments{
\item{g}{Graph object.}

\item{from}{Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'}
}
\description{
This function allows you to convert your graph to adjacency matrix format.
}
\examples{
g <- as.adjacency(g)
}
