% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{fit.coefficients}
\alias{fit.coefficients}
\title{Estimate The Coefficients Of An Adjacency Matrix}
\usage{
fit.coefficients(
  g,
  df,
  to = c("igraph", "adjacency", "edges", "graph", "bnlearn"),
  from = c("auto", "adjacency", "edges", "graph", "igraph", "bnlearn"),
  cluster = parallel::detectCores()
)
}
\arguments{
\item{g}{Graph object.}

\item{df}{Dataset.}

\item{to}{Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'}

\item{from}{Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'}

\item{cluster}{A cluster object from package parallel or the number of cores to be used (optional). Default: parallel::detectCores()}
}
\description{
This function allows you to estimate the coefficients of the adjacency matrix of a previously learned graph.
}
\examples{
g <- fit.coefficients(g, df)
}
\keyword{coefficients}
\keyword{fit}
\keyword{graph}
