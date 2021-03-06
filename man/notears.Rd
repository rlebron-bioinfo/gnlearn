% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithms.R
\name{notears}
\alias{notears}
\title{Linear NO-TEARS Algorithm (Reimplemented)}
\usage{
notears(
  df,
  lambda1 = 0.1,
  loss.type = c("l2", "logistic", "poisson"),
  max.iter = 100,
  h.tol = 1e-06,
  rho.max = 1e+06,
  w.threshold = 0.1,
  m = NULL,
  to = c("igraph", "adjacency", "edges", "graph", "bnlearn"),
  seed = sample(1:10^6, 1)
)
}
\arguments{
\item{df}{Dataset.}

\item{lambda1}{L1 regularization parameter. Default: 0.1}

\item{loss.type}{Type of loss function to be used: 'l2', 'logistic', or 'poisson'. Default: 'l2'}

\item{max.iter}{Maximum number of dual ascent steps. Default: 100}

\item{h.tol}{Minimum absolute value of h. Default: 1e-8}

\item{rho.max}{Maximum value of rho. Default: 1e+16}

\item{w.threshold}{Threshold of absolute value of weight. Default: 0.1}

\item{m}{Size of training set (optional). Default: nrow(df)/2}

\item{to}{Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn') (optional).}
}
\description{
This function allows you to learn an adjacency matrix from a dataset using the Linear NO-TEARS algorithm.
}
\examples{
g <- notears(df)
}
\keyword{adjacency}
\keyword{learning}
