% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithms.R
\name{boot.parents.children}
\alias{boot.parents.children}
\title{Parents & Children Algorithm With Bootstrapping}
\usage{
boot.parents.children(
  df,
  whitelist = NULL,
  blacklist = NULL,
  test = ci.tests,
  alpha = 0.01,
  B = NULL,
  max.sx = NULL,
  version = c("mmpc", "si.hiton.pc", "hpc"),
  R = 200,
  m = NULL,
  threshold = 0.5,
  to = c("igraph", "adjacency", "edges", "graph", "bnlearn"),
  cluster = parallel::detectCores(),
  seed = sample(1:10^6, 1)
)
}
\arguments{
\item{df}{Dataset.}

\item{whitelist}{A data frame with two columns, containing a set of arcs to be included in the graph (optional).}

\item{blacklist}{A data frame with two columns, containing a set of arcs not to be included in the graph (optional).}

\item{test}{Conditional independence test to be used: 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'}

\item{alpha}{Target nominal type I error rate. Default: 0.01}

\item{B}{Number of permutations considered for each permutation test.}

\item{max.sx}{Maximum allowed size of the conditioning sets.}

\item{version}{Algorithm version: 'mmpc', 'si.hiton.pc', or 'hpc'. Default: 'mmpc'}

\item{R}{Number of bootstrap replicates (optional). Default: 200}

\item{m}{Size of training set (optional). Default: nrow(df)/2}

\item{threshold}{Minimum strength required for a coefficient to be included in the average adjacency matrix (optional). Default: 0.5}

\item{to}{Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn') (optional).}

\item{cluster}{A cluster object from package parallel or the number of cores to be used (optional). Default: parallel::detectCores()}

\item{seed}{Seed used for random selection. Default: NULL}
}
\description{
This function allows you to learn a directed graph from a dataset using Parents & Children algorithms.
}
\examples{
obj <- boot.parents.children(df)
avg.g <- obj$average
g.rep <- obj$replicates
}
\keyword{graph}
\keyword{learning}
