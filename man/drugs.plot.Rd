% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{drugs.plot}
\alias{drugs.plot}
\title{Drug-Gene Interactions Plotting}
\usage{
drugs.plot(
  x,
  drugs,
  gene,
  neighbors = TRUE,
  from = c("auto", "adjacency", "edges", "graph", "igraph", "bnlearn"),
  interactive = FALSE
)
}
\arguments{
\item{x}{Graph object.}

\item{drugs}{Geneset with drug-gene interactions.}

\item{gene}{Gene whose drug interactions you want to explore.}

\item{neighbors}{Whether or not to draw neighboring genes. Default: TRUE}

\item{from}{Input format (optional).}

\item{interactive}{Interactive plot (optional). Default: FALSE}
}
\description{
This function allows you to visualize the interaction of a selected gene with drugs and other genes.
}
\examples{
drugs.plot(g, 3, 'Bax')
}
\keyword{drugs}
\keyword{genes}
\keyword{graph}
\keyword{plot}
