% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{download.dataset}
\alias{download.dataset}
\title{Download A Dataset Via RESTful API}
\usage{
download.dataset(code, log = TRUE, host = HOST)
}
\arguments{
\item{code}{Download Code (indicated by list.datasets() output).}

\item{log}{Whether or not to apply log(x+1) (optional). Default: TRUE}
}
\description{
This function allows you to download a dataset available via RESTful API.
}
\examples{
df <- download.dataset (1)
}
\keyword{api}
\keyword{datasets}
