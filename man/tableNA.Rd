% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableNA.R
\name{tableNA}
\alias{tableNA}
\title{Wrapper around table function}
\usage{
tableNA(...)
}
\arguments{
\item{...}{arguments passes to \code{\link[base]{table}}}
}
\description{
Wrapper around table function
}
\details{
Run the \code{\link[base]{table}} function on \code{...}, while also
passing the argument \code{useNA = 'ifany'}
}
\examples{
tableNA(c(1, 2, NA, 2, NA, 2, 1, 3))
tableNA(c(1, 2, 2, 2, 1, 3))

}
