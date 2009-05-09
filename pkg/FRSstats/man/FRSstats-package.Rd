\name{FRSstats-package}
\alias{FRSstats-package}
\alias{FRSstats}
\docType{FRSstats}
\title{Fisheries Research Services Statistics Group R Package}
\description{
The point in this package is to combine a set of functions to help out in a
stock assessment working group
}
\details{
\tabular{ll}{
Package: \tab icesfrs\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-05-08\cr
License: \tab GNU \cr
LazyLoad: \tab yes\cr
}
The package can be used with raw data matrices or used with FLR objects
}
\author{Colin Millar \email{millarc@marlab.ac.uk}}

\examples{
# simple example of how to use plots.index.corr
numbers <- matrix(runif(n=8*15), ncol=8, nrow=15)
plots.index.corr(numbers)
}
