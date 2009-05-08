\name{centre.log}
\alias{centre.log}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{}
\description{
  takes the log of a real valued matrix, removes any columns where there are
  less than three finite values, and scales the values to lie between zero and 1.
}
\usage{
  centre.log(mat)
}
\arguments{
  \item{mat}{a numeric n by p matrix, with 0's and possibly with NAs}
}
\details{
  No details
}
\value{
  a numeric matrix of dimension n by q < p containing values between 0 and 1
}
\references{no references}
\author{Colin Millar}
\note{
   intended to be used within the plots.index.corr function
}
\seealso{\code{\link{plots.index.corr}}}
\examples{

## The function is currently defined as
function(mat)
{
  mat[mat<=0] <- NA
  mat <- log(mat)
  mat <- mat[,apply(mat,2,function(x) sum(!is.na(x))>2)]
  apply(mat,2,function(x) (x-min(x, na.rm=T))/diff(range(x,na.rm=T)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
