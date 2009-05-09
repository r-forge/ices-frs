\name{plots.index.corr}
\alias{plots.index.corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plots.index.corr(object, main = NULL, cex = 1, tol = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ ~~Describe \code{object} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{tol}{ ~~Describe \code{tol} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ 
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object, main = NULL, cex = 1, tol = 0.05)
{
  tune.mat <- object
  if (is.null(main)) main <- ""
  # make cohort matrix
  n <- dim(tune.mat)[2]
  cohort.mat <- matrix(NA, ncol=n, nrow=dim(tune.mat)[1]+n-1)
  colnames(cohort.mat) <- colnames(tune.mat)
  for (j in 1:n) {
	  cohort.mat[,j] <- c(rep(NA,n-j),tune.mat[,j],rep(NA,j-1))
  }
  panel.pairs.cm (centre.log(cohort.mat), main = main, cex = cex, tol = tol)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
