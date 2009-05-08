\name{diag.panel.cm}
\alias{diag.panel.cm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
diag.panel.cm(varname = NULL, cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varname}{ ~~Describe \code{varname} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (varname = NULL, cex = 1, ...)
{
  grid.text(varname, gp = gpar(col = "black", cex = cex))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

