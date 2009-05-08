\name{main.panel.cm}
\alias{main.panel.cm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
main.panel.cm(x, y, tol = 0.05, cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{tol}{ ~~Describe \code{tol} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
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
function(x, y, tol = 0.05, cex = 1)
{
  # panel for upper off diagonal panels
  
  grid.points(x, y, pch = 16, gp=gpar(col=grey(0.35), cex=0.2*cex))

  # fit a linear model to the data
  lm1 <- lm(y ~ x)
  x1 <- 0:20/20
  fit <- suppressWarnings(predict.lm(lm1, newdata=data.frame(x=x1), se.fit=T))
  y1 <- fit$fit
  yu <- y1 + 2*fit$se
  yl <- y1 - 2*fit$se

  sig <- identical(anova(lm1)$"Pr(>F)"[1] < tol, T)

  if (sig) {
    line.f <- list(lwd=3*cex, lty=1, col="#000000")
    line.ci <- list(lwd=2*cex, lty=1, col="red4")
  } else {
    line.f <- list(lwd=cex, lty=1, col="#0000FF")
    line.ci <- list(lwd=cex, lty=2, col="#0000FF")
  }

  panel.lines(x1,y1, lwd=line.f$lwd, lty=line.f$lty, col=line.f$col)
  panel.lines(x1,yu, lwd=line.ci$lwd, lty=line.ci$lty, col=line.ci$col)
  panel.lines(x1,yl, lwd=line.ci$lwd, lty=line.ci$lty, col=line.ci$col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
