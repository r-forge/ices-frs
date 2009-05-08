\name{panel.pairs.cm}
\alias{panel.pairs.cm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
panel.pairs.cm(z, cex = 1, main = "", tol = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
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
function (z, cex = 1, main = "", tol = 0.05)
{
  grid.newpage()
  pushViewport(
    viewport(name = "base",
             x = .025, y = .025,
             w = .95,  h = .95,
             just = c("left", "bottom")) )

  n.var <- ncol(z)

  if (n.var == 0)
      return()

  lim <- vector("list", length = n.var)

  prepanel.limits <-
  function (x)
  {
    range(x,finite = TRUE) + 0.07*diff(range(x, finite = TRUE))*c(-1,1)
  }

  for (i in seq_len(n.var)) lim [[i]] <- prepanel.limits(z[, i])

  splom.layout <- grid.layout(nrow = n.var, ncol = n.var)
  pushViewport(
    viewport( layout = splom.layout, name = "pairs" )
  )
  for (i in 1:n.var)
  {
    for (j in 1:n.var)
    {
      pushViewport(
        viewport(
          layout.pos.row = n.var - i + 1,
          layout.pos.col = j,
          name = paste("subpanel", j, i, sep = "."),
          clip = TRUE,
          xscale = if (is.character( lim [[j]] ))
                     c(0, length( lim [[j]] ) + 1)
                   else lim [[j]],
          yscale = if (is.character( lim [[i]] ))
                     c(0, length( lim [[i]] ) + 1)
                   else lim [[i]]
        )
      )
      if (i == j)
      {
        diag.panel.cm(varname = colnames(z)[i], cex=cex)
      } else
      {
        if (i > j)
        {
          main.panel.cm(x = z[, j], y = z[,i], tol = tol, cex = cex)
          grid.rect(gp = gpar(lwd = cex, col=grey(0.5), fill="transparent"))
        }
      }
      upViewport()
    }
  }
  upViewport()
  grid.text(main, x=0.5, y=0.1, default.units="npc", gp = gpar(cex = cex*1.2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
