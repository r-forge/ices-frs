`panel.pairs.cm` <-
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

