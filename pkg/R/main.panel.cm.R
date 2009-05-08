`main.panel.cm` <-
function(x, y, tol = 0.05, cex = 1)
{
  # panel for upper off diagonal panels
  
  grid.points(x, y, pch = 16, gp=gpar(col=grey(0.35), cex=0.2*cex))

  # fit a linear model to the data
  lm1 <- lm(y ~ x)
  x1 <- 0:20/20
  fit <- suppressWarnings(predict.lm(lm1, newdata=data.frame(x=x1), se.fit=TRUE))
  y1 <- fit$fit
  yu <- y1 + 2*fit$se
  yl <- y1 - 2*fit$se

  sig <- identical(anova(lm1)$"Pr(>F)"[1] < tol, TRUE)

  if (sig) {
    line.f <- list(lwd=3*cex, lty=1, col="#000000")
    line.ci <- list(lwd=2*cex, lty=1, col="red4")
  } else {
    line.f <- list(lwd=cex, lty=1, col="#0000FF")
    line.ci <- list(lwd=cex, lty=2, col="#0000FF")
  }

  grid.lines(x1, y1, gp = gpar(lwd=line.f$lwd, lty=line.f$lty, col=line.f$col))
  grid.lines(x1, yu, gp = gpar(lwd=line.ci$lwd, lty=line.ci$lty, col=line.ci$col))
  grid.lines(x1, yl, gp = gpar(lwd=line.ci$lwd, lty=line.ci$lty, col=line.ci$col))
}

