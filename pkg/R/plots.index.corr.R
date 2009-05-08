`plots.index.corr` <-
function(object, main = NULL, cex = 1, tol = 0.05, file = "")
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
  if (file != "")
  {
    png(file, width=2000, height=2000, pointsize=36)
      panel.pairs.cm (centre.log(cohort.mat), main = main, cex = cex*1.75, tol = tol)
    dev.off()
  } else
  {
    panel.pairs.cm (centre.log(cohort.mat), main = main, cex = cex, tol = tol)
  }
}

