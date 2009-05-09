`centre.log` <-
function(mat)
{
  mat[mat<=0] <- NA
  mat <- log(mat)
  mat <- mat[,apply(mat,2,function(x) sum(!is.na(x))>2)]
  apply(mat,2,function(x) (x-min(x, na.rm=TRUE))/diff(range(x,na.rm=TRUE)))
}

