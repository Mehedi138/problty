#' cfchisq
#' @export

cfchisq <- function(t, df, ncp = 0){
  if (df < 0 || ncp < 0  )
    stop("df and ncp must be nonnegative")
  exp(1i*ncp*t/(1-2i*t))/(1 - 2i*t)^(df/2)
}
