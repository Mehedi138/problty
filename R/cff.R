#' cff
#' @export

cff <- function(t, df1, df2, ncp, kmax = 10){
  if (df1 <= 0 || df2 <= 0  )
    stop("df1 and df2 must be positive")
  # require(fAsianOptions)
  if( identical(all.equal(ncp, 0), TRUE) ){
    gamma((df1+df2)/2) / gamma(df2/2) * kummerU(-1i*df2*t/df1, df1/2, 1 - df2/2)
  } else {
    exp(-ncp/2)*sum((ncp/2)^(0:kmax)/factorial(0:kmax)* kummerM(-1i*df2*t/df1, df1/2 + 0:kmax, -df2/2))
  }
}
