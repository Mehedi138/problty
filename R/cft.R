#' cft
#' @export
cft <- function(t, df, ncp){
  if(missing(ncp)) ncp <- 0
  if (df <= 0)
    stop("df must be positive")
  if (identical(all.equal(ncp, 0), TRUE)){
    ifelse(identical(all.equal(t, 0), TRUE), 1+0i,
           as.complex(besselK(sqrt(df)*abs(t), df/2)*(sqrt(df)*abs(t))^(df/2)/( gamma(df/2) * 2^(df/2 - 1) ))
    )
  } else {
    fr <- function(x) cos(t*x)*dt(x, df, ncp)
    fi <- function(x) sin(t*x)*dt(x, df, ncp)
    Rp <- integrate(fr, lower = -Inf, upper = Inf)$value
    Ip <- integrate(fi, lower = -Inf, upper = Inf)$value
    return(Rp + 1i*Ip)
  }
}
