#' cflnorm
#' @export
cflnorm <- function(t, meanlog = 0, sdlog = 1){
  if (sdlog <= 0)
    stop("sdlog must be positive")
  if (identical(all.equal(t, 0), TRUE)){
    return(1+0i)
  } else {
    t <- t * exp(meanlog)
    Rp1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * cos(y)/y, lower = 0, upper = t )$value
    Rp2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * cos(1/y)/y, lower = 0, upper = 1/t )$value
    Ip1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * sin(y)/y, lower = 0, upper = t )$value
    Ip2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * sin(1/y)/y, lower = 0, upper = 1/t )$value
    return((Rp1 + Rp2 + 1i*(Ip1 + Ip2))/(sqrt(2*pi) * sdlog))
  }
}
