#' cfweibull
#' @export
cfweibull <- function(t, shape, scale = 1){
  if (shape <= 0 || scale <= 0)
    stop("shape and scale must be positive")
  fr <- function(x) cos(t*x)*dweibull(x, shape, scale)
  fi <- function(x) sin(t*x)*dweibull(x, shape, scale)
  Rp <- integrate(fr, lower = 0, upper = Inf)$value
  Ip <- integrate(fi, lower = 0, upper = Inf)$value
  return( Rp + 1i*Ip )
}
