#' cfgamma
#' @export

cfgamma <- function(t, shape, rate = 1, scale = 1/rate){
  if (rate <= 0  || scale <= 0)
    stop("rate must be positive")
  (1-scale*1i*t)^(-shape)
}
