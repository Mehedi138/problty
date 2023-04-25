#' cfexp
#' @export
cfexp <- function(t, rate = 1){
  cfgamma(t, shape = 1, scale = 1/rate)
}
