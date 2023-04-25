#' cfcauchy
#' @export

cfcauchy = function(t, location = 0, scale = 1){
  if (scale <= 0 )
    stop("scale must be positive")
  exp(1i*location*t - scale*abs(t))
}
