#' cfnorm
#' @export
cfnorm <- function(t, mean = 0, sd = 1){
  if (sd <= 0)
    stop("sd must be positive")
  exp(1i*mean - (sd*t)^2/2)
}
