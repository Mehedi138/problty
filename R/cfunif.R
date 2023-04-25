#' cfunif
#' @export
cfunif <- function(t, min = 0, max = 1){
  if (max < min)
    stop("min cannot be greater than max")
  ifelse( identical(all.equal(t, 0), TRUE),
          1+0i,
          (exp(1i*t*max) - exp(1i*t*min))/(1i*t*(max - min)))
}
