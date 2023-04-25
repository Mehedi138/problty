#' cfwilcox
#' @export
cfwilcox <- function(t, m, n){
  sum(exp(1i*t*0:(m*n)) * dwilcox(0:(m*n), m, n))
}
