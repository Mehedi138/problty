#' cfgeom
#' @export

cfgeom <- function(t, prob){
  cfnbinom(t, size = 1, prob = prob)
}
