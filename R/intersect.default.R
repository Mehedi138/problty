#' intersect.default
#' @export

`intersect.default` <- function (x, y, ...){
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0)])
}
