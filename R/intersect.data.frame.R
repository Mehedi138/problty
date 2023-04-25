#' intersect.data.frame
#' @export

`intersect.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(intersect(a, b), a), ]
}
