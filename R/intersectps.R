#' intersect.ps
#' @export

`intersect.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(intersect(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}
