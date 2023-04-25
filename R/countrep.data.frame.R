#' countrep.data.frame
#' @export
`countrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = countrep, ...)
}
