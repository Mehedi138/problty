#' cflogis
#' @export
cflogis <- function(t, location = 0, scale = 1){
  if (scale <= 0)
    stop("scale must be positive")
  ifelse( identical(all.equal(t, 0), TRUE),
          return(1),
          return(exp(1i*location)*pi*scale*t/sinh(pi*scale*t)))
}
