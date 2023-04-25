#' cfpois
#' @export
cfpois <- function(t, lambda){
  if (lambda <= 0)
    stop("lambda must be positive")
  exp(lambda*(exp(1i*t) - 1))
}
