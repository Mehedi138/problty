#' empirical
#' @export

`empirical` <- function (x){
  if (any(class(x) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(x))
    message("'x' must be a data frame")
  temp <- x
  n <- dim(temp)[1]
  vars <- names(temp)
  temp$probs <- rep(1, n)/n
  return(marginal(temp))
}
