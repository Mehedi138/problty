#' binom
#' @export

cfbinom <- function(t, size, prob){
  if (size <= 0 )
    stop("size must be positive")
  if (prob < 0 || prob > 1)
    stop("prob must be in [0,1]")
  (prob*exp(1i*t) + (1 - prob))^size
}
