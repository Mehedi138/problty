#' cfnbinom
#' @export
cfnbinom <- function(t, size, prob, mu){
  if (size <= 0 )
    stop("size must be positive")
  if (prob <= 0 || prob > 1)
    stop("prob must be in (0,1]")
  if (!missing(mu)) {
    if (!missing(prob))
      stop("'prob' and 'mu' both specified")
    prob <- size/(size+mu)
  }
  (prob/(1-(1-prob)*exp(1i*t)))^size
}
