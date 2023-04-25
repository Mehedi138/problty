#' cfhyper
#' @export
cfhyper <- function(t, m, n, k){
  if (m < 0 || n < 0 || k < 0)
    stop("m, n, k must be positive")
  hypergeo::hypergeo(-k, -m, n - k + 1, exp(1i*t))/hypergeo::hypergeo(-k, -m, n - k + 1, 1)
}
