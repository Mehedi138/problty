#' genXdata
#' @export

####################################
# Functions to generate data
# for pedagogical purposes

#######################################################
# continuous X data

genXdata <- function(n, nvar = 1,
                     mu = rep(0, nvar),
                     Sigma = diag(length(mu)),
                     varnames = paste("x", 1:length(mu), sep = ""),
                     roundto = NULL
){
  tmp <- as.data.frame(MASS::mvrnorm(n, mu = mu, Sigma = Sigma))
  names(tmp) <- varnames
  if (!is.null(roundto)){
    tmp <- round(tmp, roundto)
  }
  tmp
}
