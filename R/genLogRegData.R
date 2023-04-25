#' genLogRegData
#' @export

# genXdata(10, nvar = 3, roundto = 2)
# X = genXdata(10, nvar = 3, roundto = 2)

#######################################################
# logistic regression data

genLogRegData <- function(xdata,
                          beta = rep(1, ncol(xdata)),
                          yname = "y"){
  tmp <- as.matrix(xdata) %*% beta
  probs <- exp(tmp)/(1 + exp(tmp))
  y <- apply(probs, 1, function(p){rbinom(1, size = 1, prob = p)})
  resdata <- cbind(xdata, y)
  as.data.frame(resdata, col.names = c(names(xdata), yname))
}
