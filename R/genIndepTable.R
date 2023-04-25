#' genIndepTable
#' @export


#params <- c(1,2,3,4)
#require(MASS)
#xmean <- Null(params)[ , 1]
#X = genXdata(10, mu = xmean, roundto = 2)
#genLogRegData(X, beta = params)


######################################################3
# contingency tables

genIndepTable <- function(n = sample(100:500, size = 1),
                          prow = 1:3, pcol = 1:4,
                          dmnames = list(X = paste("x", 1:length(prow), sep = ""),
                                         Y = paste("y", 1:length(pcol), sep = "")),
                          addmargins = TRUE,
                          as.df = FALSE, untable = TRUE){
  prow <- prow/sum(prow)
  pcol <- pcol/sum(pcol)
  pmatrix <- outer(prow, pcol)
  probs <- as.numeric(pmatrix)
  x <- factor(sample(1:length(probs), size = n, replace = TRUE, prob = probs),
              levels = 1:length(probs))
  tmp <- matrix(as.integer(table(x)), nrow = length(prow))
  dimnames(tmp) <- dmnames
  tmp <- as.table(tmp)

  if (as.df){
    tmp <- as.data.frame(tmp)
    if (untable){
      tmp <- with(tmp, reshape::untable(tmp, Freq))
      tmp[ , "Freq"] <- NULL
      rownames(tmp) <- 1:dim(tmp)[1]
    }
    tmp
  } else if (addmargins) {
    addmargins(tmp)
  } else {
    tmp
  }
}
