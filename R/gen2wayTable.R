#'gen2wayTable
#' @export

#
# genIndepTable(n = 100)
# genIndepTable(n = 100, nfixed = TRUE)
# genIndepTable(n = 100, nfixed = TRUE, as.df = TRUE)
# genIndepTable(n = 100, nfixed = TRUE, as.df = TRUE, untable = FALSE)
#
# tmp = genIndepTable(n = 10, nfixed = TRUE, as.df = TRUE)
# tmp
#
# model.matrix(~., data = tmp)
# tmp2 = as.data.frame(model.matrix(~ X*Y, data = tmp))
# tmp2
#
# genLogRegData(tmp2)
#
# A = genIndepTable(n = 500, nfixed = TRUE, as.df = TRUE)
# chisq.test(xtabs(~., data = A))
#


######################################################3
# general two-way tables

gen2wayTable <- function(n = sample(100:500, size = 1),
                         pmatrix = matrix(1:12, nrow = 3),
                         dmnames = list(X = paste("x", 1:nrow(pmatrix), sep = ""),
                                        Y = paste("y", 1:ncol(pmatrix), sep = "")),
                         addmargins = TRUE,
                         as.df = FALSE, untable = TRUE){
  probs <- as.numeric(pmatrix)
  x <- factor(sample(1:length(probs), size = n, replace = TRUE, prob = probs),
              levels = 1:length(probs))
  tmp <- matrix(as.integer(table(x)), nrow = nrow(pmatrix))
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






#
# gen2wayTable(n = 100)
# gen2wayTable(n = 100, nfixed = TRUE)
# gen2wayTable(n = 100, nfixed = TRUE, as.df = TRUE)
# gen2wayTable(n = 100, nfixed = TRUE, as.df = TRUE, untable = FALSE)
#
# w = matrix(c(8, 5, 3, 2, 5, 5), nrow = 2)
#
# B = gen2wayTable(n = 300, pmatrix = w, addmargins = FALSE)
# chisq.test(B)
#

#
#
# `nsamp` <- function (n, k, replace = FALSE, ordered = FALSE){
#     m <- length(n)
#     if (length(k) != m)
#         stop("number of urns doesn't equal number of sample sizes")
#     if (length(replace) != m) {
#         replace <- rep(replace, length.out = m)
#     }
#     if (length(ordered) != m) {
#         ordered <- rep(ordered, length.out = m)
#     }
#     res <- c()
#     for (i in 1:m) if (isTRUE(replace[i])) {
#         if (isTRUE(ordered[i])) {
#             res[i] <- n[i]^k[i]
#         }
#         else {
#             res[i] <- choose(n[i] - 1 + k[i], k[i])
#         }
#     }
#     else {
#         if (isTRUE(ordered[i])) {
#             res[i] <- factorial(n[i])/factorial(n[i] - k[i])
#         }
#         else {
#             res[i] <- choose(n[i], k[i])
#         }
#     }
#     return(res)
# }
#
#
#
# `permsn` <- function (x, m)
# {
#
#     # require(combinat)
#     if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)
#
#         x <- seq(x)
#     temp <- combn(x, m)
#     if ( isTRUE(all.equal(m,1)) ) {
#
#         P <- temp
#     } else if (isTRUE(all.equal(m, length(x)))) {
#
#         temp <- matrix(x, ncol = 1)
#         P <- array(unlist(permn(temp[, 1])), dim = c(m, factorial(m)))
#     } else {
#         k <- dim(temp)[1]
#         n <- dim(temp)[2]
#         P <- array(unlist(permn(temp[, 1])), dim = c(k, factorial(k)))
#         for (i in 2:n) {
#             a <- temp[, i]
#             perms <- array(unlist(permn(a)), dim = c(k, factorial(k)))
#             P <- cbind(P, perms)
#         }
#
#
#     }
#     return(P)
# }
