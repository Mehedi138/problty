#' iidspace
#' @export

`iidspace` <- function (x, ntrials, probs = NULL){
  temp = list()
  for (i in 1:ntrials) {
    temp[[i]] <- x
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  if (is.null(probs)) {
    res$probs <- rep(1/dim(res)[1], dim(res)[1])
  }
  else {
    if (!identical(length(x), length(probs))) {
      stop("'probs' is not the same length as 'outcomes'")
    }
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
    ptemp = list()
    for (i in 1:ntrials) {
      ptemp[[i]] <- probs
    }
    pdf <- expand.grid(ptemp, KEEP.OUT.ATTRS = FALSE)
    pres <- apply(pdf, 1, prod)
    res$probs <- pres
  }
  names(res) <- c(paste(rep("X", ntrials), 1:ntrials, sep = ""),
                  "probs")
  return(res)
}
