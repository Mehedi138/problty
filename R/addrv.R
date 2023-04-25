#' addrv
#' @export

`addrv` <- function (space, FUN = NULL, invars = NULL, name = NULL, ...){
  if (any(class(space) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  bnames <- names(space)[which(names(space) != "probs")]
  out <- subset(space, select = bnames)
  probs <- subset(space, select = probs)
  if (is.null(invars))
    invars <- bnames
  if (!is.character(invars))
    stop("vars should be a character vector")
  if (!is.null(FUN)) {
    if (is.null(name))
      name <- "X"
    temp <- apply(subset(space, select = invars), 1, FUN)
    val <- cbind(out, temp, probs)
    names(val) <- c(bnames, name, "probs")
  }
  else {
    val <- transform(out, ...)
    val$probs <- probs
  }
  return(val)
}

