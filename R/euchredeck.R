#' euchredeck
#' @export
`euchredeck` <- function (benny = FALSE, makespace = FALSE){
  x <- c(9:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(value = x, suit = y)
  if (benny) {
    levels(res$value) <- c(levels(res$value), "Joker")
    res <- rbind(res, data.frame(value = c("Joker"), suit = NA))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}
