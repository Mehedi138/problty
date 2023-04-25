#' cards
#' @export

`cards` <- function (jokers = FALSE, makespace = FALSE){
  x <- c(2:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers) {
    levels(res$rank) <- c(levels(res$rank), "Joker")
    res <- rbind(res, data.frame(rank = c("Joker", "Joker"),
                                 suit = c(NA, NA)))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}
