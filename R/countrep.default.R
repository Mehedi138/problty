#' countrep.default
#' @export
`countrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- 0
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- res + 1
      }
    }
  }
  return(res)
}
