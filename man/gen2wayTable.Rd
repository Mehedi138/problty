% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen2wayTable.R
\name{gen2wayTable}
\alias{gen2wayTable}
\title{gen2wayTable}
\usage{
gen2wayTable(
  n = sample(100:500, size = 1),
  pmatrix = matrix(1:12, nrow = 3),
  dmnames = list(X = paste("x", 1:nrow(pmatrix), sep = ""), Y = paste("y",
    1:ncol(pmatrix), sep = "")),
  addmargins = TRUE,
  as.df = FALSE,
  untable = TRUE
)
}
\description{
gen2wayTable
}
