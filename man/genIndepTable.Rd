% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genIndepTable.R
\name{genIndepTable}
\alias{genIndepTable}
\title{genIndepTable}
\usage{
genIndepTable(
  n = sample(100:500, size = 1),
  prow = 1:3,
  pcol = 1:4,
  dmnames = list(X = paste("x", 1:length(prow), sep = ""), Y = paste("y", 1:length(pcol),
    sep = "")),
  addmargins = TRUE,
  as.df = FALSE,
  untable = TRUE
)
}
\description{
genIndepTable
}
