% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_groups.R
\name{test_groups}
\alias{test_groups}
\title{Perform Group separation and uniformity tests}
\usage{
test_groups(distMatrix, groups)
}
\arguments{
\item{distMatrix}{Numeric, distance matrix}

\item{groups}{Factor, vector containing the assignation of each observation
in the matrix to a specific group.}
}
\description{
Perform four tests (anosim, betadisper, permdisp2, and permanova) that assess
the separation and uniformity of the given group factor. Additionally, creates
a generative text with the results of PERMANOVA and PERMDISP2 test results.
}
