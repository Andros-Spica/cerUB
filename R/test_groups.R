#' Perform Group separation and uniformity tests
#'
#' Perform four tests (anosim, betadisper, permdisp2, and permanova) that assess
#' the separation and uniformity of the given group factor. Additionally, creates
#' a generative text with the results of PERMANOVA and PERMDISP2 test results.
#'
#' @param distMatrix Numeric, distance matrix
#' @param groups Factor, vector containing the assignation of each observation
#'               in the matrix to a specific group.
#'
#' @export
test_groups <- function(distMatrix, groups){

  print("initiating test batch...")

  test_anosim <- vegan::anosim(distMatrix, groups)

  print("vegan::anosim done.")

  test_betadisp <- vegan::betadisper(distMatrix, groups)

  print("vegan::betadisper done.")

  test_permdisp2 <- vegan::permutest(test_betadisp, pairwise = TRUE)

  print("vegan::permutest done.")

  test_permanova <- vegan::adonis(distMatrix ~ groups)

  print("vegan::adonis done.")

  x <- NULL
  x$permanova <- test_permanova
  x$betadisp <- test_betadisp
  x$permdisp2 <- test_permdisp2
  x$anosim <- test_anosim

  x$text <- function(tests){

    permanova.F = as.character(round(tests$permanova$aov.tab$F.Model[1], 3))
    permanova.p_value = as.character(round(tests$permanova$aov.tab$"Pr(>F)"[1], 3))
    permanova.rSquared = as.character(round(tests$permanova$aov.tab$R2[1], 3))
    permdisp2.F = as.character(round(tests$permdisp2$tab$F[1], 3))
    permdisp2.p_value = as.character(round(tests$permdisp2$tab$"Pr(>F)"[1], 3))
    text <- list(paste("PERMANOVA:\n   F = ", permanova.F,
                       " (p = ", permanova.p_value, ")", sep = ""),
                 c(expression(paste("   ", R^2, " = ", sep = "")),
                   paste("            ", permanova.rSquared, sep = "")),
                 paste("PERMDISP2:\n   F = ", permdisp2.F,
                       " (p = ", permdisp2.p_value,")", sep = ""))

    return(text)
  }

  print("Test batch completed.")

  return(x)
}
