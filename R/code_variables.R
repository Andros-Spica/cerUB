#' Codify petrographic variable names
#'
#' Creates a code for each variable in a petrographic data frame
#' using cerUB naming system, and return a two-column data frame
#' relating original names with their codes.
#'
#' @param data A petrographic data frame
#' using cerUB naming system.
#'
#' @export
code_variables <- function(data) {

  # replace variables names with codes
  nI = 1
  nF = 1
  nV = 1
  nL = 1
  nS = 1

  # create code
  varCode <- vector()

  for (i in 1:ncol(data)) {

    if (names(data)[i] == "INCLUS_DISTRIB" |
        names(data)[i] == "INCLUS_ORIENT") {

      varCode[i] <- paste("I", nI, sep = "")
      nI = nI + 1

    } else if (names(data)[i] == "TEMP" |
               names(data)[i] == "ATM" | names(data)[i] == "POST_ATM") {

      varCode[i] <- paste("F", nF, sep = "")
      nF = nF + 1

    } else if (stringr::str_detect(names(data)[i], "VOID_")) {

      varCode[i] <- paste("V", nV, sep = "")
      nV = nV + 1

    } else if (stringr::str_detect(names(data)[i], "COAR_")) {

      varCode[i] <- paste("L", nL, sep = "")
      nL = nL + 1

    } else if (stringr::str_detect(names(data)[i], "FINE_")) {

      varCode[i] <- paste("S", nS, sep = "")
      nS = nS + 1

    } else {

      varCode[i] <- names(data)[i]

    }
  }

  varCodes <- cbind("Variable name" = names(data), "Variable code" = varCode)
  return(varCodes)
}
