#' Transform compositional data
#'
#' Transform compositional data in a given data frame and replace it.
#'
#' @param data Data frame containing compositional data.
#' @param coda_variables Numeric/Character, vector containing the names/indexes
#'                       of the compositional variables.
#' @param method Character, the log-ratio transformation to be applied.
#'                       "ALR" -> additive log-ratio,
#'                       "CLR" -> centered log-ratio,
#'                       "ILR" -> isometric log-ratio.
#'                       Additionally, accepts "log" for applying
#'                       logarithmic transformation and "std" for standardization.
#' @param alr_base Character/Numeric, the name/index of the variable to be used
#'                 as divisor in additional log-ratio transformation.
#'                 in additive log-ratio transformation.
#' @param raw_filename,trans_filename,final_filename Character, file names for
#'                 saving the raw (complete), tranformed (only coda), and
#'                 final (complete) data sets.
#'
#'
transform_coda <- function(data,
                           coda_variables,
                           method = c("CLR"),
                           alr_base = 1,
                           raw_filename = NULL,
                           trans_filename = NULL,
                           final_filename = NULL) {

  data_chem <- c()
  for (i in coda_variables) {
    data_chem <- cbind(data_chem, data[, names(data) == i])
  }
  data_chem <- data.frame(data_chem)
  row.names(data_chem) <- row.names(data)
  names(data_chem) <- coda_variables

  # closure (rows to constant sum)
  data_chem <- robCompositions::constSum(data_chem, const = 100)

  if (is.null(raw_filename) == F) {
    write.csv(data_chem, file = raw_filename, row.names = TRUE)
  }

  data_chem_trans_all <- matrix(, nrow = nrow(data), ncol = 0)

  if (!is.null(method)) {

    for (trans in method) {

      if (is.null(trans) == F) {

        if (trans == "std") {

          data_chem_trans <- data.frame(scale(data_chem))
          names(data_chem_trans) <-
            paste("std-", names(data_chem), sep = "")

        }
        if (trans == "log") {

          data_chem_trans <- data.frame(log(data_chem))
          names(data_chem_trans) <-
            paste("log-", names(data_chem), sep = "")

        }
        if (trans == "ALR") {

          data_chem_trans <- robCompositions::addLR(data_chem, alr_base)
          data_chem_trans <- data.frame(data_chem_trans$x.alr)
          dimnames(data_chem_trans) <-
            paste("ALR-", names(data_chem_trans), sep = "")

        }
        if (trans == "CLR") {

          data_chem_trans <- robCompositions::cenLR(data_chem)
          data_chem_trans <- data.frame(data_chem_trans$x.clr)
          names(data_chem_trans) <-
            paste("CLR-", names(data_chem_trans), sep = "")

        }
        if (trans == "ILR") {

          data_chem_trans <- data.frame(robCompositions::isomLR(data_chem))
          names(data_chem_trans) <-
            gsub("X", "ILR-", names(data_chem_trans))

        }

        data_chem_trans_all <-
          cbind(data_chem_trans_all, data_chem_trans)

        if (is.null(trans_filename) == F) {
          write.csv(data_chem_trans,
                    file = trans_filename,
                    row.names = TRUE)
        }
      }
    }

    data <- data[,!(names(data) %in% names(data_chem))]
    data <- cbind(data, data_chem, data_chem_trans_all)

  } else {

    data <- data[,!(names(data) %in% names(data_chem))]
    data <- cbind(data, data_chem)

  }

  if (is.null(final_filename) == F) {
    write.csv(data, file = final_filename, row.names = TRUE)
  }

  return(data)

}

buildOrthBasis <- function(x) {
  V <- matrix(0, nrow = ncol(x), ncol = ncol(x) - 1)
  for (i in 1:ncol(V)) {
    V[1:i, i] <- 1 / i
    V[i + 1, i] <- (-1)
    V[, i] <- V[, i] * sqrt(i / (i + 1))
  }
  return(V)
}
