#' Get indexes of petrographic variables
#'
#' Get indexes of petrographic variables that use the cerUB naming system.
#'
#' @param data Data frame containing petrographic variables that use the
#'             cerUB naming system.
#'
get_petro <- function(data) {
  indexVector <- c()

  for (i in 1:ncol(data)) {
    if (names(data)[i] == "CLAY" ||
        names(data)[i] == "TEMPER" ||
        names(data)[i] == "INCLUSIONS" ||
        names(data)[i] == "CLAY_MIX" ||
        stringr::str_detect(names(data), "INCLUS_")[i] == T ||
        names(data)[i] == "TEMP" ||
        names(data)[i] == "ATM" ||
        names(data)[i] == "POST_ATM" ||
        stringr::str_detect(names(data), "VOID_")[i] == T ||
        stringr::str_detect(names(data), "COAR_")[i] == T ||
        stringr::str_detect(names(data), "FINE_")[i] == T) {
      indexVector <- c(indexVector, i)
    }
  }

  return(indexVector)
}

#' Get indexes of compositional variables
#'
#' Get indexes of compositional variables that were transformed using
#' \code{\link[cerUB]{transform_coda}}.
#'
#' @param data Data frame containing petrographic variables that use the
#'             cerUB naming system.
#' @param coda_variables Character/Numeric, vector with the names/indexes of the compositional variables.
#' @param transformation_method Character, vector specifying a transformation:
#'                                         "ALR" -> additive log-ratio,
#'                                         "CLR" -> centered log-ratio,
#'                                         "ILR" -> isometric log-ratio.
#'                                         Additionally, accepts "log" for applying
#'                                         logarithmic transformation.
#'
get_coda <- function(data, coda_variables, transformation_method = "") {

  indexVector <- c()

  if (is.numeric(coda_variables))
    coda_variables <- names(data[, coda_variables])

  for (i in 1:ncol(data)) {
    for (j in 1:length(geochemVars)) {
      if (names(data)[i] == paste(transformation_method,
                                  coda_variables[j], sep = "-") ||
          names(data)[i] == paste(transformation_method,
                                  coda_variables[j], sep = "")) {
        indexVector <- c(indexVector, i)
      }
    }
  }

  return(indexVector)
}

#' Get indexes of provenance-related variables
#'
#' Get indexes of the compositional and petrographic variables indicative
#' of provenance. Petrographic variables must use the cerUB naming system.
#' Compositional variables must have been transformed using
#' \code{\link[cerUB]{transform_coda}}.
#'
#' @param data Data frame containing petrographic variables that use the
#'             cerUB naming system.
#' @param coda_variables Character/Numeric, vector with the names/indexes of
#'             the compositional variables.
#' @param transformation_method Character, vector specifying a transformation:
#'                                         "ALR" -> additive log-ratio,
#'                                         "CLR" -> centered log-ratio,
#'                                         "ILR" -> isometric log-ratio.
#'                                         Additionally, accepts "log" for applying
#'                                         logarithmic transformation.
#'
#' @return List of two numeric vectors containing the column indexes of (1)
#' petrographic and (2) compositional variables.
#'
get_provenance <- function(data, coda_variables, transformation_method = "") {

  indexVectorOrdinal <- c()
  indexVectorContinuos <- c()

  for (i in 1:ncol(data)) {
    # check petro vars
    if (names(data)[i] == "COAR_ROUNDNESS" ||
        names(data)[i] == "COAR_FORM" ||
        names(data)[i] == "FINE_FORM" ||
        stringr::str_detect(names(data), "COAR_R")[i] == T ||
        stringr::str_detect(names(data), "COAR_C")[i] == T ||
        stringr::str_detect(names(data), "FINE_C")[i] == T) {
      indexVectorOrdinal <- c(indexVectorOrdinal, i)
    } else {
      # check geochem vars
      if (is.numeric(coda_variables))
        coda_variables <- names(data[, coda_variables])

      for (j in 1:length(coda_variables)) {
        if (names(data)[i] == paste(transformation_method,
                                    geochemVars[j], sep = "-") ||
            names(data)[i] == paste(transformation_method,
                                    geochemVars[j], sep = "")) {
          indexVectorContinuos <- c(indexVectorContinuos, i)
        }
      }
    }
  }

  indexVector <- list(indexVectorOrdinal, indexVectorContinuos)
  return(indexVector)
}
