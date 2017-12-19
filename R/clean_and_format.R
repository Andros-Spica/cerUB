#' Clean and format data for cerUB protocols
#'
#' Cleaning and format procedures, including coercing variables as numeric or factor,
#' excluding columns (constants, perturbed, unreliable) and rows (incomplete data,
#' outliers).
#'
#' @param data Data frame, a data frame to be prepared for applying cerUB protocols.
#' @param categorical_columns Character/Numeric, vector with the names/indexes of the categorical variables.
#' @param numerical_columns Character/Numeric, vector with the names/indexes of the numeric variables.
#' @param completion_variable Character, vector with two elements (name, value) referencing the column that indicates wheter observations (rows) are completed. For instance, \code{c("isCompleted", "yes")}.
#' @param as_na Character, vector that specifies values to be considered as NA.
#' @param method Character, method to be used in for replacing NA, if any (\code{\link[cerUB]{replace_na}}).
#' @param columns_to_exclude Character/Numeric, vector with the names/indexes of columns to exclude.
#' @param rows_to_exclude Character/Numeric, vector with the names/indexes of rows to exclude.
#'
#' @examples
#'
#' \dontrun{
#'
#' dt <- data.frame("First" = c(1,2,2,3,5,1,6,0,4,10),
#'                  "Second" = c("A","A","A","A","A","A","A","A","A","A"),
#'                  "Third" = c("1","2","2","3","5","1","6","0","4","10"),
#'                  "Fourth" = c("A","B","C","D","E","F","G","H","I","J"),
#'                  "dummy" = c("bla","ble","bli","blo","blu","bla","ble",
#'                              "bli","blo","blu"),
#'                  "checked" = c("yes","yes","no","yes","no","yes","yes",
#'                                "no","yes","yes"))
#' row.names(dt) <- 1:10
#' dt_clean <- clean_and_format(dt,
#'                              categorical_columns = c("Second", "Fourth"),
#'                              numerical_columns = c("First", "Third"),
#'                              completion_variable = c("checked","yes"),
#'                              as_na = c("D"),
#'                              method = "random",
#'                              columns_to_exclude = c("dummy"),
#'                              rows_to_exclude = c(1, 10)
#'                  )
#'
#' }
#'
#' @export
clean_and_format <- function(data,
                             categorical_columns = NULL,
                             numerical_columns = NULL,
                             completion_variable = NULL,
                             as_na = NULL,
                             method = NULL,
                             columns_to_exclude = NULL,
                             rows_to_exclude = NULL) {

  # copy of original data
  tempdt <- data

  # format numeric data
  if (!is.null(numerical_columns)) {

    numColNames <- numerical_columns
    if (is.numeric(numerical_columns))
      numColNames <- names(data)[numerical_columns]

    if (all(numColNames %in% names(data))) {
      for (v in numColNames) {
        tempdt[, v] <- as.numeric(as.character(data[, v]))
      }
    } else {
      warning("Not all numerical variables were found. Please specify the exact name of numerical index of the columns in the data frame.")
    }
  }

  # format categorical data
  if (!is.null(categorical_columns)) {

    catColNames <- categorical_columns
    if (is.numeric(categorical_columns))
      catColNames <- names(data)[categorical_columns]

    if (all(catColNames %in% names(data))) {
      for (v in catColNames) {
        tempdt[, v] <- factor(data[, v])
      }
    } else {
      warning("Not all categorical variables were found. Please specify the exact name of numerical index of the columns in the data frame.")
    }
  }

  # filter incomplete characterization (only if the variable is present)
  if (!is.null(completion_variable)) {
    if (completion_variable[1] %in% names(data)) {
      tempdt <- subset(tempdt,
                       data[, completion_variable[1]] == completion_variable[2])
    } else {
      warning("Completion variable not recognized. The argument must be a vector with the name of the variable and the character value indicating completion.")
    }
  }

  # filter observations (rows)
  if (!is.null(rows_to_exclude)) {

    rowNamesToExclude <- rows_to_exclude

    if (is.numeric(rows_to_exclude))
      rowNamesToExclude <- row.names(data)[rows_to_exclude]

    if (length(rowNamesToExclude) > 0 &
        all(rowNamesToExclude %in% row.names(data))) {
      for (i in 1:length(rowNamesToExclude)){
        tempdt <- subset(tempdt,
                         row.names(tempdt) != rowNamesToExclude[i])
      }
    } else {
      warning("Not all rows to exclude were found. Please specify the exact name of numerical index of the columns in the data frame.")
    }
  }

  # replace Nas
  if (!is.null(as_na) | !is.null(method)) {
    ## categorical data
    if (!is.null(categorical_columns)) {

      catColNames <- categorical_columns
      if (is.numeric(categorical_columns))
        catColNames <- names(data)[categorical_columns]

      if (all(catColNames %in% names(data))) {
        for (v in catColNames) {
          if (nlevels(tempdt[, v]) > 1) {
            tempdt[, v] <- replace_na(tempdt[, v],
                                      as_na = as_na,
                                      method = method)
          }
        }
      }
    }

    ## numeric data
    if (!is.null(numerical_columns)) {

      numColNames <- numerical_columns
      if (is.numeric(numerical_columns))
        numColNames <- names(data)[numerical_columns]

      if (all(numColNames %in% names(data))) {
        for (v in numColNames){
          tempdt[, v] <- replace_na(tempdt[, v],
                                    method = method)
        }
      }
    }
  }

  # exclude variables in "columns_to_exclude"
  if (!is.null(columns_to_exclude)) {

    colNamesToExclude <- columns_to_exclude
    if (is.numeric(columns_to_exclude))
      colNamesToExclude <- names(data)[columns_to_exclude]

    if (all(colNamesToExclude %in% names(data))) {
      for (i in colNamesToExclude){
        tempdt <- tempdt[, names(tempdt) != i]
      }
    }
  }

  # filter constant variables
  cte <- sapply(tempdt,
                function(.col){ all(.col[1L] == .col, na.rm = T) } )
  tempdt <- tempdt[!cte]

  return(tempdt)
}
