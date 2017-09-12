#' Clean and format data for cerUB protocols
#'
#' Cleaning and format procedures, including coercing variables as numeric or factor, excluding columns (incomplete data, constant) and rows ().
#'
#' @param data Data frame, a data frame to be prepared for applying cerUB protocols.
#' @param categorical_columns Character/Numeric, vector with the names/indexes of the categorical variables.
#' @param numerical_columns Character/Numeric, vector with the names/indexes of the numeric variables.
#' @param completion_variable Character, vector with two elements (name, value) referencing the column that indicates wheter observations (rows) are completed.
#' @param as_na Character, vector that specifies values to be considered as NA.
#' @param method Character, method to be used in for replacing NA, if any (\code{\link[cerUB]{replace_na}}).
#' @param columns_to_exclude Character/Numeric, vector with the names/indexes of columns to exclude.
#' @param rows_to_exclude Character/Numeric, vector with the names/indexes of rows to exclude.
#'
#' @examples
#'
#' \dontrun{
#'
#' dt <- cbind("First" = c(1,2,2,3,5,1,6,0,4,10),
#'             "Second" = c("A","A","A","A","A","A","A","A","A","A"),
#'             "Third" = c("1","2","2","3","5","1","6","0","4","10"),
#'             "Fourth" = c("A","B","C","D","E","F","G","H","I","J"),
#'             "dummy" = c("bla","ble","bli","blo","blu","bla","ble",
#'                         "bli","blo","blu"),
#'             "checked" = c("yes","yes","no","yes","no","yes","yes",
#'                           "no","yes","yes"))
#' row.names(dt) <- 1:10
#' claen_and_format(dt,
#'                  categorical_columns = c("Second", "Fourth"),
#'                  numerical_columns = c("First", "Third"),
#'                  completion_variable = "checked",
#'                  as_na = c("0","D"),
#'                  method = "random",
#'                  columns_to_exclude = c("dummy"),
#'                  rows_to_exclude = c(1, 10)
#'                  )
#'
#' }
#'
clean_and_format <- function(data,
                             categorical_columns = c(),
                             numerical_columns = c(),
                             completion_variable = c("CHARAC", "complete"),
                             as_na = c(NULL),
                             method = "random",
                             columns_to_exclude = c("variableNameToExclude"),
                             rows_to_exclude = c("outlierObservation")) {

  # "data" is a data frame
  # "complete" is the boolean indicating if output needs to have the petrological characterization complete

  # format numeric data
  for (v in numerical_columns) {
    data[, v] <- as.numeric(as.character(data[, v]))
  }

  # format categorical data
  for (v in categorical_columns) {
    data[, v] <- as.factor(data[, v])
  }

  # filter incomplete characterization (only if the variable is present)
  if(!is.null(data[, completion_variable[1]])){
    data <- subset(data,
                   data[, completion_variable[1]] == completion_variable[2])
  }

  # filter observations (rows)
  for (i in 1:length(rows_to_exclude)){
    data <- subset(data,
                   row.names(data) != rows_to_exclude[i])
  }

  # replace Nas
  ## categorical data
  for (v in categorical_columns){
    data[, v] <- replace_na(data[, v],
                            as_na = as_na,
                            method = method)
  }

  ## numeric data
  for (v in numerical_columns){
    data[, v] <- replace_na(data[, v],
                            as_na = as_na,
                            method = method)
  }

  # exclude variables in "columns_to_exclude"
  for (i in columns_to_exclude){
    data <- data[, names(data) != i]
  }

  # filter constant variables
  cte <- sapply(data,
                function(.col){ all(.col[1L] == .col, na.rm = T) } )
  data <- data[!cte]

  return(data)
}
