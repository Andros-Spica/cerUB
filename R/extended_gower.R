#' Extended Gower with exceptions
#'
#' Calculate the extended Gower coefficient for archaeometric data sets,
#' considering exceptional values in ordinal variables, using a modified version
#' of \code{\link[ade4]{dist.ktab}}.
#'
#' @param data Data frame containing ordinal variables and optionally
#'             compositional data (log-ratios).
#' @param variable_sets List containing vectors with the numeric index of variables
#'                      of different kind.
#' @param method Character, "NI" for neighbour interexchange,
#'                        "RRD" for relative ranking difference,
#'                        "MM" for mixed mode.
#' @param exception_columns Numeric, the vector of variables names to be
#'                        searched for exceptions.
#' @param exception_values Numeric, the value to be set with distance.
#' @param exception_distance Numeric, the index of the variable to be used as
#'                           divisor in additive log-ratio transformation.
#'
#' @export
extended_gower <- function(data,
                           variable_sets,
                           method = "RRD",
                           exception_columns = NULL,
                           exception_values = NULL,
                           exception_distance = 0) {

  data_ <- data

  # set exception values as NAs
  excepColIndexes <- c()
  for (vn in exception_columns){
    if (vn %in% names(data_)){ # check if the variable is present
      excepColIndexes <- c(excepColIndexes, grep(vn, names(data_)))
    }
  }

  for (v in excepColIndexes){
    for (i in 1:nrow(data_)){
      if (!is.na(data_[i, v])){ # if it is not already a NA
        if (data_[i, v] == exception_values){ # if the value is the one to be considered exceptional, eg. "none"
          data_[i, v] <- NA
        }
      }
    }
  }

  ### Extended Gower with Exception

  data_ <- data.frame(data.matrix(data_))

  # prepare list separate variable sets
  list_variable_sets <- list()
  for (i in 1:length(variable_sets)){
    list_variable_sets[[i]] <- data_[,variable_sets[[i]]]
  }

  ktab1 <- ade4::ktab.list.df(list_variable_sets)

  # calculate distance
  if(method == "NI"){

    dist_matrix <- dist.ktab_cerUB(ktab1,
                                   variable_classes = c("O"),
                                   option = "scaledBYrange",
                                   scann = F,
                                   dist.excep = exception_distance,
                                   is_protocol2b = TRUE)

  } else if(method == "RRD"){

    dist_matrix <- dist.ktab_cerUB(ktab1,
                                   variable_classes = c("O"),
                                   option = "scaledBYrange",
                                   scann = F,
                                   dist.excep = exception_distance,
                                   is_protocol2b = FALSE)

    if (!ade4::is.euclid(dist_matrix)) {
      # Lingoes transformation (1971)
      dist_matrix <- ade4::lingoes(dist_matrix)
      print("Lingoes transformation done.")
    }

  } else if(method == "MM") {

    dist_matrix <- dist.ktab_cerUB(ktab1,
                                   variable_classes = c("O", "Q"),
                                   option = "scaledBYrange",
                                   scann = F,
                                   dist.excep = exception_distance,
                                   is_protocol2b = FALSE,
                                   weight = c(.5, .5))

    if (!ade4::is.euclid(dist_matrix)) {
      # Lingoes transformation (1971)
      dist_matrix_euc <- ade4::lingoes(dist_matrix)
      print("Lingoes transformation done.")
    }

  }

  return(dist_matrix)
}
