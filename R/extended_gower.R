#' Extended Gower with exceptions
#'
#' Calculate the extended Gower coefficient for archaeometric data sets,
#' considering exceptional values in ordinal variables.
#'
#' @param dt data frame containing ordinal variables and optionally
#'           compositional data (log-ratios).
#' @param var_sets list containing vectors with the numeric index of variables
#'                of different kind.
#' @param type Character, "NI" for neighbour interexchange,
#'                        "RRD" for relative ranking difference,
#'                        "MM" for mixed mode.
#' @param excep_col Numeric, the vector of variables names to be searched for exceptions
#' @param excep_val Numeric, the value to be set with distance
#' @param excep_dist Numeric, the index of the variable to be used as divisor
#'                   in additive log-ratio transformation.
#'
extended_gower <- function(dt,
                           var_sets,
                           type = "RRD",
                           excep_col = NULL,
                           excep_val = NULL,
                           excep_dist = 0) {

  dt_ <- dt

  # set exception values as NAs
  excepColIndexes <- c()
  for (vn in excep_col){
    if (vn %in% names(dt_)){ # check if the variable is present
      excepColIndexes <- c(excepColIndexes, grep(vn, names(dt_)))
    }
  }

  for (v in excepColIndexes){
    for (i in 1:nrow(dt_)){
      if (!is.na(dt_[i, v])){ # if it is not already a NA
        if (dt_[i, v] == excep_val){ # if the value is the one to be considered exceptional, eg. "none"
          dt_[i, v] <- NA
        }
      }
    }
  }

  ### Extended Gower with Exception

  dt_ <- data.frame(data.matrix(dt_))

  # prepare list separate variable sets
  list_var_sets <- list()
  for (i in 1:length(var_sets)){
    list_var_sets[[i]] <- data[,var_sets[[i]]]
  }
  ktab1 <- ade4::ktab.list.df(list_var_sets)

  # calculate distance
  if(type == "NI"){

    dist_matrix <- CAMOTECCER_dist.ktab(ktab1,
                                        type = c("O"),
                                        option = "scaledBYrange",
                                        scann = F,
                                        dist.excep = excep_dist,
                                        type = "Protocol_2")

  } else if(type=="RRD"){

    dist_matrix <- CAMOTECCER_dist.ktab(ktab1,
                                        type = c("O"),
                                        option = "scaledBYrange",
                                        scann = F,
                                        dist.excep = excep_dist,
                                        type = "Protocol_3")

    if (!is.euclid(dist_matrix)) {
      # Lingoes transformation (1971)
      dist_matrix <- lingoes(dist_matrix)
      print("Lingoes transformation done.")
    }

  } else if(type == "MM") {

    dist_matrix <- CAMOTECCER_dist.ktab(ktab1,
                                        type=c("O", "Q"),
                                        option = "scaledBYrange",
                                        scann = F,
                                        dist.excep = excep_dist,
                                        type = "Protocol_4",
                                        weight = c(.5, .5))

    if (!is.euclid(dist_matrix)) {
      # Lingoes transformation (1971)
      dist_matrix_euc <- lingoes(dist_matrix)
      print("Lingoes transformation done.")
    }

  }

  return(dist_matrix)
}
