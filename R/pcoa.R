#' Principal Coordinates Analysis
#'
#' Apply Principal Coordinates Analysis to a given distance matrix,
#' calculate variable covariances, and the percent of variance explained by
#' 2D and 3D projections.
#'
#' @param distance_matrix distance or dissimilarity matrix
#' @param dt data frame containing the original data
#' @param variable_tags list containing vectors with the numeric index of variables
#'                of different kind.
#' @param dimensions Numeric, number of dimensions of the projection equivalent to
#'                   k in \code{\link[stats]{cmdscale}}
#'
pcoa <- function(distance_matrix,
                 dt,
                 variable_tags = NULL,
                 dimensions = 2) {

  pcoa_obj <- cmdscale(distance_matrix,
                   k = dimensions,
                   eig = T)

  # variance explained by the first two dimensions found
  new_distance_matrix <- dist(pcoa_obj$points[, 1:2],
                              diag = TRUE,
                              upper = TRUE)

  r <- cor(c(distance_matrix), c(new_distance_matrix))
  r_squared <- r * r

  pcoa_obj$sub2D <- paste(as.character(100 * round(r_squared, digits = 4)),
                          "% of variance explained", sep = "")

  # F-test
  freedom = NROW(c(new_distance_matrix)) - 2
  f_value <- r_squared / ((1 - r_squared) / freedom)
  p_value <- pf(f_value, 1, freedom, lower.tail = FALSE)

  pcoa_obj$GOF2_2D <- cbind(r_squared, f_value, p_value)

  print(pcoa_obj$sub2D)
  print(pcoa_obj$GOF2_2D)

  if (dimensions > 2) {
    # variance explained by the first three dimensions found
    new_distance_matrix <- dist(pcoa_obj$points[, 1:3], diag = TRUE, upper = TRUE)
    r <- cor(c(distance_matrix), c(new_distance_matrix))
    r_squared <- r * r

    pcoa_obj$sub3D <- paste(as.character(100 * round(r_squared, digits = 4)),
                            "% of variance explained", sep = "")

    # F-test
    freedom = NROW(c(new_distance_matrix)) - 3
    f_value <- r_squared / ((1 - r_squared) / freedom)
    p_value <- pf(f_value, 1, freedom, lower.tail = FALSE)

    pcoa_obj$GOF2_3D <- cbind(r_squared, f_value, p_value)

    print(pcoa_obj$sub3D)
    print(pcoa_obj$GOF2_3D)
  }

  # calculate covariance axis vs. variables
  dt_ <- data.frame(data.matrix(dt))
  transrank <- function(u){ return(rank(u, na.last = "keep")) }
  dt_ <- apply(dt_, 2, transrank)

  covmat <- cov(cbind(dt_, pcoa_obj$points), use = "pairwise.complete.obs")

  pcoa_obj$loadings <- covmat[1:ncol(dt_), (ncol(dt_) + 1):ncol(covmat)]

  varNames <- vector()
  for (i in (ncol(dt_) + 1):ncol(covmat)){
    varNames <- c(varNames, paste("PCo", i - ncol(dt_), sep = "-"))
  }

  dimnames(pcoa_obj$loadings)[[2]] <- varNames

  if (is.null(variable_tags)){
    vcod <- names(dt_)
  } else {
    vcod <- vector()
    for (i in 1:ncol(dt_)){
      index = match(names(dt_)[i], variable_tags[, 1])
      if (!is.na(index)) {
        vcod[i] <- variable_tags[, 2][index]
      } else {
        vcod[i] <- names(dt_)[i]
      }
    }
  }

  dimnames(pcoa_obj$loadings)[[1]] <- vcod

  pcoa_obj$variable_tags <- cbind(names(dt_), vcod)

  return(pcoa_obj)
}
