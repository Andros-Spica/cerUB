#' Principal Coordinates Analysis
#'
#' Apply Principal Coordinates Analysis to a given distance matrix,
#' calculate variable covariances, and the percent of variance explained by
#' 2D and 3D projections.
#'
#' @param distance_matrix distance or dissimilarity matrix
#' @param original_data data frame containing the original data
#' @param variable_tags Character, two-column data frame containing (1)
#'                      the names of variables and (2) their tags.
#' @param dimensions Numeric, number of dimensions of the projection equivalent to
#'                   k in \code{\link[stats]{cmdscale}}
#'
#' @export
pcoa <- function(distance_matrix,
                 original_data,
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

  print(paste(pcoa_obj$sub2D, "in 2D"))

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

    print(paste(pcoa_obj$sub3D, "in 3D"))
  }

  # calculate covariance axis vs. variables
  original_data_ranks <- data.frame(data.matrix(original_data))
  transrank <- function(u){ return(rank(u, na.last = "keep")) }
  original_data_ranks <- apply(original_data_ranks, 2, transrank)

  covmat <- cov(cbind(original_data_ranks, pcoa_obj$points),
                use = "pairwise.complete.obs")

  pcoa_obj$loadings <- covmat[1:ncol(original_data_ranks), (ncol(original_data_ranks) + 1):ncol(covmat)]

  varNames <- vector()
  for (i in (ncol(original_data_ranks) + 1):ncol(covmat)){
    varNames <- c(varNames, paste("PCo", i - ncol(original_data_ranks), sep = "-"))
  }

  dimnames(pcoa_obj$loadings)[[2]] <- varNames

  if (is.null(variable_tags)){

    vcod <- names(original_data)

  } else {

    vcod <- vector()

    for (i in 1:ncol(original_data)){

      index = match(names(original_data)[i], variable_tags[, 1])

      if (!is.na(index)) {

        vcod[i] <- variable_tags[, 2][index]

      } else {

        vcod[i] <- names(original_data)[i]

      }
    }
  }

  dimnames(pcoa_obj$loadings)[[1]] <- vcod

  pcoa_obj$variable_tags <- cbind(names(original_data), vcod)

  return(pcoa_obj)
}
