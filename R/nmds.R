#' Non-metric Multidimensional Scaling (NMDS)
#'
#' Apply Non-metric Multidimensional Scaling to a given distance matrix,
#' calculate variable covariances, and the percent of variance explained by
#' 2D and 3D projections.
#'
#' @param distance_matrix distance or dissimilarity matrix
#' @param original_data data frame containing the original data
#' @param variable_tags Character, two-column data frame containing (1)
#'                      the names of variables and (2) their tags.
#' @param dimensions Numeric, number of dimensions of the projection equivalent to
#'                   k in \code{\link[vegan]{metaMDS}}.
#' @param init_seed Numeric, the seed for the random number generator
#'                  used by \code{\link[vegan]{metaMDS}}.
#' @param trymax,autotransform Numeric, Maximum number of random starts in search of
#'                   stable solution. Logical, whether to use simple heuristics for
#'                   possible data transformation of typical community data (see below).
#'                   If you do not have community data, you should probably set
#'                   autotransform = FALSE.
#'                   Arguments passed to \code{\link[vegan]{metaMDS}}.
#'
#' @export
nmds<-function(distance_matrix,
               original_data,
               variable_tags = c(),
               dimensions = 2,
               init_seed = 0,
               trymax = 100,
               autotransform = FALSE) {

  set.seed(init_seed)

  nmds_obj <- vegan::metaMDS(distance_matrix,
                             k = dimensions,
                             trymax = trymax,
                             autotransform = autotransform)

  nmds_obj$init_seed <- init_seed

  nmds_obj$trymax <- trymax

  nmds_obj$sub_stress <- paste(as.character(100 * round(nmds_obj$stress, digits = 4)),
                               "% of stress", sep = "")

  print(nmds_obj$nmds$sub_stress)

  # variance explained by the first two dimensions found
  new_distance_matrix <- dist(nmds_obj$points[, 1:2], diag = TRUE, upper = TRUE)
  r <- cor(c(distance_matrix), c(new_distance_matrix))
  rSquared <- r * r

  nmds_obj$sub2D <- paste(as.character(100 * round(rSquared, digits = 4)),
                      "% of variance explained", sep = "")

  # F-test
  freedom = NROW(c(new_distance_matrix)) - 2
  Fvalue <- rSquared / ((1 - rSquared) / freedom)
  p_value <- pf(Fvalue, 1, freedom, lower.tail = FALSE)

  nmds_obj$GOF2_2D <- cbind(rSquared, Fvalue, p_value)

  print(paste(nmds_obj$sub2D, "in 2D"))

  if (dimensions > 2) {
    # variance explained by the first three dimensions found
    new_distance_matrix <- dist(nmds_obj$points[, 1:3], diag = TRUE, upper = TRUE)
    r <- cor(c(distance_matrix), c(new_distance_matrix))
    rSquared <- r * r

    nmds_obj$sub3D <- paste(as.character(100 * round(rSquared, digits = 4)),
                        "% of variance explained", sep = "")

    # F-test
    freedom = NROW(c(new_distance_matrix)) - 3
    Fvalue <- rSquared / ((1 - rSquared) / freedom)
    p_value <- pf(Fvalue, 1, freedom, lower.tail = FALSE)

    nmds_obj$GOF2_3D <- cbind(rSquared, Fvalue, p_value)

    print(paste(nmds_obj$sub3D, "in 3D"))
  }

  # calculate covariance axis vs. variables
  original_data_ranks <- data.frame(data.matrix(original_data))
  transrank <- function(u){
    return(rank(u, na.last = "keep"))
  }
  original_data_ranks <- apply(original_data_ranks, 2, transrank)

  covmat <- cov(cbind(original_data_ranks, nmds_obj$points), use = "complete.obs")

  nmds_obj$loadings <- covmat[1:ncol(original_data_ranks), (ncol(original_data_ranks) + 1):ncol(covmat)]

  varNames <- vector()

  for (i in (ncol(original_data_ranks) + 1):ncol(covmat)){
    varNames <- c(varNames, paste("MDS", i - ncol(original_data_ranks), sep = "-"))
  }

  dimnames(nmds_obj$loadings)[[2]] <- varNames

  if (is.null(variable_tags)) {

    vcod <- names(original_data)

  } else {

    vcod <- vector()

    for (i in 1:ncol(original_data)) {

      index = match(names(original_data)[i], variable_tags[, 1])

      if (!is.na(index)) {

        vcod[i] <- variable_tags[, 2][index]

      } else {

        vcod[i] <- names(original_data)[i]

      }
    }
  }

  dimnames(nmds_obj$loadings)[[1]] <- vcod

  nmds_obj$variable_tags <- cbind(names(original_data), vcod)

  return(nmds_obj)
}
