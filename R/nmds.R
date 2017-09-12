#' Non-metric Multidimensional Scaling (NMDS)
#'
#' Apply Non-metric Multidimensional Scaling to a given distance matrix,
#' calculate variable covariances, and the percent of variance explained by
#' 2D and 3D projections.
#'
#' @param distance_matrix distance or dissimilarity matrix
#' @param dt data frame containing the original data
#' @param variable_tags Character, two-column data frame containing (1)
#'                      the names of variables and (2) their tags.
#' @param dimensions Numeric, number of dimensions of the projection equivalent to
#'                   k in \code{\link[vegan]{metaMDS}}
#'
nmds<-function(distance_matrix,
               dt,
               variable_tags = c(),
               dimensions = 2,
               trymax = 100,
               init_seed = 0,
               autotransform = FALSE) {

  setRNG::set.seed(init_seed)

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
  r <- cor(c(dist), c(new_distance_matrix))
  rSquared <- r * r

  nmds_obj$sub2D <- paste(as.character(100 * round(rSquared, digits = 4)),
                      "% of variance explained", sep = "")

  # F-test
  freedom = NROW(c(new_distance_matrix)) - 2
  Fvalue <- rSquared / ((1 - rSquared) / freedom)
  p_value <- pf(Fvalue, 1, freedom, lower.tail = FALSE)

  nmds_obj$GOF2_2D <- cbind(rSquared, Fvalue, p_value)

  print(nmds_obj$sub2D)
  print(nmds_obj$GOF2_2D)

  if (dimensions > 2) {
    # variance explained by the first three dimensions found
    new_distance_matrix <- dist(nmds_obj$points[, 1:3], diag = TRUE, upper = TRUE)
    r <- cor(c(dist), c(new_distance_matrix))
    rSquared <- r * r

    nmds_obj$sub3D <- paste(as.character(100 * round(rSquared, digits = 4)),
                        "% of variance explained", sep = "")

    # F-test
    freedom = NROW(c(new_distance_matrix)) - 3
    Fvalue <- rSquared / ((1 - rSquared) / freedom)
    p_value <- pf(Fvalue, 1, freedom, lower.tail = FALSE)

    nmds_obj$GOF2_3D <- cbind(rSquared, Fvalue, p_value)

    print(nmds_obj$sub3D)
    print(nmds_obj$GOF2_3D)
  }

  # calculate covariance axis vs. variables
  dt<-data.frame(data.matrix(dt))
  transrank <- function(u){
    return(rank(u, na.last = "keep"))
  }
  dt <- apply(dt, 2, transrank)

  covmat <- cov(cbind(dt,nmds_obj$points), use = "complete.obs")

  nmds_obj$loadings <- covmat[1:ncol(dt), (ncol(dt) + 1):ncol(covmat)]

  varNames <- vector()

  for (i in (ncol(dt) + 1):ncol(covmat)){
    varNames <- c(varNames, paste("MDS", i - ncol(dt), sep = "-"))
  }

  dimnames(nmds_obj$loadings)[[2]] <- varNames

  if (is.null(variable_tags)) {

    vcod <- names(dt)

  } else {

    vcod <- vector()

    for (i in 1:ncol(dt)) {

      index = match(names(dt)[i], variable_tags[,1])

      if (!is.na(index)) {

        vcod[i] <- variable_tags[,2][index]

      } else {

        vcod[i] <- names(dt)[i]

      }
    }
  }

  dimnames(nmds_obj$loadings)[[1]] <- vcod

  nmds_obj$variable_tags <- cbind(names(dt), vcod)

  return(nmds_obj)
}
