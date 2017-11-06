#' Principal Components Analysis for compositional data
#'
#' Principal Components Analysis (PCA) of compositional data
#' after applying log-ratio transformation.
#'
#' @param dt Data frame containing compositional data
#' @param transformation_method Character, the log-ratio transformation to be applied.
#'                       "ALR" -> additive log-ratio,
#'                       "CLR" -> centered log-ratio,
#'                       "ILR" -> isometric log-ratio.
#'                       Additionally, accepts "log" for applying
#'                       logarithmic transformation and "std" for standardization
#'                       (scaled and centred).
#' @param method Character, "standard" for standard PCA, "robust" for robust PCA.
#' @param init_seed Numeric, the seed for the random number generator used in
#'                  \code{\link{best_pcaCoDa}}).
#' @param samples Numeric, the number of iterations applying to samples in
#'                \code{\link{best_pcaCoDa}}) and
#'                maxiter in \code{\link[pcaPP]{PCAgrid}}).
#' @param alr_base Character/Numeric, the name/index of the variable to be used
#'                 as divisor in additional log-ratio transformation.
#'
#' @export
princomp_coda <- function(dt,
                          transformation_method = "ILR",
                          method = "robust",
                          init_seed = 0,
                          samples = 100,
                          alr_base = 1) {
  transformed_data <- NULL
  pca_return <- NULL

  if (transformation_method == "ILR") {
    transformed_data <- robCompositions::pivotCoord(dt)

    if (method == "robust" && nrow(dt) > ncol(dt)) {
      pca_return <- best_pcaCoDa(dt,
                                 method = "robust",
                                 init_seed =  init_seed,
                                 samples = samples)

    } else {
      if (method == "robust")
        warning(
          "n <= number of components.
          Robust PCA cannot be performed,
          standard method applied instead."
        )

      pca_return <- best_pcaCoDa(dt,
                                 method = "standard",
                                 init_seed =  init_seed,
                                 samples = samples)

    }

  } else

    if (transformation_method == "CLR") {
      transformed_data <- robCompositions::cenLR(dt)

      if (method == "standard") {
        pca_return <- princomp(transformed_data$x.clr)

      } else {
        dimensions <- ncol(transformed_data$x.clr)

        pca_return <- pcaPP::PCAgrid(transformed_data$x.clr,
                                     k = dimensions,
                                     maxiter = samples)

      }

    } else

      if (transformation_method == "ALR") {
        alr_base_ <- alr_base

        if (is.character(alr_base))
          alr_base_ <- match(alr_base,
                             names(dt))

        if (is.na(alr_base_))
          stop("ERROR: the alr_base given is not present in the data.")

        transformed_data <- robCompositions::addLR(dt,
                               ivar = alr_base_)

        if (method == "standard") {
          pca_return <- princomp(transformed_data$x.alr)


        } else {
          dimensions <- ncol(transformed_data$x.alr)

          pca_return <- pcaPP::PCAgrid(transformed_data$x.alr,
                                       k = dimensions,
                                       maxiter = samples)

        }

        pca_return$base <- names(dt)[alr_base_]

      } else

        if (transformation_method == "log") {

          transformed_data <- data.frame(log(dt))
          names(transformed_data) <-
            paste("log-", names(dt), sep = "")

          if (method == "standard") {
            pca_return <- princomp(transformed_data)

          } else {
            dimensions <- ncol(transformed_data)

            pca_return <- pcaPP::PCAgrid(transformed_data,
                                         k = dimensions,
                                         maxiter = samples)

          }

        } else

          if (transformation_method == "std") {

            transformed_data <- data.frame(scale(dt))
            names(transformed_data) <-
              paste("std-", names(dt), sep = "")

            if (method == "standard") {
              pca_return <- princomp(transformed_data)

            } else {
              dimensions <- ncol(transformed_data)

              pca_return <- pcaPP::PCAgrid(transformed_data,
                                           k = dimensions,
                                           maxiter = samples)
            }

          } else {
            stop(
              "The transformation_method selected is not valid. Please select one of the following: 'ALR', 'CLR', 'ILR', 'log', 'std'."
            )

          }

  pca_var <-
    cumsum((pca_return$sdev) ^ 2) / sum(pca_return$sdev ^ 2)
  if (transformation_method == "ILR")
    pca_var <- cumsum((pca_return$princompOutputClr$sdev) ^ 2) / sum(pca_return$princompOutputClr$sdev ^ 2)

  pca_return$sub2D <-
    paste(as.character(100 * round(pca_var[2], digits = 4)),
          "% of variance explained",
          sep = "")

  pca_return$sub3D <-
    paste(as.character(100 * round(pca_var[3], digits = 4)),
          "% of variance explained",
          sep = "")

  print(paste(pca_return$sub2D, "in 2D"))
  print(paste(pca_return$sub3D, "in 3D"))

  pca_return$transformation_method <- transformation_method

  pca_return$method <- method

  pca_return$transformed_data <- transformed_data

  if (transformation_method == "CLR")
    transformed_data <- transformed_data$x.clr

  if (transformation_method == "ALR")
    transformed_data <- transformed_data$x.alr

  pca_return$dist_matrix <- dist(transformed_data)

  return(pca_return)

}

#' Find the best 'pcaCoDa' (robCompositions)
#'
#' Searches the best projection given by \code{\link[robCompositions]{pcaCoDa}}.
#'
#' @param dt Data frame containing compositional data
#' @param method Character, "standard" for standard PCA, "robust" for robust PCA.
#' @param init_seed Numeric, the seed for the random number generator used in
#'                  \code{\link{best_pcaCoDa}}).
#' @param samples Numeric, the number of iterations applying to samples in
#'                \code{\link{best_pcaCoDa}}) and
#'                maxiter in \code{\link[pcaPP]{PCAgrid}}).
#'
#' @export
best_pcaCoDa <- function(dt,
                         method = "robust",
                         init_seed = 0,
                         samples = 100) {

  seed_list <- list()

  pca_list <- list()

  pca_2sd <- vector()

  pca_3sd <- vector()

  set.seed(init_seed)

  for (i in 1:samples) {

    seed_list[[i]] <- setRNG::getRNG()

    # pcaCoDa() takes the ***original data*** as input
    pca <- robCompositions::pcaCoDa(dt,
                                    method = method)

    pca_list[[i]] <- pca

    pca_var <- cumsum((pca$princompOutputClr$sdev) ^ 2) / sum(pca$princompOutputClr$sdev ^ 2)

    pca_2sd[i] <- pca_var[2]
    pca_3sd[i] <- pca_var[3]

  }

  pca_best_index <- match(max(pca_2sd),
                          pca_2sd)

  pca_return <- pca_list[[pca_best_index]]

  pca_return$seed <- seed_list[[pca_best_index]]

  pca_return$init_seed <- init_seed

  pca_return$samples <- samples

  # pca_return$sub2D <- paste(as.character(100 * round(pca_2sd[pca_best_index], digits=4)),
  #                           "% of variance explained",
  #                           sep = "")
  #
  # pca_return$sub3D <- paste(as.character(100 * round(pca_3sd[pca_best_index], digits=4)),
  #                           "% of variance explained",
  #                           sep = "")

  return(pca_return)

}
