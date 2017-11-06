#' Apply ordination procedures for multivariate statistical analysis
#'
#'#' Applies a given ordination procedure to a data set
#' and returns a ordination object.
#'
#' @param data Data frame, including compositional and petrographic data.
#' @param protocol Character, cerUB protocol to be applied.
#'                            "1": Analysis of compositional data;
#'                            "2a": Analysis of petrographic data (relative ranking difference);
#'                            "2b": Analysis of petrographic data (neighbor interchange);
#'                            "3": Analysis of compositional data and petrographic data
#'                            (relative ranking);
#'                            "4": Analysis of compositional data and petrographic data
#'                            (relative ranking) to characterize provenance.
#' @param dimensions Numeric, number of dimensions of the ordination object.
#' @param exception_columns Numeric, the vector of variables names to be
#'                          searched for exceptions.
#' @param variable_tags Character, two-column data frame containing (1)
#'                      the names of variables and (2) their tags.
#' @param coda_override Character, vector with the names of the
#'                      compositional variables.
#' @param coda_transformation_method Character, the log-ratio transformation
#'                            to be applied:
#'                       "ALR" for additive log-ratio,
#'                       "CLR" for centered log-ratio,
#'                       "ILR" for isometric log-ratio.
#'                       Additionally, accepts "log" for applying
#'                       logarithmic transformation and "std" for standardization
#'                       (scaled and centred).
#' @param coda_alr_base Character/Numeric, the name/index of the variable
#'                      to be used as divisor in additional log-ratio
#'                      transformation.
#' @param coda_pca_method Character, Principal Components Analysis (PCA) method:
#'                       "standard" for standard PCA, "robust" for robust PCA.
#' @param init_seed,coda_samples Numeric, arguments passed to
#'                       \code{\link[cerUB]{princomp_coda}}.
#'
#' @return Ordination object containing the projection of observations (scores)
#' and variables (loadings) in 'n' dimensions, the distance matrix used
#' (dist_matrix), and an approximation of the fitness of projections.
#'
#' @export
#'
apply_ordination <- function(data,
                             protocol = "1",
                             dimensions = 2,
                             exception_columns = NULL,
                             variable_tags = NULL,
                             coda_override = NULL,
                             coda_transformation_method = "CLR",
                             coda_alr_base = 1,
                             coda_pca_method = "robust",
                             init_seed = 0,
                             coda_samples = 100) {

  if (protocol == "1") {
    #######################################################################
    # Protocol 1 - Analysis of compositional data
    # recommended: ilr -> Euclidean -> robPCA
    # (alternatively, any transformation)
    ########################################################################

    coda_vars <- 1:ncol(data)
    if (!is.null(coda_override))
      coda_vars <- coda_override

    prot1 <- princomp_coda(data[, coda_vars],
                           transformation_method = coda_transformation_method,
                           method = coda_pca_method,
                           init_seed = init_seed,
                           samples = coda_samples,
                           alr_base = coda_alr_base
    )

    prot1$name <- "Protocol 1"

    print("Protocol 1 ended.")

    return(prot1)

  } else if (protocol == "2a") {
    #######################################################################
    # Protocol_2a - Analysis of petrographic data
    # (rank -> relative ranking -> PCoA)
    #######################################################################

    # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
    vars_petro <- get_petro(data)

    var_set_index <- list(1:length(vars_petro))

    data_ <- order_petro(data)

    dist2a <- extended_gower(data = data_[, vars_petro],
                             variable_sets = var_set_index,
                             method = "RRD",
                             exception_columns = exception_columns,
                             exception_values = "none",
                             exception_distance = 0
    )

    # pcoa
    prot2a <- pcoa(dist2a,
                   data_[, vars_petro],
                   variable_tags,
                   dimensions = dimensions)

    prot2a$name <- "Protocol 2a"

    prot2a$dist_matrix <- dist2a

    print("Protocol 2a ended.")

    return(prot2a)

  }
  else if (protocol == "2b") {
    #######################################################################
    # Protocol 2b - Analysis of petrographic data
    # (rank -> neighbor interchange -> NMDS)
    #######################################################################

    # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
    vars_petro <- get_petro(data)

    var_set_index <- list(1:length(vars_petro))

    data_ <- order_petro(data)

    dist2b <- extended_gower(data = data_[, vars_petro],
                             variable_sets = var_set_index,
                             method = "NI",
                             exception_columns = exception_columns,
                             exception_values = "none",
                             exception_distance = 0
    )

    # Kruskal's non-metric multidimensional scaling (or NMDS)
    prot2b <- nmds(dist2b,
                   data_[, vars_petro],
                   variable_tags,
                   dimensions = dimensions,
                   trymax = 1000,
                   init_seed = 0,
                   autotransform = FALSE
    )

    prot2b$name <- "Protocol 2b"

    prot2b$dist_matrix <- dist2b

    print("Protocol 2b ended.")

    return(prot2b)

  }
  else if (protocol == "3") {
    #######################################################################
    # Protocol_3 - Analysis of compositional data (recommended: clr -> Euclidean)
    # and petrographic data (rank -> relative ranking),
    # Gower coefficient (50:50 weights)
    #######################################################################

    # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
    data_transformed <- transform_coda(data,
                                       coda_variables = coda_override,
                                       method = coda_transformation_method)

    vars_petro <- get_petro(data_transformed)

    coda_vars <- get_coda(data_transformed,
                          coda_override,
                          transformation_method = coda_transformation_method)

    vars <- c(vars_petro, coda_vars)

    var_set_index <- list(1:length(vars_petro),
                          (length(vars_petro) + 1):(length(vars_petro) +
                                                      length(coda_vars)))

    data_transformed <- order_petro(data_transformed)

    dist3 <- extended_gower(data = data_transformed[, vars],
                            variable_sets = var_set_index,
                            method = "MM",
                            exception_columns = exception_columns,
                            exception_values = "none",
                            exception_distance = 0
    )

    # pcoa
    prot3 <- pcoa(dist3,
                  data_transformed[, vars],
                  variable_tags,
                  dimensions = dimensions)

    prot3$name <- "Protocol 3"

    prot3$dist_matrix <- dist3

    prot3$transformation_method <- coda_transformation_method

    print("Protocol 3 ended.")

    return(prot3)

  }
  else
    if (protocol == "4") {
      #######################################################################
      # Protocol 4 - Analysis of geochemical data (recommended: clr -> Euclidean)
      # and petrographic data (rank -> relative ranking) to characterize provenance
      #######################################################################

      # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
      data_transformed <- transform_coda(data,
                                         coda_variables = coda_override,
                                         method = coda_transformation_method)

      vars_indexes <- get_provenance(data_transformed,
                                     coda_override,
                                     transformation_method = coda_transformation_method)

      var_set_index <- list(1:length(vars_indexes[[1]]),
                            (length(vars_indexes[[1]]) + 1):(length(vars_indexes[[1]]) + length(vars_indexes[[2]])))

      vars_prov <- c(vars_indexes[[1]], vars_indexes[[2]])

      data_transformed <- order_petro(data_transformed)

      dist4 <- extended_gower(data = data_transformed[, vars_prov],
                              variable_sets = var_set_index,
                              method = "MM",
                              exception_columns = exception_columns,
                              exception_values = "none",
                              exception_distance = 0
      )

      # pcoa
      prot4 <- pcoa(dist4,
                    data_transformed[, vars_prov],
                    variable_tags,
                    dimensions = dimensions)

      prot4$name <- "Protocol 4"

      prot4$dist_matrix <- dist4

      prot4$transformation_method <- coda_transformation_method

      print("Protocol 4 ended.")

      return(prot4)

    }
}
