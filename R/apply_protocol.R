#' Apply protocol for multivariate statistical analysis
#'
#'
#'
#'
apply_protocol <- function(dt,
                           protocol,
                           dimensions = 2,
                           excep_cols = NULL,
                           variable_tags = NULL,
                           directory = "",
                           chem_vars_override = NULL,
                           pca_method = "robust",
                           coda_logratio_trans = "ILR",
                           coda_alr_base = 1,
                           init_seed = 0,
                           coda_samples = 100) {

  if (protocol == "1") {

    #######################################################################
    # Protocol 1 - Analysis of compositional data
    # ilr -> Euclidean -> robPCA
    # (alternatively, any transformation)
    ########################################################################

    chem_vars <- 1:ncol(dt)
    if (!is.null(chem_vars_override))
      chem_vars <- chem_vars_override

    prot1 <- princomp_coda(dt[, chem_vars],
                           trans = coda_logratio_trans,
                           method = pca_method,
                           init_seed = init_seed,
                           samples = coda_samples,
                           alr_base = coda_alr_base)

    prot1$name <- "Protocol 1"

    if (coda_logratio_trans == "ALR")
      prot1$loadings <- prot1$rotation

    print("Protocol 1 ended.")

    return(prot1)

  } else if (protocol == "2a"){

    #######################################################################
    # Protocol_2a - Analysis of petrographic data (rank -> relative ranking -> PCoA)
    #######################################################################

    # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
    vars_petro <- order_petro(dt)

    var_set_index <- list(1:length(vars_petro))

    dt_ <- order_petro(dt)

    dist2a <- extended_gower(data = dt_[,vars_petro],
                             var_set = var_set_index,
                             d_type = "RRD",
                             excep_col = excep_cols,
                             excep_val = "none",
                             excep_dist = 0)

    # pcoa
    prot2a <- pcoa(dist2a,
                   dt_[, vars_petro],
                   variable_tags,
                   k = dimensions)

    prot2a$name <- "Protocol 2a"

    prot2a$dist_matrix <- dist2a

    print("Protocol 2a ended.")

    return(prot2a)

  }
  else if (protocol == "2b"){

      #######################################################################
      # Protocol 2b - Analysis of petrographic data
      # (rank -> neighbor interchange -> NMDS)
      #######################################################################

      # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
      vars_petro <- CAMOTECCERDATA_getPETRO(dt)

      var_set_index <- list(1:length(vars_petro))

      dt_ <- order_petro(dt)

      dist2b <- extended_gower(data = dt_[, vars_petro],
                               var_set = var_set_index,
                               d_type = "NI",
                               excep_col = excep_cols,
                               excep_val = "none",
                               excep_dist = 0)

      # Kruskal's non-metric multidimensional scaling (or NMDS)
      prot2b <- nmds(dist2b,
                     dt_[, vars_petro],
                     variable_tags,
                     k = dimensions,
                     trymax = 1000,
                     init_seed = 0,
                     autotransform = FALSE)

      prot2b$name <- "Protocol 2b"

      prot2b$dist_matrix <- dist2b

      print("Protocol 2b ended.")

      return(prot2b)

    }
  else if (protocol == "3"){

      #######################################################################
      # Protocol_3 - Analysis of compositional data (clr -> Euclidean)
      # and petrographic data (rank -> relative ranking),
      # Gower coefficient (50:50 weights)
      #######################################################################

      # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
      vars_petro <- CAMOTECCERDATA_getPETRO(dt)

      vars_chemcomp <- CAMOTECCERDATA_getCHEMCOMP(dt,
                                                  chem_vars_override,
                                                  trans = "CLR")

      vars <- c(vars_petro, vars_chemcomp)

      var_set_index <- list(1:length(vars_petro),
                            (length(vars_petro) + 1):(length(vars_petro) +
                                                        length(vars_chemcomp)))

      dt_ <- order_petro(dt)

      dist3 <- extended_gower(data = dt[, vars],
                              var_set = var_set_index,
                              d_type = "MM",
                              out_file = paste(directory, "P3_CAT.csv", sep = "/"),
                              excep_col = excep_cols,
                              excep_val = "none",
                              excep_dist = 0)

      # pcoa
      prot3 <- pcoa(dist3,
                    dt[, vars],
                    variable_tags,
                    k = dimensions)

      prot3$name <- "Protocol 3"

      prot3$dist_matrix <- dist3

      print("Protocol 3 ended.")

      return(prot3)

    }
  else
    if (protocol == "4"){

      #######################################################################
      # Protocol 4 - Analysis of geochemical data (clr -> Euclidean)
      # and petrographic data (rank -> relative ranking) to characterize provenance
      #######################################################################

      # calculate extended Gower's distance (Pavoine et al. 2009) with exceptions
      vars_indexes <- CAMOTECCERDATA_getPROVENANCE(dt,
                                                   chem_vars_override,
                                                   trans="CLR")
      var_set_index <- list(
        1:length(vars_indexes[[1]]),
        (length(vars_indexes[[1]]) + 1):
          (length(vars_indexes[[1]]) + length(vars_indexes[[2]])))
      vars_prov <- c(vars_indexes[[1]], vars_indexes[[2]])

      dt_ <- order_petro(dt)

      dist4 <- extended_gower(data = dt_[, vars_prov],
                              var_set = var_set_index,
                              d_type = "MM",
                              excep_col = excep_cols,
                              excep_val = "none",
                              excep_dist = 0)

      # pcoa
      prot4 <- pcoa(dist4,
                    dt_[, vars_prov],
                    variable_tags,
                    k = dimensions)

      prot4$name <- "Protocol 4"

      prot4$dist_matrix <- dist4

      print("Protocol 4 ended.")

      return(prot4)

    }
}
