#' Mixed-variables coefficient of distance (cerUB version)
#'
#' "The mixed-variables coefficient of distance generalizes Gower's general
#' coefficient of distance to allow the treatment of various statistical types of
#' variables when calculating distances. This is especially important when
#' measuring functional diversity. Indeed, most of the indices that measure
#' functional diversity depend on variables (traits) that have various
#' statistical types (e.g. circular, fuzzy, ordinal) and that go through a matrix
#' of distances among species." (From \code{\link[ade4]{dist.ktab}}) This is a
#' modified version that allows for 'exception values' in ordinal variables and
#' weighting the class-wise data sets differently.
#'
#' @param ktab_data Object of class ktab (created with
#'                  \code{\link[ade4]{ktab.list.df}}), including data frames
#'                  of different data classes.
#' @param variable_classes Vector that provide the type of each table in x.
#'                  The possible types are "Q" (quantitative), "O" (ordinal),
#'                  "N" (nominal), "D" (dichotomous), "F" (fuzzy, or expressed
#'                  as a proportion), "B" (multichoice nominal variables, coded
#'                  by binary columns), "C" (circular), "CODA" (compositional).
#'                  Values in type must be in the same order as in x.
#' @param option,scann,tol Arguments of \code{\link[ade4]{dist.ktab}}.
#' @param is_protocol2b Logical, whether the protocol 2b is being applied.
#'                  Protocol 2b uses 'neighbour interexchange' to calculate
#'                  distance in ordinal variables.
#' @param dist.excep The distance from any value to an exception value, that
#'                   should be previously transform into NA.
#' @param weight Numeric, vector with the weights attributed to every data frame
#'               included in ktab_data.
#'
#' @export
dist.ktab_cerUB <- function(ktab_data,
                            variable_classes,
                            option = c("scaledBYrange",
                                       "scaledBYsd",
                                       "noscale"),
                            scann = FALSE,
                            tol = 1e-8,
                            is_protocol2b = FALSE,
                            dist.excep = 2,
                            weight = NULL) {
  #******************************************************#

  # Parameters are checked                               #

  #******************************************************#



  if (!inherits(ktab_data, "ktab"))

    stop("data is not an object of class ktab")

  if (any(is.na(match(
    variable_classes, c("Q", "O", "N", "D", "F", "B", "C", "CODA")
  ))))

    stop("incorrect variable_classes: accepted values are O, Q, N, D, F, B, C and CODA")

  if (length(ktab_data$blo) != length(variable_classes))

    stop("incorrect length for variable_classes")

  if (!is.numeric(tol))

    stop("tol is not a numeric")



  #*****************************************************#

  # If scann is TRUE, the functions of distance         #

  #*****************************************************#



  if (scann == TRUE) {
    if (any(variable_classes == "F")) {
      cat("Choose your metric for fuzzy variables\n")

      cat("1 = d1 Manly\n")

      cat("d1 = Sum|p(i)-q(i)|/2\n")

      cat("2 = Overlap index Manly\n")

      cat("d2 = 1-Sum(p(i)q(i))/sqrt(Sum(p(i)^2))/sqrt(Sum(q(i)^2))\n")

      cat("3 = Rogers 1972 (one locus)\n")

      cat("d3 = sqrt(0.5*Sum(p(i)-q(i)^2))\n")

      cat("4 = Edwards 1971 (one locus)\n")

      cat("d4 = sqrt(1 - (Sum(sqrt(p(i)q(i)))))\n")

      cat("Selec an integer (1-4): ")

      methodF <- as.integer(readLines(n = 1))

      if (methodF == 4)

        methodF <- 5

    }

    if (any(variable_classes == "B")) {
      cat("Choose your metric for binary variables\n")

      cat("1 = JACCARD index (1901) S3 coefficient of GOWER &

          LEGENDRE\n")

      cat("s1 = a/(a+b+c) --> d = sqrt(1 - s)\n")

      cat("2 = SOCKAL & MICHENER index (1958) S4 coefficient of GOWER & LEGENDRE \n")

      cat("s2 = (a+d)/(a+b+c+d) --> d = sqrt(1 - s)\n")

      cat("3 = SOCKAL & SNEATH(1963) S5 coefficient of GOWER &

          LEGENDRE\n")

      cat("s3 = a/(a+2(b+c)) --> d = sqrt(1 - s)\n")

      cat("4 = ROGERS & TANIMOTO (1960) S6 coefficient of GOWER &

          LEGENDRE\n")

      cat("s4 = (a+d)/(a+2(b+c)+d) --> d = sqrt(1 - s)\n")

      cat("5 = CZEKANOWSKI (1913) or SORENSEN (1948) S7 coefficient of GOWER & LEGENDRE\n")

      cat("s5 = 2*a/(2*a+b+c) --> d = sqrt(1 - s)\n")

      cat("6 = S9 index of GOWER & LEGENDRE (1986)\n")

      cat("s6 = (a-(b+c)+d)/(a+b+c+d) --> d = sqrt(1 - s)\n")

      cat("7 = OCHIAI (1957) S12 coefficient of GOWER & LEGENDRE\n")

      cat("s7 = a/sqrt((a+b)(a+c)) --> d = sqrt(1 - s)\n")

      cat("8 = SOKAL & SNEATH (1963) S13 coefficient of GOWER &

          LEGENDRE\n")

      cat("s8 = ad/sqrt((a+b)(a+c)(d+b)(d+c)) --> d = sqrt(1 - s)\n")

      cat("9 = Phi of PEARSON = S14 coefficient of GOWER & LEGENDRE\n")

      cat("s9 = (ad-bc)/sqrt((a+b)(a+c)(b+d)(d+c)) --> d = sqrt(1 -

          s)\n")

      cat("10 = S2 coefficient of GOWER & LEGENDRE\n")

      cat("s10 =  a/(a+b+c+d) --> d = sqrt(1 - s) and unit

          self-similarity\n")

      cat("Select an integer (1-10): ")

      methodB <- as.integer(readLines(n = 1))

    }

    methodO <- 0

    if (any(variable_classes == "O")) {
      cat("Choose your metric for ordinal variables\n")

      cat("1 = ranked variables treated as quantitative variables\n")

      cat("2 = Podani (1999)'s formula\n")

      cat("Select an integer (1-2): ")

      methodO <- as.integer(readLines(n = 1))

    }

    if (any(c(variable_classes == "Q", methodO == 1))) {
      cat("Choose your metric for quantitative variables\n")

      cat("1 = Euclidean\n")

      cat("d1 = Sum((x(i)-y(i))^2)/n\n")

      cat("2 = Manhattan\n")

      cat("d2= Sum(|x(i)-y(i)|)/n\n")

      cat("Select an integer (1-2): ")

      methodQ <- as.integer(readLines(n = 1))

    }

  }



  else{
    if (is_protocol2b == TRUE) {
      methodO <- 2
    } else {
      methodO <- 1
      methodQ <- 1
    }

    methodF <- 2

    methodB <- 1

  }



  nlig <- nrow(ktab_data[[1]])

  nAcceptedClasses <- length(unique(variable_classes))

  if (any(variable_classes == "D"))
    napres <- TRUE

  else

    napres <-
    any(is.na(unlist(ktab_data[(1:length(ktab_data$blo))])))

  d.names <- rownames(ktab_data[[1]])



  treatment <- function(i)

  {
    #*****************************************************#

    # Ordinal data                                        #

    #*****************************************************#



    if (variable_classes[i] == "O") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      transrank <- function(u) {
        return(rank(u, na.last = "keep"))

      }

      df <- apply(ktab_data[[i]], 2, transrank)



      #*****************************************************#



      if (methodO == 1) {
        if (!any(is.na(df))) {
          cmax <- apply(df, 2, max, na.rm = TRUE)

          cmin <- apply(df, 2, min, na.rm = TRUE)

          df <-
            as.data.frame(scale(df, center = cmin, scale = (cmax - cmin)))

          if (methodQ == 1) {
            thedis <- ade4::dist.quant(df, method = 1)

          }

          else{
            mat <- matrix(0, nlig, nlig)

            index <-
              cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])

            fun1 <- function(tab) {
              fun2 <- function(u) {
                return(abs(tab[u[1],] - tab[u[2],]))

              }

              d <- unlist(apply(index, 1, fun2))

              attr(d, "Size") <- nlig

              attr(d, "Labels") <- d.names

              attr(d, "Diag") <- FALSE

              attr(d, "Upper") <- FALSE

              attr(d, "method") <- "quantitative"

              attr(d, "call") <- match.call()

              class(d) <- "dist"

              return(d)

            }

            thedis <- fun1(df)

          }

          thedis[thedis < tol] <- 0

          nbvar <- ncol(ktab_data[[i]])

          if (napres) {
            ntvar <- matrix(ncol(df), nrow(df), nrow(df))

          }

        }

        else{
          cmax <- apply(df, 2, max, na.rm = TRUE)

          cmin <- apply(df, 2, min, na.rm = TRUE)

          df <-
            as.data.frame(scale(
              df,
              center = cmin,
              scale = ifelse((cmax - cmin) < tol,

                             1, cmax - cmin)
            ))

          mat <- matrix(0, nlig, nlig)

          index <-
            cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])



          fun1 <- function(vect) {
            fun2 <- function(u) {
              if (methodQ == 1)

                return((vect[u[1]] - vect[u[2]]) ^ 2)



              else

                return(abs(vect[u[1]] - vect[u[2]]))

            }

            d <- unlist(apply(index, 1, fun2))

            attr(d, "Size") <- nlig

            attr(d, "Labels") <- d.names

            attr(d, "Diag") <- FALSE

            attr(d, "Upper") <- FALSE

            attr(d, "method") <- "quantitative"

            attr(d, "call") <- match.call()

            class(d) <- "dist"

            return(d)

          }

          if (ncol(df) == 1)

            lis <- list(df[, 1])

          else

            lis <- as.list(df)

          listdis <- lapply(lis, fun1)



          listmat <- lapply(listdis, as.matrix)

          fun1 <- function(u) {
            u[!is.na(u)] <- 1

            u[is.na(u)] <- 0

            return(u)

          }

          interm <- lapply(listmat, fun1)

          mat <- interm[[1]]

          if (length(interm) > 1) {
            for (k in 2:length(interm)) {
              mat <- interm[[k]] + mat

            }

          }

          ntvar <- mat

          # calculation of the sum of distances

          fun2 <- function(u) {
            u[is.na(u)] <- dist.excep

            return(u)

          }

          res <- lapply(listdis, fun2)

          mat <- res[[1]]

          if (length(res) > 1) {
            for (k in 2:length(res)) {
              mat <- res[[k]] + mat

            }

          }

          thedis <- mat

          thedis[thedis < tol] <- 0

          thedis <- sqrt(thedis)

        }

      }

      else{
        #####################################################

        # Podani's distance                                 #

        #####################################################



        df2 <-
          cbind.data.frame(df[, apply(df, 2, function(u)
            ! all(is.na(u)))])

        if (ncol(df2) == 0)
          stop("one of the quantitative data frames is full of NA")

        if (ncol(df) != ncol(df2)) {
          warning("a column full of NA in the quantitative or ordinal data set ",
                  i)

          df <- as.data.frame(df2)

        }

        if (!all(unlist(lapply(df, is.numeric))))

          stop("Incorrect definition of the quantitative variables")



        #*****************************************************#

        cmax <- apply(df, 2, max, na.rm = TRUE)

        cmin <- apply(df, 2, min, na.rm = TRUE)

        if (ncol(df) > 1)

          granks <- apply(df, 2, table)

        else

          granks <- list(as.vector(apply(df, 2, table)))

        grankmax <-
          as.vector(unlist(lapply(granks, function(u)
            u[length(u)])))

        grankmin <-
          as.vector(unlist(lapply(granks, function(u)
            u[1])))

        if (ncol(df) > 1)

          uranks <- apply(df, 2, function(u)
            sort(unique(u)))

        else

          uranks <-
          list(as.vector(apply(df, 2, function(u)
            sort(unique(
              u
            )))))

        mat <- matrix(0, nlig, nlig)

        index <-
          cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])

        fun1 <- function(k) {
          r <- df

          fun2 <- function(u) {
            if (any(is.na(c(r[u[1], k], r[u[2], k])))) {
              return(NA)
            }

            else{
              if (r[u[1], k] == r[u[2], k]) {
                return(0)
              }

              else{
                val <-
                  (abs(r[u[1], k] - r[u[2], k]) - (granks[[k]][uranks[[k]] == r[u[1], k]] - 1) /
                     2 -

                     (granks[[k]][uranks[[k]] == r[u[2], k]] - 1) /
                     2) / ((cmax[k] - cmin[k]) -

                             (grankmax[k] - 1) /
                             2 - (grankmin[k] - 1) / 2)

                return(val)

              }

            }

          }

          d <- unlist(apply(index, 1, fun2))

          attr(d, "Size") <- nlig

          attr(d, "Labels") <- d.names

          attr(d, "Diag") <- FALSE

          attr(d, "Upper") <- FALSE

          attr(d, "method") <- "quantitative"

          attr(d, "call") <- match.call()

          class(d) <- "dist"

          return(d)

        }

        lis <- as.list(1:ncol(df))

        listdis <- lapply(lis, fun1)

        if (napres) {
          listmat <- lapply(listdis, as.matrix)

          fun1 <- function(u) {
            u[!is.na(u)] <- 1

            u[is.na(u)] <- 0

            return(u)

          }

          interm <- lapply(listmat, fun1)

          mat <- interm[[1]]

          if (length(interm) > 1) {
            for (k in 2:length(interm)) {
              mat <- interm[[k]] + mat

            }

          }

          ntvar <- mat

        }

        else

          nbvar <- ncol(ktab_data[[i]])

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- dist.excep

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }



    #*****************************************************#

    # Quantitative data                                   #

    #*****************************************************#



    if (variable_classes[i] == "Q") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      df <- ktab_data[[i]]

      df2 <-
        cbind.data.frame(df[, apply(df, 2, function(u)
          ! all(is.na(u)))])

      if (ncol(df2) == 0)
        stop("one of the quantitative data frames is full of NA")

      if (ncol(df) != ncol(df2)) {
        warning("a column full of NA in the quantitative or ordinal data set ",
                i)

        df <- as.data.frame(df2)

      }

      if (!all(unlist(lapply(df, is.numeric))))

        stop("Incorrect definition of the quantitative variables")



      #*****************************************************#



      if (option[1] == "scaledBYsd")

        df <- as.data.frame(scale(df))

      if (option[1] == "scaledBYrange")

      {
        cmax <- apply(df, 2, max, na.rm = TRUE)

        cmin <- apply(df, 2, min, na.rm = TRUE)

        df <-
          as.data.frame(scale(
            df,
            center = cmin,
            scale = ifelse((cmax - cmin) < tol,

                           1, cmax - cmin)
          ))

      }



      if (!any(is.na(df))) {
        if (methodQ == 1) {
          thedis <- ade4::dist.quant(df, method = methodQ)

        }

        else{
          mat <- matrix(0, nlig, nlig)

          index <-
            cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])

          fun1 <- function(tab) {
            fun2 <- function(u) {
              return(abs(tab[u[1],] - tab[u[2],]))

            }

            d <- unlist(apply(index, 1, fun2))

            attr(d, "Size") <- nlig

            attr(d, "Labels") <- d.names

            attr(d, "Diag") <- FALSE

            attr(d, "Upper") <- FALSE

            attr(d, "method") <- "quantitative"

            attr(d, "call") <- match.call()

            class(d) <- "dist"

            return(d)

          }

          thedis <- fun1(df)

        }

        thedis[thedis < tol] <- 0

        nbvar <- ncol(ktab_data[[i]])

        if (napres) {
          ntvar <- matrix(ncol(df), nrow(df), nrow(df))

        }

      }

      else{
        mat <- matrix(0, nlig, nlig)

        index <-
          cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])



        fun1 <- function(vect) {
          fun2 <- function(u) {
            if (methodQ == 1)

              return((vect[u[1]] - vect[u[2]]) ^ 2)

            else

              return(abs(vect[u[1]] - vect[u[2]]))

          }

          d <- unlist(apply(index, 1, fun2))

          attr(d, "Size") <- nlig

          attr(d, "Labels") <- d.names

          attr(d, "Diag") <- FALSE

          attr(d, "Upper") <- FALSE

          attr(d, "method") <- "quantitative"

          attr(d, "call") <- match.call()

          class(d) <- "dist"

          return(d)

        }

        if (ncol(df) == 1)

          lis <- list(df[, 1])

        else

          lis <- as.list(df)

        listdis <- lapply(lis, fun1)



        listmat <- lapply(listdis, as.matrix)

        fun1 <- function(u) {
          u[!is.na(u)] <- 1

          u[is.na(u)] <- 0

          return(u)

        }

        interm <- lapply(listmat, fun1)

        mat <- interm[[1]]

        if (length(interm) > 1) {
          for (k in 2:length(interm)) {
            mat <- interm[[k]] + mat

          }

        }

        ntvar <- mat

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- 0

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }



    #*****************************************************#

    # Nominal data                                        #

    #*****************************************************#



    if (variable_classes[i] == "N") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      df <- ktab_data[[i]]

      df2 <-
        cbind.data.frame(df[, apply(df, 2, function(u)
          ! all(is.na(u)))])

      if (ncol(df2) == 0)
        stop("one of the nominal data frames is full of NA")

      if (ncol(df) != ncol(df2)) {
        warning("a column full of NA in the nominal data sets")

        df <- as.data.frame(df2)

      }



      verif <- function(u) {
        if (!is.factor(u)) {
          if (!is.character(u))

            stop("Incorrect definition of the nominal variables")

        }

      }



      lapply(df, verif)



      #*****************************************************#



      if (!any(is.na(df))) {
        FUN <- function(u) {
          m <- model.matrix( ~ -1 + as.factor(u))

          return(dist(m) / sqrt(2))

        }

        lis <- as.list(df)

        res <- lapply(lis, FUN)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

        nbvar <- ncol(df)

        if (napres) {
          ntvar <- matrix(ncol(df), nrow(df), nrow(df))

        }

      }

      else{
        mat <- matrix(0, nlig, nlig)

        index <-
          cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])

        fun1 <- function(vect) {
          fun2 <- function(u) {
            if (any(is.na(c(vect[u[1]], vect[u[2]]))))
              return(NA)

            else{
              if (vect[u[1]] == vect[u[2]]) {
                return(0)

              }

              else
                return(1)

            }

          }

          d <- unlist(apply(index, 1, fun2))

          attr(d, "Size") <- nlig

          attr(d, "Labels") <- d.names

          attr(d, "Diag") <- FALSE

          attr(d, "Upper") <- FALSE

          attr(d, "method") <- "nominal"

          attr(d, "call") <- match.call()

          class(d) <- "dist"

          return(d)

        }

        if (ncol(df) == 1)

          lis <- list(df[, 1])

        else

          lis <- as.list(df)

        listdis <- lapply(lis, fun1)



        listmat <- lapply(listdis, as.matrix)

        fun1 <- function(u) {
          u[!is.na(u)] <- 1

          u[is.na(u)] <- 0

          return(u)

        }

        interm <- lapply(listmat, fun1)

        mat <- interm[[1]]

        if (length(interm) > 1) {
          for (k in 2:length(interm)) {
            mat <- interm[[k]] + mat

          }

        }

        ntvar <- mat

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- 0

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }



    #*****************************************************#

    # Dichotomous data                                    #

    #*****************************************************#



    if (variable_classes[i] == "D") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      df <- ktab_data[[i]]

      df2 <-
        cbind.data.frame(df[, apply(df, 2, function(u)
          ! all(is.na(u)))])

      if (ncol(df2) == 0)
        stop("one of the nominal data frames is full of NA")

      if (ncol(df) != ncol(df2)) {
        warning("a column full of NA in the dichotomous data sets")

        df <- as.data.frame(df2)

      }



      verif <- function(u) {
        if (any(is.na(match(u, c(0, 1)))))

          stop("Dichotomous variables should have only 0, and 1")

      }



      lapply(df, verif)



      #*****************************************************#



      mat <- matrix(0, nlig, nlig)

      index <-
        cbind(col(mat)[col(mat) < row(mat)], row(mat)[col(mat) < row(mat)])

      fun1 <- function(vect) {
        fun2 <- function(u) {
          if (any(is.na(c(vect[u[1]], vect[u[2]]))))
            return(NA)

          else{
            if (vect[u[1]] == vect[u[2]]) {
              if (vect[u[1]] == 1)

                return(0)

              else
                return(NA)

            }

            else

              return(1)

          }

        }

        d <- unlist(apply(index, 1, fun2))

        attr(d, "Size") <- nlig

        attr(d, "Labels") <- d.names

        attr(d, "Diag") <- FALSE

        attr(d, "Upper") <- FALSE

        attr(d, "method") <- "nominal"

        attr(d, "call") <- match.call()

        class(d) <- "dist"

        return(d)

      }

      if (ncol(df) == 1)

        lis <- list(df[, 1])

      else

        lis <- as.list(df)

      listdis <- lapply(lis, fun1)



      listmat <- lapply(listdis, as.matrix)

      fun1 <- function(u) {
        u[!is.na(u)] <- 1

        u[is.na(u)] <- 0

        return(u)

      }

      interm <- lapply(listmat, fun1)

      mat <- interm[[1]]

      if (length(interm) > 1) {
        for (k in 2:length(interm)) {
          mat <- interm[[k]] + mat

        }

      }

      ntvar <- mat

      # calculation of the sum of distances

      fun2 <- function(u) {
        u[is.na(u)] <- 0

        return(u)

      }

      res <- lapply(listdis, fun2)

      mat <- res[[1]]

      if (length(res) > 1) {
        for (k in 2:length(res)) {
          mat <- res[[k]] + mat

        }

      }

      thedis <- mat

      thedis[thedis < tol] <- 0

      thedis <- sqrt(thedis)

    }



    #*****************************************************#

    # Fuzzy data                                          #

    #*****************************************************#



    if (variable_classes[i] == "F") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#

      df <- ktab_data[[i]]

      df2 <- df[, apply(df, 2, function(u)
        ! all(is.na(u)))]

      if (ncol(df2) == 0)
        stop("one of the fuzzy data frames is full of NA")

      if (ncol(df) != ncol(df2)) {
        stop("a column full of NA in the fuzzy data sets")

      }



      if (!all(unlist(lapply(df, is.numeric))))

        stop("Incorrect definition of the fuzzy variables")



      if (is.null(attributes(df)$col.blocks))

        stop("The fuzzy data set must be prepared with the function prep.fuzzy")



      if (!all(abs(apply(df, 1, sum, na.rm = TRUE) - floor(apply(
        df, 1, sum, na.rm = TRUE
      ))) < tol, na.rm = TRUE))

        stop("The fuzzy data set must be prepared with the function prep.fuzzy")



      #*****************************************************#



      blocs <- attributes(ktab_data[[i]])$col.blocks

      fac <- as.factor(rep(1:length(blocs), blocs))

      lis <- split(as.data.frame(t(ktab_data[[i]])), fac)

      lis <- lapply(lis, t)

      lis <- lapply(lis, cbind.data.frame)



      if (!any(is.na(ktab_data[[i]]))) {
        if (methodF != 3 & methodF != 4)

          res <-
            lapply(lis, function(u)
              ade4::dist.prop(u, method = methodF))

        else

          res <-
            lapply(lis, function(u)
              ade4::dist.prop(u, method = methodF) ^ 2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

        nbvar <- length(blocs)

        if (napres) {
          ntvar <- matrix(length(blocs), nrow(df), nrow(df))

        }

      }

      else{
        fun1 <- function(mtflo) {
          res <- matrix(0, nlig, nlig)

          positions <- apply(mtflo, 1, function(u)
            any(is.na(u)))

          dfsansna <- mtflo[!positions,]

          if (methodF != 3 & methodF != 4)

            resdis <-
            as.matrix(ade4::dist.prop(dfsansna, method = methodF))

          else

            resdis <-
            as.matrix(ade4::dist.prop(dfsansna, method = methodF) ^ 2)

          res[!positions,!positions] <- as.vector(resdis)

          res[positions,] <- NA

          res[, positions] <- NA

          return(as.dist(res))

        }

        listdis <- lapply(lis, fun1)



        listmat <- lapply(listdis, as.matrix)

        fun1 <- function(u) {
          u[!is.na(u)] <- 1

          u[is.na(u)] <- 0

          return(u)

        }

        interm <- lapply(listmat, fun1)

        mat <- interm[[1]]

        if (length(interm) > 1) {
          for (k in 2:length(interm)) {
            mat <- interm[[k]] + mat

          }

        }

        ntvar <- mat

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- 0

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }



    #*****************************************************#

    # Binary data                                         #

    #*****************************************************#



    if (variable_classes[i] == "B") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      if (!all(unlist(lapply(ktab_data[[i]], is.numeric))))

        stop("Incorrect definition of the binary variables")



      if (is.null(attributes(ktab_data[[i]])$col.blocks))

        stop("The binary data set must be prepared with the function prep.binary")



      if (any(is.na(match(as.vector(
        as.matrix(ktab_data[[i]])
      ), c(0, 1, NA)))))

        stop("The binary data set must be prepared with the function prep.binary")



      #*****************************************************#



      blocs <- attributes(ktab_data[[i]])$col.blocks

      fac <- as.factor(rep(1:length(blocs), blocs))

      lis <- split(as.data.frame(t(ktab_data[[i]])), fac)

      lis <- lapply(lis, t)

      lis <- lapply(lis, cbind.data.frame)



      if (!any(is.na(ktab_data[[i]]))) {
        res <- lapply(lis, function(u)
          ade4::dist.binary(u, method = methodB) ^ 2)

        if (any(is.na(unlist(res))))

          stop("Rows of zero for binary variables")

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

        nbvar <- length(blocs)

        if (napres) {
          ntvar <- matrix(length(blocs), nlig, nlig)

        }

      }

      else{
        fun1 <- function(mtbin) {
          res <- matrix(0, nlig, nlig)

          positions <- apply(mtbin, 1, function(u)
            any(is.na(u)))

          dfsansna <- mtbin[!positions,]

          resdis <-
            as.matrix(ade4::dist.binary(dfsansna, method = methodB) ^ 2)

          res[!positions,!positions] <- as.vector(resdis)

          res[positions,] <- NA

          res[, positions] <- NA

          return(as.dist(res))

        }

        listdis <- lapply(lis, fun1)



        listmat <- lapply(listdis, as.matrix)

        fun1 <- function(u) {
          u[!is.na(u)] <- 1

          u[is.na(u)] <- 0

          return(u)

        }

        interm <- lapply(listmat, fun1)

        mat <- interm[[1]]

        if (length(interm) > 1) {
          for (k in 2:length(interm)) {
            mat <- interm[[k]] + mat

          }

        }

        ntvar <- mat

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- 0

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }



    #*****************************************************#

    # Circular data                                       #

    #*****************************************************#



    if (variable_classes[i] == "C") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      df <- ktab_data[[i]]

      df2 <- cbind.data.frame(df[, apply(df, 2,

                                         function(u)
                                           ! all(is.na(u)))])

      if (ncol(df2) == 0)
        stop("the circular data frames ", i, " is full of NA")

      if (ncol(df) != ncol(df2)) {
        warning("a column full of NA in the circular data sets")

        df <- as.data.frame(df2)

      }



      if (is.null(attributes(df)$max))

        stop("The circular data sets must be prepared with the function prep.circular")



      verif <- function(u) {
        if (any(u[!is.na(u)] < 0))
          stop("negative values in circular variables")

      }

      lapply(df, verif)



      #*****************************************************#



      d.names <- row.names(ktab_data[[i]])

      nlig <- nrow(ktab_data[[i]])

      mat <- matrix(0, nlig, nlig)

      index <- cbind(col(mat)[col(mat) < row(mat)],

                     row(mat)[col(mat) < row(mat)])

      odd <- function(u) {
        ifelse(abs(u / 2 - floor(u / 2)) < 1e-08, FALSE, TRUE)

      }

      if (!any(is.na(df))) {
        fun1 <- function(nucol) {
          vect <- ktab_data[[i]][, nucol]

          maxi <- attributes(df)$max[nucol]

          vect <- vect / maxi

          fun2 <- function(u) {
            if (odd(maxi))

              return((2 * maxi / (maxi - 1)) *

                       min(c(abs(
                         vect[u[1]] - vect[u[2]]
                       ),

                       (
                         1 - abs(vect[u[1]] - vect[u[2]])
                       )),

                       na.rm = TRUE))

            else

              return(2 * min(c(abs(
                vect[u[1]] - vect[u[2]]
              ),

              (
                1 - abs(vect[u[1]] - vect[u[2]])
              )),

              na.rm = TRUE))

          }

          d <- unlist(apply(index, 1, fun2))

          attr(d, "Size") <- nlig

          attr(d, "Labels") <- d.names

          attr(d, "Diag") <- FALSE

          attr(d, "Upper") <- FALSE

          attr(d, "method") <- "circular"

          attr(d, "call") <- match.call()

          class(d) <- "dist"

          return(d)

        }

        lis <- as.list(1:ncol(df))

        res <- lapply(lis, fun1)



        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

        nbvar <- ncol(ktab_data[[i]])

        if (napres) {
          ntvar <- matrix(ncol(ktab_data[[i]]), nrow(df), nrow(df))

        }

      }

      else{
        fun1 <- function(nucol) {
          vect <- ktab_data[[i]][, nucol]

          maxi <- attributes(df)$max[nucol]

          vect <- vect / maxi

          fun2 <- function(u) {
            if (any(is.na(c(vect[u[1]], vect[u[2]]))))
              return(NA)

            else{
              if (odd(maxi))

                return((2 * maxi / (maxi - 1)) *

                         min(c(
                           abs(vect[u[1]] - vect[u[2]]),

                           (1 - abs(vect[u[1]] - vect[u[2]]))
                         ),

                         na.rm = TRUE))

              else

                return(2 * min(c(
                  abs(vect[u[1]] - vect[u[2]]),

                  (1 - abs(vect[u[1]] - vect[u[2]]))
                ),

                na.rm = TRUE))

            }

          }

          d <- unlist(apply(index, 1, fun2))

          attr(d, "Size") <- nlig

          attr(d, "Labels") <- d.names

          attr(d, "Diag") <- FALSE

          attr(d, "Upper") <- FALSE

          attr(d, "method") <- "circular"

          attr(d, "call") <- match.call()

          class(d) <- "dist"

          return(d)

        }

        lis <- as.list(1:ncol(df))

        listdis <- lapply(lis, fun1)



        listmat <- lapply(listdis, as.matrix)

        fun1 <- function(u) {
          u[!is.na(u)] <- 1

          u[is.na(u)] <- 0

          return(u)

        }

        interm <- lapply(listmat, fun1)

        mat <- interm[[1]]

        if (length(interm) > 1) {
          for (k in 2:length(interm)) {
            mat <- interm[[k]] + mat

          }

        }

        ntvar <- mat

        # calculation of the sum of distances

        fun2 <- function(u) {
          u[is.na(u)] <- 0

          return(u)

        }

        res <- lapply(listdis, fun2)

        mat <- res[[1]]

        if (length(res) > 1) {
          for (k in 2:length(res)) {
            mat <- res[[k]] + mat

          }

        }

        thedis <- mat

        thedis[thedis < tol] <- 0

        thedis <- sqrt(thedis)

      }

    }


    #*****************************************************#

    # Compositional data                                        #

    #*****************************************************#


    if (variable_classes[i] == "CODA") {
      #*****************************************************#

      # Data are checked                                    #

      #*****************************************************#



      df <- ktab_data[[i]]

      df2 <-
        cbind.data.frame(df[, apply(df, 2, function(u)
          ! all(is.na(u)))])

      if (ncol(df2) == 0)
        stop("one of the quantitative data frames is full of NA")

      if (ncol(df) != ncol(df2)) {
        warning("a column full of NA in the quantitative or ordinal data set ",
                i)

        df <- as.data.frame(df2)

      }

      if (!all(unlist(lapply(df, is.numeric))))

        stop("Incorrect definition of the quantitative variables")



      #*****************************************************#


      pca <- best.pcaCoDa(df, init.seed = 0, samples = 1000)

      thedis <-

        thedis[thedis < tol] <- 0

      nbvar <- ncol(ktab_data[[i]])

      if (napres) {
        ntvar <- matrix(ncol(df), nrow(df), nrow(df))

      }

    }

    if (!napres)

      return(list(nbvar, thedis))

    else

      return(list(ntvar, thedis))

  }



  # Last calculations



  interm <- as.list(1:length(ktab_data$blo))

  names(interm) <-
    paste("iteration", 1:length(ktab_data$blo), sep = "")

  res <- lapply(interm, treatment)

  if (!napres)

    nbvar <- sum(unlist(lapply(res, function(u)
      u[[1]])))

  else{
    listntvar <- lapply(res, function(u)
      u[[1]])

    mat <- listntvar[[1]]

    if (length(listntvar) > 1) {
      for (k in 2:length(listntvar)) {
        mat <- listntvar[[k]] + mat

      }

    }

    ntvar <- mat + diag(rep(1, nlig))

  }

  dis <- lapply(res, function(u)
    u[[2]])

  if (is.null(weight) == F) {
    dis <- lapply(dis, function(x)
      x ^ 2) # squared dist

    mat <-
      weight[1] * as.matrix(dis[[1]]) / res[[1]][[1]] # average x weight

    if (length(dis) > 1) {
      for (k in 2:length(dis)) {
        # sum averages x weight
        mat <-
          (weight[k] * as.matrix(dis[[k]]) / res[[k]][[1]]) + mat

      }

    }
    # square root
    if (!napres) {
      disglobal <- sqrt(mat)

    }

    else{
      disglobal <- as.dist(sqrt(as.matrix(mat)))

    }

  } else {
    # average weighted by number of variables
    mat <- dis[[1]] ^ 2

    if (length(dis) > 1) {
      for (k in 2:length(dis)) {
        mat <- dis[[k]] ^ 2 + mat

      }

    }

    if (!napres) {
      disglobal <- sqrt(mat / nbvar)

    }

    else{
      disglobal <- as.dist(sqrt(as.matrix(mat) / ntvar))

    }

  }

  attributes(disglobal)$Labels <- d.names



  return(disglobal)

}



prep.binary <- function (df, col.blocks)

{
  if (!is.data.frame(df))

    stop("data.frame expected")



  if (sum(col.blocks) != ncol(df)) {
    stop("non convenient data in col.blocks")

  }



  if (is.null(names(col.blocks))) {
    names(col.blocks) <- paste("FV", as.character(1:length(col.blocks)),

                               sep = "")

  }

  df2 <- df[, apply(df, 2, function(u)
    ! all(is.na(u)))]

  bloc <- rep(1:length(col.blocks), col.blocks)

  bloc <- as.factor(bloc[apply(df, 2, function(u)
    ! all(is.na(u)))])

  col.blocks <- as.vector(table(bloc))

  df <- df2

  if (any(df[!is.na(df)] < 0))

    stop("non negative value expected in df")

  d.names <- row.names(df)

  nlig <- nrow(df)

  df <- as.matrix(1 * (df > 0))

  f1 <- function(x, k) {
    a <- sum(x)

    if (is.na(a)) {
      return(rep(NA, length(x)))

      cat("missing data found in block", k, "\n")

    }

    if (a == 0)

      return(rep(NA, length(x)))

    return(x)

  }

  k2 <- 0

  for (k in 1:(length(col.blocks))) {
    k1 <- k2 + 1

    k2 <- k2 + col.blocks[k]

    X <- df[, k1:k2]

    X <- t(apply(X, 1, f1, k = k))

    df[, k1:k2] <- X

  }

  df <- as.data.frame(df)

  attr(df, "col.blocks") <- col.blocks

  col.num <- factor(rep((1:length(col.blocks)), col.blocks))

  attr(df, "col.num") <- col.num

  return(df)

}



prep.circular <-

  function (data,
            rangemin = apply(data, 2, min, na.rm = TRUE),
            rangemax = apply(data, 2, max, na.rm = TRUE))

  {
    if (!is.data.frame(data))

      stop("data.frame expected")

    veriffun <- function(i) {
      if (rangemin[i] > min(data[i], na.rm = TRUE))
        stop("Incorrect minimum in rangemin")

      if (rangemax[i] < max(data[i], na.rm = TRUE))
        stop("Incorrect maximum in rangemax")

    }

    sapply(1:ncol(data), veriffun)

    data1 <- sweep(data, 2, rangemin, "-")

    max2 <- rangemax - rangemin

    attr(data1, "max") <- max2 + 1

    return(data1)

  }



prep.fuzzy <-

  function (data, col.blocks, row.w = rep(1, nrow(data)))

  {
    if (!is.data.frame(data))

      stop("data.frame expected")

    if (!is.null(row.w)) {
      if (length(row.w) != nrow(data))

        stop("non convenient dimension")

    }

    if (sum(col.blocks) != ncol(data)) {
      stop("non convenient data in col.blocks")

    }

    data2 <- data[, apply(data, 2, function(u)
      ! all(is.na(u)))]

    bloc <- rep(1:length(col.blocks), col.blocks)

    bloc <-
      as.factor(bloc[apply(data, 2, function(u)
        ! all(is.na(u)))])

    col.blocks <- as.vector(table(bloc))

    data <- data2

    if (is.null(row.w))

      row.w <- rep(1, nrow(data)) / nrow(data)

    row.w <- row.w / sum(row.w)

    if (is.null(names(col.blocks))) {
      names(col.blocks) <- paste("FV", as.character(1:length(col.blocks)),

                                 sep = "")

    }

    f1 <- function(x, k) {
      a <- sum(x)

      if (is.na(a)) {
        return(rep(NA, length(x)))

        cat("missing data found in block", k, "\n")

      }

      if (a == 0)

        return(rep(NA, length(x)))

      return(x / a)

    }

    k2 <- 0

    col.w <- rep(1, ncol(data))

    for (k in 1:(length(col.blocks))) {
      k1 <- k2 + 1

      k2 <- k2 + col.blocks[k]

      X <- data[, k1:k2]

      X <- t(apply(X, 1, f1, k = k))

      X.marge <- apply(X, 1, sum, na.rm = TRUE)



      X.marge <- X.marge * row.w

      X.marge <- X.marge / sum(X.marge, na.rm = TRUE)

      X.mean <- apply(X * X.marge, 2, sum)

      data[, k1:k2] <- X

      col.w[k1:k2] <- X.mean

    }

    attr(data, "col.blocks") <- col.blocks

    attr(data, "row.w") <- row.w

    attr(data, "col.freq") <- col.w

    col.num <- factor(rep((1:length(col.blocks)), col.blocks))

    attr(data, "col.num") <- col.num

    return(data)

  }
