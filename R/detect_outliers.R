#' Detect outliers
#'
#' Detects outliers in a distance matrix using a certain method and
#' following a certain criterion.
#'
#' @param distMatrix Numeric, distance matrix
#' @param method Character, method for measuring separation of one point
#'                          from all other points.
#'                          The following are accepted:
#'                          "MdD": median distance;
#'                          "MD": average (mean) distance;
#'                          "MAH": Mahalanobis distances (\code{\link[robCompositions]{outCoDa}});
#'                          "LOF": Local Outlier Factor Score (\code{\link[dbscan]{lof}}).
#' @param criterion Numeric/Character, the criterion used for separating outliers.
#'                          The following are accepted:
#'                          <Numeric, 0-1>: number between 0 and 1, sets a quantile type of threshold;
#'                          "boxplot": outliers are those singled out as points in a boxplot;
#'                          "MAD": threshold is given by Median Absolute Deviation.
#' @param LOF_k Numeric, when method = "LOF", the size of the neighborhood.
#'                       See \code{\link[dbscan]{lof}}.
#' @param MAD_trim Numeric, when criterion = "MAD", the multipler of MAD
#'                          to calculate a outlier threshold.
#' @param boxplot_trim Numeric, when criterion = "boxplot", the multipler of
#'                          the interquartile range (IQR) to calculate a outlier
#'                          threshold.
#'
#' @examples
#'
#' \dontrun{
#'
#' pca <- princomp(iris[, 1:4])
#'
#' irisOutliers_MD <- detect_outliers(dist(iris[, 1:4]),
#'                                    method = "MD",
#'                                    criterion = "MAD")
#' irisOutliers_LOF <- detect_outliers(dist(iris[, 1:4]),
#'                                    method = "LOF",
#'                                    criterion = "MAD")
#' plot(pca$scores[, 1:2], col = "black", main = "Outliers")
#' points(pca$scores[irisOutliers_MD$index, 1:2],
#'        col = "red", pch = 2, cex = 1.5)
#' points(pca$scores[irisOutliers_LOF$index, 1:2],
#'        col = "blue", pch = 6, cex = 1.5)
#' legend(0.65 * max(pca$scores[,1]), max(pca$scores[,2]),
#'        c("MD", "LOF"), pch = c(2, 6), col = c("red", "blue"))
#'
#' }
#'
#' @export
detect_outliers <- function(distMatrix,
                            method = "MD",
                            criterion = "MAD",
                            LOF_k = 2,
                            MAD_trim = 2,
                            boxplot_trim = 1.5) {

  outliers <- NULL

  n <- 0

  if (is(distMatrix, "dist"))
    n <- attr(distMatrix, "Size")
  else
    n <- nrow(distMatrix)

  m <- as.matrix(distMatrix)

  if (n > 2) {

    # using Mahalanobis distances
    if (method == "MAH") {

      probs = 0.975

      if (is.numeric(criterion)) {

        probs = criterion

      } else {

        warning("criterion must be numeric when applying mah method. The default 0.975 was used (see quantile in outCoDa{robCompositions})")

      }

      if (n <= ncol(distMatrix) + 1) {

        warning("outCoDa{robCompositions} is not possible. n is less than p+1. Too small sample to detect outliers")
        outliers$index <- rep(FALSE, nrow(distMatrix))
        outliers$Null <- NULL

      } else {

        outliers <- robCompositions::outCoDa(distMatrix,
                                             quantile = probs,
                                             method = "robust",
                                             h = 1/2,
                                             coda = TRUE)
        names(outliers)[3] <- "index"

      }

    }
    else {

      scores <- NULL

      # using Local Outlier Factor Scores
      if (method == "LOF"){

        outliers$out.scores <- dbscan::lof(distMatrix, k = LOF_k)
        names(outliers$out.scores) <- row.names(m)
        outliers$k <- LOF_k
        scores <- outliers$out.scores

      }
      # using average (mean) distances
      else if (method == "MD"){

        outliers$average.dist <- apply(m, 1, function(x) mean(x))
        scores <- outliers$average.dist

      }
      # using median distances
      else if (method == "MdD"){

        outliers$median.dist <- apply(m, 1, function(x) median(x))
        scores <- outliers$median.dist

      }

      # trim (generate index) using the criterion given
      # if numeric (it is a quantile threshold:
      if (is.numeric(criterion)) {

        uif = quantile(scores, probs = criterion)

      }
      # if "boxplot", values singled out as points in a boxplot are selected
      else if (criterion == "boxplot") {

        uif = quantile(scores, probs = .75) + boxplot_trim * IQR(scores)
        outliers$boxplot_trim <- boxplot_trim

      }
      # if "MAD", threshold is given by Median Absolute Deviation
      else if (criterion == "MAD") {

        uif = median(scores) + MAD_trim * mad(scores)
        outliers$MAD_trim <- MAD_trim

      }

      outliers$index <- scores > uif
    }
  }
  else {

    warning("n is less than 3. Too small sample to detect outliers")
    outliers$index <- rep(FALSE, nrow(distMatrix))
    outliers$Null <- NULL

  }

  names(outliers$index) <- row.names(m)
  outliers$method <- method
  outliers$criterion <- criterion

  return(outliers)
}

#' Detect outliers per group
#'
#' Detects outliers per each group in a distance matrix using a certain method and
#' following a certain criterion.
#'
#' @param distMatrix Numeric, distance matrix
#' @param groups Factor, vector containing the assignation of each observation
#'               in the matrix to a specific group.
#' @param method Character, method for measuring separation of one point
#'                          from all other points.
#'                          The following are accepted:
#'                          "MdD": median distance;
#'                          "MD": average (mean) distance;
#'                          "MAH": Mahalanobis distances (\code{\link[robCompositions]{outCoDa}});
#'                          "LOF": Local Outlier Factor Score (\code{\link[dbscan]{lof}}).
#' @param criterion Numeric/Character, the criterion used for separating outliers.
#'                          The following are accepted:
#'                          <Numeric, 0-1>: number between 0 and 1, sets a quantile type of threshold;
#'                          "boxplot": outliers are those singled out as points in a boxplot;
#'                          "MAD": threshold is given by Median Absolute Deviation.
#' @param LOF_k Numeric, when method = "LOF", the size of the neighborhood.
#'                       See \code{\link[dbscan]{lof}}.
#' @param MAD_trim Numeric, when criterion = "MAD", the multipler of MAD
#'                          to calculate a outlier threshold.
#' @param boxplot_trim Numeric, when criterion = "boxplot", the multipler of
#'                          the interquartile range (IQR) to calculate a outlier
#'                          threshold.
#'
#' @examples
#'
#' \dontrun{
#'
#' pca <- princomp(iris[, 1:4])
#'
#' irisSpeciesOutliers_MD <- detect_outliers_per_group(dist(iris[, 1:4]),
#'                                                     iris$Species,
#'                                                     method = "MD",
#'                                                     criterion = "MAD")
#' irisSpeciesOutliers_LOF <- detect_outliers_per_group(dist(iris[, 1:4]),
#'                                                      iris$Species,
#'                                                      method = "LOF",
#'                                                      criterion = "MAD")
#' plot(pca$scores[, 1:2],
#'     col = iris$Species,
#'     main = "Outliers per group")
#' points(pca$scores[irisOutliers_MD$index, 1:2],
#'        col = "purple", pch = 2, cex = 1.5)
#' points(pca$scores[irisOutliers_LOF$index, 1:2],
#'        col = "orange", pch = 6, cex = 1.5)
#' legend(0.65 * max(pca$scores[,1]), max(pca$scores[,2]),
#'        c("MD", "LOF"), pch = c(2, 6), col = c("purple", "orange"))
#'
#' }
#'
#' @export
detect_outliers_per_group <- function(distMatrix,
                                      groups,
                                      method = "MD",
                                      criterion = "MAD",
                                      LOF_k = 2,
                                      MAD_trim = 2,
                                      boxplot_trim = 1.5) {

  outliers <- NULL
  outliers$index <- rep(FALSE, length(groups))

  outliers$details <- vector("list", nlevels(groups))
  names(outliers$details) <- levels(groups)

  if (is(distMatrix, "dist"))
    m <- as.matrix(distMatrix)
  else
    m <- distMatrix

  names(outliers$index) <- row.names(m)

  # iterate for every group
  for (group in levels(groups)){

    if (is(distMatrix, "dist"))
      group.dist <- m[groups == group, groups == group]
    else
      group.dist <- m[groups == group,]

    # skip singletons
    if (length(group.dist) == 1)
      next

    out <- detect_outliers(group.dist,
                           method = method,
                           criterion = criterion,
                           LOF_k = LOF_k,
                           MAD_trim = MAD_trim,
                           boxplot_trim = boxplot_trim)

    outliers$details[[match(group, levels(groups))]] <- out

    for (case in row.names(group.dist)) {
      outliers$index[case] <- outliers$index[case] | out$index[case]
    }

    print(paste("found local outliers of class ", group, sep=""))
  }

  print("finding class-wise outliers ended.")

  return(outliers)
}
