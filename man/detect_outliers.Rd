% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_outliers.R
\name{detect_outliers}
\alias{detect_outliers}
\title{Detect outliers}
\usage{
detect_outliers(distMatrix, method = "MD", criterion = "MAD", LOF_k = 2,
  MAD_trim = 2, boxplot_trim = 1.5)
}
\arguments{
\item{distMatrix}{Numeric, distance matrix}

\item{method}{Character, method for measuring separation of one point
from all other points.
The following are accepted:
"MdD": median distance;
"MD": average (mean) distance;
"MAH": Mahalanobis distances (\code{\link[robCompositions]{outCoDa}});
"LOF": Local Outlier Factor Score (\code{\link[dbscan]{lof}}).}

\item{criterion}{Numeric/Character, the criterion used for separating outliers.
The following are accepted:
<Numeric, 0-1>: number between 0 and 1, sets a quantile type of threshold;
"boxplot": outliers are those singled out as points in a boxplot;
"MAD": threshold is given by Median Absolute Deviation.}

\item{LOF_k}{Numeric, when method = "LOF", the size of the neighborhood.
See \code{\link[dbscan]{lof}}.}

\item{MAD_trim}{Numeric, when criterion = "MAD", the multipler of MAD
to calculate a outlier threshold.}

\item{boxplot_trim}{Numeric, when criterion = "boxplot", the multipler of
the interquartile range (IQR) to calculate a outlier
threshold.}
}
\description{
Detects outliers in a distance matrix using a certain method and
following a certain criterion.
}
\examples{

\dontrun{

pca <- princomp(iris[, 1:4])

irisOutliers_MD <- detect_outliers(dist(iris[, 1:4]),
                                   method = "MD",
                                   criterion = "MAD")
irisOutliers_LOF <- detect_outliers(dist(iris[, 1:4]),
                                   method = "LOF",
                                   criterion = "MAD")
plot(pca$scores[, 1:2], col = "black", main = "Outliers")
points(pca$scores[irisOutliers_MD$index, 1:2],
       col = "red", pch = 2, cex = 1.5)
points(pca$scores[irisOutliers_LOF$index, 1:2],
       col = "blue", pch = 6, cex = 1.5)
legend(0.65 * max(pca$scores[,1]), max(pca$scores[,2]),
       c("MD", "LOF"), pch = c(2, 6), col = c("red", "blue"))

}

}
