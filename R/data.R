#' Wine Roman amphorae from Catalonia, NE Spain
#'
#' A dataset containing petrographic, mineralogical, and geochemical data of wine Roman amphorae from Catalonia, NE Spain.
#'
#' @format A data frame with 238 rows and 148 variables:
#' \describe{
#'   \item{Site_Name}{Archaeological site name (see \code{levels(amphorae$Site_Name)})}
#'   \item{LOCATION_SITE_INITIALS}{Initials indicating site location (see \code{levels(amphorae$LOCATION_SITE_INITIALS)})}
#'   \item{CHARAC}{Is the observation fully characterised? ("complete", "incomplete"; in this data set all are "complete")}
#'   \item{FabricGroup}{Fabric group (see \code{levels(amphorae$FabricGroup)})}
#'   \item{ChemReferenceGroup}{Chemical reference group (see \code{levels(amphorae$ChemReferenceGroup)})}
#'   \item{INCLUS_DISTRIB}{Inclusions distribution ("poorly", "poorly to moderately", "moderately", "moderately to well", "well", "none")}
#'   \item{INCLUS_ORIENT}{Inclusions orientation ("unparallel", "slightly parallel", "parallel", "none")}
#'   \item{TEMP}{Estimated firing temperature in degrees Celsius ("unfired", "700-800oC", "800-900oC", "900-1000oC", "1000-1100oC")}
#'   \item{ATM}{Firing atmosphere ("reducing", "reducing to oxidising", "oxidising", "indeterminate"; in this data set all are "oxidising")}
#'   \item{POST_ATM}{Post-firing atmosphere ("reducing", "reducing to oxidising", "oxidising", "indeterminate"; in this data set all are either "oxidising" or "indeterminate")}
#'   \item{VOID_OVERALL}{Overall void frequency ("none", "very few", "few", "common", "abundant", "very abundant")}
#'   \item{VOID_X_Y}{Void frequency by shape and size ("none", "few", "frequent", "predominant")}
#'   \item{COAR_FREQ}{Inclusions coarse fraction frequency ("none", "very few", "few", "common", "abundant", "very abundant")}
#'   \item{COAR_GRAINSIZE}{Inclusions coarse fraction grain size ("none","very fine","very fine to fine","fine","fine to medium","medium","medium to coarse","coarse","coarse to very coarse","very coarse")}
#'   \item{COAR_ROUNDNESS}{Inclusions coarse fraction roundness ("angular","angular to subangular", "subangular","subangular to subrounded","subrounded","subrounded to rounded","rounded","none")}
#'   \item{COAR_FORM}{Inclusions coarse fraction form ("elongate","elongate to equidimensional","equidimensional","equidimensional to laminar","laminar", "none")}
#'   \item{COAR_SPACING}{Inclusions coarse fraction spacing ("single-spaced","single to double-spaced","double-spaced","double to open-spaced","open-spaced","none")}
#'   \item{COAR_SORTING}{Inclusions coarse fraction sorting ("poorly-sorted","poorly to moderately-sorted","moderately-sorted","moderately to well-sorted","well-sorted","none")}
#'   \item{COAR_R_X}{Inclusions coarse fraction (rocks) frequency by type ("none", "few", "common", "frequent", "dominant", "predominant")}
#'   \item{COAR_C_X}{Inclusions coarse fraction (crystals) frequency by type ("none", "few", "common", "frequent", "dominant", "predominant")}
#'   \item{FINE_FREQ}{Inclusions fine fraction frequency ("none", "very few", "few", "common", "abundant", "very abundant")}
#'   \item{FINE_GRAINSIZE}{Inclusions fine fraction grain size ("none","very fine silt","very fine to fine silt","fine silt","fine to medium silt","medium silt","medium to coarse silt","coarse silt","coarse silt to very fine sand")}
#'   \item{FINE_FORM}{Inclusions fine fraction form ("elongate","elongate to equidimensional","equidimensional","equidimensional to laminar","laminar", "none")}
#'   \item{FINE_C_X}{Inclusions fine fraction (crystals) frequency by type ("none", "few", "frequent", "predominant")}
#'   \item{Fe2O3}{Component mass (Fe2O3)}
#'   ...
#' }
#' @source \url{}
"amphorae"
