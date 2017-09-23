#' Replaces NA values
#'
#' Replaces NA values in a vector according to a given method.
#' Specific non-NA values can be considered as NA, if given in as_na.
#'
#' @param x Numeric or Character/factor vector.
#' @param as_na Numeric or Character, specifies values that may be considered as NA.
#' @param method Character, the method used for choosing the replacement ("NULL", "random", "mode", "normal"). Accepts numeric value if x is numeric. Method must be compatible with x class.
#'
#' @examples
#'
#' \dontrun{
#'
#' replace_na(c(1, 2, NA, 4, 5, 6), method = 3)
#' replace_na(c(1, 3.5, 9, 2.8, 5.6, 10.4, 0.7, 2.4, 5.5, NA), method = "normal")
#' replace_na(c(1, 3.5, 9, 2.8, 5.6, 10.4, 0.7, 2.4, 5.5, NA), method = "random")
#' replace_na(c("A", "B", "A", "F", "K", "B", "O", "A"), method = "mode")
#'
#' }
#'
#' @export
replace_na <- function(x,
                       as_na = c(NULL),
                       method = NULL) {

  for (i in 1:length(as_na)) {

    if (class(x) == "numeric")
      as_na <- as.numeric(as_na)

    x[x == as_na[i]] <- NA

  }

  if (is.null(method)) {

    return(x)

  }

  if (class(x) == "character" | class(x) == "factor") {

    variableLevels <- levels(!is.na(x))

    if (method == "random") {

      x[is.na(x)] <- sample(variableLevels, 1)

    } else if (method == "mode") {

      Mode <- function(x) {

        ux <- unique(x)
        counts <- tabulate(match(x, ux))
        if (counts[1] == counts[2])
          warning("Replace Na with 'mode' method found a tie and the first value was selected.")
        ux[which.max(counts)]

      }

      md <- Mode(x)
      x[is.na(x)] <- md

    } else {

      print("ERROR: Method passed for naValues is incorrect")

    }
  } else if (class(x) == "numeric") {

    if (method == "random") {

      minV = min(x, na.rm = TRUE)
      maxV = max(x, na.rm = TRUE)

      if (minV == maxV) {

        x[is.na(x)] <- minV

      } else {

        x[is.na(x)] <- runif(1, minV, maxV)

      }
    } else if (method == "normal") {

      meanV = mean(x, na.rm = TRUE)
      sdV = sd(x, na.rm = TRUE)
      x[is.na(x)] <- rnorm(1, mean = meanV, sd = sdV)

    } else if (is.numeric(method)) {

      x[is.na(x)] <- method

    } else {

      print("ERROR: Method passed for naValues is incorrect")

    }
  }

  return(x)
}
