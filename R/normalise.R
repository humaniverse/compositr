#' Normalise ranks between 0 and 1
#'
#' \code{normalise_ranks_01} scales (i.e., normalises) a rank between a scale of
#' 0 and 1.
#'
#' @param x Vector of ranks to be scaled.
#'
#' @export
#'
#' @examples
#' normalise_ranks_01(c(1:3))
normalise_ranks_01 <- function(x) {
  if (all(rank(x) != x)) {
    stop("Please supply ranked data")
  }

  output <- (x - 1) / (length(x) - 1)

  if (max(output) > 1 | min(output) < 0) {
    stop(
      paste0(
        "'x' could not be normalised between 0 and 1. \n  ",
        "Please ensure you provided a ranked vector."
      )
    )
  }

  return(output)
}

#' Mean normalisation
#'
#' \code{normalise_mean} standardises (i.e., normalises) a vector to mean = 0 &
#' SD = 1.
#'
#' @param x Vector to standardise.
#' @param remove_na Boolean indicating whether NA values should be removed.
#'   Defaults to TRUE.
#'
#' @export
#'
#' @examples
#' normalise_mean(c(1:10))
normalise_mean <-
  function(x, remove_na = TRUE) {
    (x - mean(x, na.rm = remove_na)) / stats::sd(x, na.rm = remove_na)
  }

#' Median normalisation
#'
#' This function performs median normalisation on a numeric vector. It
#' subtracts the median from each element in the vector and then divides the
#' resulting values by the square root of the sum of their squared deviations
#' from the median.
#'
#' @param x A numeric vector that will be normalised.
#' @param remove_na Boolean indicating whether NA values should be removed.
#'   Defaults to TRUE.
#'
#' @return A numeric vector of the same length as `x` where each element has
#'   been positionally normalised.
#'
#' @examples
#' normalise_median(c(1, 2, 3, 4, 5))
#' normalise_median(c(10, 20, 30, 40, 50))
#'
#' @export
normalise_median <-
  function(x, remove_na = TRUE) {
    ((x - stats::median(x, na.rm = remove_na)) /
      sqrt(sum((x - stats::median(x, na.rm = remove_na))^2, na.rm = remove_na)))
  }
