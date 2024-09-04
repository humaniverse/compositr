#' Normalise ranks
#'
#' \code{normalise_rank} normalises a rank between a scale of 0 and 1.
#'
#' @param vec Vector of ranks to be normalised.
#'
#' @export
#'
#' @examples
#' normalise_rank(c(1:3))
normalise_rank <-
  function(vec) {
    output <- (vec - 1) / (length(vec) - 1)

    if (max(output) > 1 | min(output) < 0) {
      stop("The vector could not be normalised between 0 and 1. \n  Are you sure you provided a ranked vector?")
    }

    return(output)
  }

#' Standardise
#'
#' \code{standardise} standardises (i.e., normalises) a vector to mean = 0 & SD
#'   = 1.
#'
#' @param vec Vector to normalise.
#'
#' @export
#'
#' @examples
#' standardise(c(1:10))
standardise <-
  function(vec) {
    (vec - mean(vec)) / stats::sd(vec)
  }

#' Positional Normalisation
#'
#' This function performs positional normalisation on a numeric vector. It
#' subtracts the median from each element in the vector and then divides the
#' resulting values by the square root of the sum of their squared deviations
#' from the median.
#'
#' @param x A numeric vector that will be normalised.
#'
#' @return A numeric vector of the same length as `x` where each element has
#'   been positionally normalised.
#'
#' @examples
#' positional_normalisation(c(1, 2, 3, 4, 5))
#' positional_normalisation(c(10, 20, 30, 40, 50))
#'
#' @export
positional_normalisation <-
  function(x) {
    ((x - median(x)) / sqrt(sum((x - median(x))^2)))
  }
