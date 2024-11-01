#' Quantise
#'
#' \code{quantise} quantises a vector.
#'
#' @param x The vector to quantise.
#' @param n The Number of quantiles, defaults to 10.
#' @param invert Should the highest quantile represent the lowest input value?
#'   Defaults to false.
#'
#' @autoglobal
#' @keywords internal
#' @noRd
#'
#' @examples
#' quantise(c(1:20))
#' quantise(c(1:20), n = 10, invert = TRUE)
quantise <- function(x,
                     n = 10,
                     invert = FALSE) {
  if (length(unique(x)) <= 1) {
    stop("The vector cannot be quantised as there is only one unique value.")
  }

  quantile_breaks <- stats::quantile(
    x,
    probs = seq(0, 1, length.out = n + 1), na.rm = TRUE
  )

  quantiles <- as.integer(
    cut(x, breaks = quantile_breaks, include.lowest = TRUE)
  )

  if (invert) {
    max_quant <- max(quantiles, na.rm = TRUE)
    quantiles <- (max_quant + 1) - quantiles
  }

  quantile_counts <- table(quantiles)
  equal_bins <- length(unique(quantile_counts)) == 1
  if (!equal_bins) warning("Quantiles are not in equal bins.")

  return(quantiles)
}
