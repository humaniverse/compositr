#' Quantise
#'
#' \code{quantise} quantises a vector.
#'
#' @param vec The vector to quantise.
#' @param num_quantiles The Number of quantiles, defaults to 10.
#' @param invert Should the highest quantile represent the lowest input value?
#' Defaults to false.
#'
#' @export
#'
#' @examples
#' quantise(c(1:20))
#' quantise(c(1:20), num_quantiles = 5, invert = TRUE)
quantise <-
  function(vec,
           num_quantiles = 10,
           invert = FALSE) {
    if (length(unique(vec)) <= 1) {
      stop("The vector cannot be quantised as there is only one unique value.")
    }

    quantile_breaks <-
      classInt::classIntervals(
        vec,
        num_quantiles,
        style = "quantile"
      )

    quantiles <-
      as.integer(
        cut(
          vec,
          breaks = quantile_breaks$brks,
          include.lowest = TRUE
        )
      )

    if (invert) {
      max_quant <- max(quantiles, na.rm = TRUE)
      quantiles <- (max_quant + 1) - quantiles
    }

    if (
      !(
        tibble::tibble(quantiles = quantiles) |>
        dplyr::count(quantiles) |>
        dplyr::mutate(
          equal_bins = dplyr::if_else(
            n >= (length(vec) / num_quantiles) - 1 &
              n <= (length(vec) / num_quantiles) + 1,
            TRUE,
            FALSE
          )
        ) |>
        dplyr::pull(equal_bins) |>
        all()
      )

    ) {
      warning("Quantiles are not in equal bins.")
    }

    return(quantiles)
  }