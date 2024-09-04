#' Exponentially transform domain indexes
#'
#' Transform domains to an exponential distribution using the exponential
#' transformation function listed in Appendix A of the Welsh IMD's technical
#' report: \url{https://www.gov.wales/sites/default/files/statistics-and-research/2023-10/welsh-index-multiple-deprivation-2019-technical-report.pdf}.
#'
#' @section When to use:
#' The exponential transformation of ranks makes it harder for high deprivation
#' in one domain to be offset by low deprivation in another. This transformation
#' stretches out the rankings of the most deprived areas, creating gaps that
#' highlight the true differences and place more emphasis on the most deprived
#' parts of the distribution.
#'
#' @param x A vector of integers, in the range \\[0, 1\\]
#'
#' @export
#' @examples
#' transform_exp(c(0, 0.5, 1))
transform_exp <- function(x) {
  if (any(x < 0) | any(x > 1)) stop("'x' must be in the range 0-1")

  -23 * log(1 - x * (1 - exp(-100 / 23)))
}
