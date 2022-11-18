#' Keep NA
#'
#' \code{keep_na} returns all rows where any column contains a missing value
#'
#' @param data Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' keep_na(mtcars)
#' }
keep_na <- function(data) {
  data |>
    dplyr::anti_join(
      dplyr::drop_na(data)
    )
}
