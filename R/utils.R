#' Download a file temporarily to disk
#'
#' @param url A URL for the request
#' @param file_extension A character vector detailing the file extension (e.g.,
#'   ".xlsx")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_file("www.example.com/example_file.csv", ".csv")
#' }
download_file <-
  function(url, file_extension) {
    stopifnot(
      !missing(url),
      !missing(file_extension),
      is.character(url),
      is.character(file_extension)
    )

    temp_path <- tempfile(fileext = file_extension)

    httr2::request(url) |>
      httr2::req_perform(path = temp_path)

    return(temp_path)
  }

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
      tidyr::drop_na(data)
    )
}
