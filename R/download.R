#' Download a file temporarily to disk
#'
#' @param url A URL for the request
#' @param file_extension A character vector detailing the file extension (e.g.,
#'         ".xlsx")
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