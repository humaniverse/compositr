#' Inverse rank
#' 
#' \code{inverse_rank} returns the inverse rank of a vector (i.e., the
#' highest value receives a rank of 1).
#' 
#' @param vec Vector to be inverse ranked.
#' 
#' @export 
#' 
#' @examples
#' inverse_rank(c(1:3))

inverse_rank <-
  function(vec) {
    (length(vec) + 1) - rank(vec, na.last = FALSE)
  }