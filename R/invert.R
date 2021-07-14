#' Invert rank
#'
#' \code{invert_rank} returns the inverse rank of a vector (i.e., the
#' highest value receives a rank of 1).
#'
#' @param vec Vector to be inverse ranked.
#'
#' @export
#'
#' @examples
#' invert_rank(c(1:3))
invert_rank <-
  function(vec) {
    (length(vec) + 1) - rank(vec, na.last = FALSE)
  }

#' Invert this
#'
#' \code{invert_this} returns the inverse of a vector. For example, a decile of
#' 10 becomes 1, and a decile 1 becomes 10. NA's are removed by default.
#'
#' @param vec Vector to be inverted.
#'
#' @export
#'
#' @examples
#' invert_this(c(1:3))
invert_this <-
  function(vec) {
    (max(vec, na.rm = TRUE) + 1) - vec
  }