#' Type Check
#'
#' Checks if an object inherits class 'cint'.
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' is.cint(ci_proportion(5, 20))
is.cint <- function(x) {
  inherits(x, "cint")
}

