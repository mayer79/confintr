#' Standard error of variance
#'
#' Internal function to calculate the standard error for the variance. It is used in \code{resample::bootstrap} and needs to be exported for this purpose.
#'
#' @importFrom stats var
#' @param z A numeric vector.
#' @return A numeric vector of length 1.
#' @export
#' @examples
#' stderr_var(1:100)
stderr_var <- function(z) {
  n <- length(z)
  var_of_var <- (1 / n) * (moment(z, p = 4) - (n - 3) / (n - 1) * var(z)^2)
  sqrt(var_of_var)
}
