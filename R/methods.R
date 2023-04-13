#' Print "cint" Object
#'
#' Print method for an object of class "cint".
#'
#' @param x A on object of class "cint".
#' @param digits Number of digits used to format numbers.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' ci_mean(1:100)
print.cint <- function(x, digits = getOption("digits"), ...) {
  # Method info
  cat("\n")
  cat(
    strwrap(
      paste(
        props2text(x$probs),
        format_p(diff(x$probs), digits = digits),
        x$type,
        "confidence interval for the",
        x$parameter,
        x$info
      ),
      prefix = "\t"
    ),
    sep = "\n"
  )
  cat("\n")

  # Estimate
  cat("Sample estimate:", format(x$estimate, digits = digits), "\n")

  # Confidence interval
  names(x$interval) <- format_p(x$probs, digits = digits)
  cat("Confidence interval:\n")
  print(x$interval[1:2], digits = digits)

  cat("\n")
  invisible(x)
}

#' Type Check
#'
#' Checks if an object inherits class "cint".
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' is.cint(ci_proportion(5, 20))
#' is.cint(c(1, 2))
is.cint <- function(x) {
  inherits(x, "cint")
}
