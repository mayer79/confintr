#' Print a cint Object
#'
#' Print method for an object of class \code{cint}.
#'
#' @param x A on object of class \code{cint}.
#' @param digits Number of digits used to format sample estimate and confidence limits.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @method print cint
#' @export
#' @examples
#' ci_mean(1:100)
print.cint <- function(x, digits = getOption("digits"), ...) {
  # Method info
  cat("\n")
  cat(strwrap(paste(props2text(x$probs), format_p(diff(x$probs), digits = digits),
              x$type, "confidence interval for the", x$parameter, x$info),
      prefix = "\t"), sep = "\n")
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
