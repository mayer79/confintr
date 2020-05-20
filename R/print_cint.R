#' Print a cint Object
#'
#' Print type for an object of class \code{cint}.
#'
#' @param x A on object of class \code{cint}.
#' @param digits Number of digits used to format sample estimate and confidence limits.
#' @param ... Further arguments passed from other types.
#' @return Invisibly, the input is returned.
#' @method print cint
#' @export
#' @examples
#' ci_mean(1:100)
print.cint <- function(x, digits = getOption("digits"), ...) {
  # Calculate prefix from "probs" used in types info
  if (is_onesided(x$probs)) {
    prefx <- "One-sided"
  } else if (!is_symmetric(x$probs)) {
    prefx <- "Asymmetric two-sided"
  } else {
    prefx <- "Two-sided"
  }

  # type info
  cat("\n")
  cat(strwrap(paste(prefx, .format_p(diff(x$probs), digits = digits),
              x$type, "confidence interval for the", x$parameter, x$info),
      prefix = "\t"), sep = "\n")
  cat("\n")

  # Estimate
  cat("Sample estimate:", format(x$estimate, digits = digits), "\n")

  # Confidence interval
  names(x$interval) <- .format_p(x$probs, digits = digits)
  cat("Confidence interval:\n")
  print(x$interval[1:2], digits = digits)

  cat("\n")
  invisible(x)
}

# Helper functions
.format_p <- function(z, digits = max(2L, getOption("digits"))) {
  paste0(format(100 * z, digits = digits), "%")
}

boot_info <- function(type, boot_type, R) {
  if (type == "bootstrap") {
    sprintf("based on %s bootstrap samples and the '%s' method", R, boot_type)
  }
}

is_symmetric <- function(probs) {
  isTRUE(all.equal(probs[1], 1 - probs[2]))
}

is_onesided <- function(probs) {
  any(probs %in% 0:1)
}
