#' Print information regarding a ceamodel object
#' 
#' @param x A ceamodel object
#' @export
print.ceamodel <- function(x) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Model:", paste(class(tmp)[1], sep = ""), "\n\n", sep = " ")
  if ("icer.table" %in% names(x)) {
    cat("\nICER Table:\n")
    print(x$icer.table)
    cat("\n\n")
  } else {
    cat("No incremental analysis results to report.\n")
  }
  cat("\n")
  invisible(x)
}