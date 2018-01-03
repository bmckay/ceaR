#' Print an icertable object
#' 
#' @param x An icertable object
#' @export
print.icertable <- function(x, ...) {

  cat("\nICER Table:\n\n")
  x.tmp <- as.matrix(x)
  class(x.tmp) <- "matrix"
  print(x.tmp, ..., na.print = "")
  cat("Dominance: 1=intervention/option strongly dominated,
           2=intervention/option weakly dominated")
  cat("\n")
}

#' Print information regarding a ceamodel object
#' 
#' @param x A ceamodel object
#' @export
print.ceamodel <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Model:", paste(class(x)[1], sep = ""), "\n\n", sep = " ")
  if ("stochastic" %in% class(x)) {
    cat("Cost Regression:\n\n")
    printCoefmat(x$cst_reg_coef)
    cat("\nEffect Regression:\n\n")
    printCoefmat(x$eff_reg_coef)
  }
  if ("icer.table" %in% names(x)) {
    print(x$icer.table, ...)
    cat("\n")
  } else {
    cat("No incremental analysis results to report.\n")
  }
  cat("\n")
  invisible(x)
}

#' Print information regarding an icermodel object
#' 
#' @param x An icermodel object
#' @export
print.icermodel <- function(x, ...) {
  
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Model:", paste(class(x)[1], sep = ""), "\n\n", sep = " ")
  
  # Add note about negative values and interpretation
  intv_char <- paste(x$intv_char, x$icer_intv, sep = "_")
  cat("\nICER for", paste(intv_char[1], intv_char[2], sep = " vs. "), "=", 
      paste(round(x$icer, 2)), "\n")
  # if (pvalues) {
  #   writeLines(paste("\nP-value for cost coefficient = ", 
  #                    round(inc_cst_pval, 4), "\n"))
  #   writeLines(paste("P-value for effect coefficient = ", 
  #                    round(inc_eff_pval, 4), "\n"))
  # }
  cat("\nICER ", x$ci_level, paste("% Confidence Interval (Method = ", 
                                   x$icer_ci_type), ")\n    ", "(", 
      paste(round(x$icer_ci[1], 2), ", ", 
            round(x$icer_ci[2], 2), sep = ""), 
      ")\n", sep="")
  
  cat("\nIncremental Regression Coefficient Table: \n\n")
  print(x$icer_coef)
  
  if ("icer.table" %in% names(x)) {
    print(x$icer.table, ...)
    cat("\n")
  }
}

#' Default plotting model for a ceamodel object
#' 
#' @param x A ceamodel object
#' @export
plot.ceamodel <- function(x, save.plot = FALSE, ...) {
  
  # for this really want to reorient all data to baseline intervention,
  # not necessarily what is in the icer table
  x.df <- data.frame(eff = x$icer.table[, "Effect"],
                     cst = x$icer.table[, "Cost"],
                     int = row.names(x$icer.table),
                     dom = x$icer.table[, "Dominance"])
  base_eff <- x.df$eff[1]
  base_cst <- x.df$cst[1]
  x.df$eff <- x.df$eff - base_eff
  x.df$cst <- x.df$cst - base_cst
  x.df$dom[is.na(x.df$dom)] <- 0
  x.df$eff_path <- ifelse(x.df$dom == 0, x.df$eff, NA)
  x.df$cst_path <- ifelse(x.df$dom == 0, x.df$cst, NA)
  x.df$dom_txt <- ifelse(x.df$dom == 0, "Not Dominated",
                         ifelse(x.df$dom == 1, "Strictly Dominated",
                                "Weakly Dominated"))
  
  plot_loc <- ggplot2::ggplot(data = x.df, mapping = ggplot2::aes(x = eff, y = cst)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(dom_txt))) +
    ggplot2::geom_text(ggplot2::aes(label = int), nudge_x = 0, size = 3) +
    ggplot2::labs(title = "Default Plot for Object of Class \"ceamodel\"", 
                  x = "Incremental Effects", y = "Incremental Costs") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_path(ggplot2::aes(x = eff_path, y = cst_path))
    #guide_legend("Option Dominance") +
  if (save.plot)  {
    ggplot2::ggsave("plot.ceamodel.jpg", plot = plot_loc, 
                    device = "jpeg")
  }
  return(plot_loc)
}