#' Print an icertable object
#' 
#' @param x An icertable object
#' @export
print.icertable <- function(x, ...) {

  cat("\nICER Table:\n")
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
  cat("Model:", paste(class(tmp)[1], sep = ""), "\n\n", sep = " ")
  if ("icer.table" %in% names(x)) {
    print(x$icer.table, ...)
    cat("\n")
  } else {
    cat("No incremental analysis results to report.\n")
  }
  cat("\n")
  invisible(x)
}

#' Default plotting model for a ceamodel object
#' 
#' @param x A ceamodel object
#' @export
plot.ceamodel <- function(x) {
  
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
  
  ggplot2::ggplot(data = x.df, aes(x = eff, y = cst)) +
    theme(panel.background = element_rect(fill = NA)) +
    geom_point(aes(color = factor(dom_txt))) +
    geom_text(aes(label = int), nudge_x = 0.05, size = 3) +
    labs(title = "Default Plot for Object of Class \"ceamodel\"", 
         x = "Incremental Effects", y = "Incremental Costs") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_path(aes(x = eff_path, y = cst_path)) +
    #guide_legend("Option Dominance") +
    ggsave("plot.ceamodel.jpg", plot = last_plot(), device = "jpeg")
}