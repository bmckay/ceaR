#' @export
create_inb_plot = function(inb_lst = list(),
                           title_lab="Incremental Net Benefit as a Function of WTP",
                           x_lab="WTP ($/effect)",
                           y_lab="Incremental Net Benefit",
                           file_name="create_inb_plot.jpg") {


  ggplot(data=inb_lst$inb_data, aes(x=b_lam)) +
    theme(panel.background = element_rect(fill=NA)) +
    labs(title=title_lab, x=x_lab, y=y_lab) +
    geom_line(aes(y=b_lam_inb), size=0.2) +
    geom_line(aes(y=b_lam_upper), size=0.2, linetype=2) +
    geom_line(aes(y=b_lam_lower), size=0.2, linetype=2) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    ggsave(file_name, plot=last_plot(), device="jpeg")
}

#' @export
create_ceac_plot <- function(x, ...) UseMethod("create_ceac_plot")

#' @export
create_ceac_plot.default <- function(ceac_props, lambda_vals,
                                     title_lab = "The Cost-Effectiveness Acceptability Curve",
                                     x_lab = "WTP ($/effect)",
                                     y_lab = "Probability of Cost-Effectiveness",
                                     file_name = "create_ceac_plot.jpg",
                                     legend_names = c(),
                                     lambda_max = NA) {

  nlines <- ncol(ceac_props)
  ceac_props.df <- data.frame(props = numeric(),
                              lambda = numeric(),
                              intv = factor(),
                              linetype = factor())
  for (i in 1:nlines) {
    ceac_props_tmp.df <- data.frame(props = ceac_props[, i],
                                    lambda = lambda_vals,
                                    intv = rep(paste("intv", i, sep=""),
                                               nrow(ceac_props)),
                                    linetype = rep(paste(i), nrow(ceac_props)))
    ceac_props.df <- rbind(ceac_props.df, ceac_props_tmp.df)
    rm(ceac_props_tmp.df)
  }

  if (is.null(legend_names)) legend_names <-
    paste("intv", seq.int(from = 1, to = nlines), sep="")

  plot_loc <- ggplot(data = ceac_props.df, aes(x = lambda, y = props,
                                               group = intv)) +
    theme(panel.background = element_rect(fill = NA), aspect.ratio = 9/16) +
    geom_line(aes(linetype = linetype)) +
    scale_linetype_discrete(name = NULL, position = "bottom", labels = legend_names) +
    labs(title = title_lab, x = x_lab, y = y_lab) +
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    ylim(0, 1.0) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)

  if (is.na(lambda_max)) plot_loc <- plot_loc + xlim(0, max(lambda_vals))
  else plot_loc <- plot_loc + xlim(0, lambda_max)

  ggsave(file_name, plot=last_plot(), device = "jpeg")
}

#' @export
create_ceac_plot <- function(inb_lst = list(),
                             title_lab = "The Cost-Effectiveness Acceptability Curve",
                             x_lab = "WTP ($/effect)",
                             y_lab = "Probability of Cost-Effectiveness",
                             file_name = "create_ceac_plot.jpg") {

  ggplot2::ggplot(data=inb_lst$inb_data, aes(x=b_lam, y=b_lam_prob)) +
    theme(panel.background = element_rect(fill=NA)) +
    labs(title=title_lab, x=x_lab, y=y_lab) +
    geom_line() +
    ylim(0, 1.0) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0)
  ggplot2::ggsave(file_name, plot=last_plot(), device="jpeg")

}
