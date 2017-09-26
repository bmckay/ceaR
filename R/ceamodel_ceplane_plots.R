create_scatter_plot <- function(cea_lst = ceamodel(),
                                title_lab = "",
                                x_lab = "Incremental Effects",
                                y_lab = "Incremental Costs",
                                file_name = "create_scatter_plot.jpg",
                                incremental = TRUE) {
browser()
#  nintv <- ncol(boot_data) / 2
  if (incremental) {

    scatter_data <- data.frame(inc_cst <- numeric(),
                               inc_eff <- numeric(),
                               intv <- factor(),
                               dottype <- factor())

    if (cea_lst$incremental$cost_order) {
      intv0 <- which.min(tapply(cea_lst$cea_data[, cea_lst$cst_char],
                                cea_lst$cea_data[, cea_lst$intv_char],
                                FUN = mean))
    } else {
      if (cea_lst$eff_more_better) {
        intv0 <- which.min(tapply(cea_lst$cea_data[, cea_lst$eff_char],
                                  cea_lst$cea_data[, cea_lst$intv_char],
                                  FUN = mean))
      } else {
        intv0 <- which.max(tapply(cea_lst$cea_data[, cea_lst$eff_char],
                                  cea_lst$cea_data[, cea_lst$intv_char],
                                  FUN = mean))
      }

    }
  #### problem here is that the above statements may not work when regressions
    ### find different min or max values than simple analysis
    intv_vec <- cea_lst$incremental$intv_vec[cea_lst$incremental$intv_vec !=
                                               intv0]
    for (i in intv_vec) {
      scatter_data_tmp <- data.frame(inc_cst <- numeric(),
                                     inc_eff <- numeric(),
                                     intv <- factor(),
                                     dottype <- factor())

      scatter_data_tmp$inc_cst <- cea_lst$incremental$cea_boot[, paste("cst", intv_vec[i], sep = "_")] -
        cea_lst$incremental$cea_boot[, paste("cst", intv0, sep = "_")]
      if (cea_lst$eff_more_better) {
        scatter_data_tmp$inc_eff <- cea_lst$incremental$cea_boot[, paste("eff", intv_vec[i], sep = "_")] -
          cea_lst$incremental$cea_boot[, paste("eff", intv0, sep = "_")]
      } else {
        scatter_data_tmp$inc_eff <- cea_lst$incremental$cea_boot[, paste("eff", intv0, sep = "_")] -
          cea_lst$incremental$cea_boot[, paste("eff", intv_vec[i], sep = "_")]
      }
      scatter_data_tmp$intv <- rep(paste(intv_vec[i]),
                                   nrow(cea_lst$incremental$cea_boot))
      scatter_data_tmp$dottype <- rep(paste(intv_vec[i]),
                                      nrow(cea_lst$incremental$cea_boot))

      scatter_data <- rbind(scatter_data, scatter_data_tmp)
    }

  } else {

  }
browser()
  plot_loc <- ggplot(data = boot_data)
}

create_icer_plot <- function(slopes_vec, intercept_vec, xlim_vec=c(),
                             ylim_vec=c(), icer_marker=c(), title_lab="",
                             x_lab="Incremental Effects",
                             y_lab="Incremental Costs",
                             file_name="create_icer_plot.jpg",
                             shade_df=NA) {

  plot_loc <- ggplot(data = data.frame(x=c(icer_marker[1]),
                                       y=c(icer_marker[2])),
                     aes(x, y)) +
    theme(panel.background = element_rect(fill = NA))

  if (is.data.frame(shade_df)) plot_loc <- plot_loc +
      geom_polygon(data=shade_df, mapping=aes(x=x, y=y, group=t), fill="grey80")

  plot_loc <- plot_loc + geom_point() +
    labs(title=title_lab, x=x_lab, y=y_lab) +
    geom_abline(slope=slopes_vec[1], intercept=intercept_vec[1], size=0.1) +
    geom_abline(slope=slopes_vec[2], intercept=intercept_vec[2], size=0.1) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    xlim(xlim_vec[1], xlim_vec[2]) +
    ylim(ylim_vec[1], ylim_vec[2])

  ggsave(file_name, plot=last_plot(), device="jpeg")
}

fieller_ci_plot <- function(x, ...) UseMethod("fieller_ci_plot")

fieller_ci_plot.default = function(icer_ci_fieller_vec, inc_cst, inc_eff,
                                   title_lab="Bow Tie ICER Confidence Region
                                   (Fieller's theorem)",
                                   x_lab="Incremental Effects",
                                   y_lab="Incremental Costs",
                                   file_name="create_fieller_ci_plot.jpg") {

  x_max <- 2*inc_eff
  xlim_vec <- c(-1.0*x_max, x_max)
  y_max <- ifelse(icer_ci_fieller_vec[2]>0, max(icer_ci_fieller_vec*x_max),
                  2*inc_cst)
  ylim_vec <- c(-1.0*y_max, y_max)

  shade <- data.frame(x=c(0, xlim_vec[2], xlim_vec[2],
                          ylim_vec[2]/icer_ci_fieller_vec[2],
                          0, ylim_vec[1]/icer_ci_fieller_vec[2],
                          xlim_vec[1], xlim_vec[1]),
                      y=c(0, icer_ci_fieller_vec[1]*xlim_vec[2],
                          ylim_vec[2], ylim_vec[2],
                          0, ylim_vec[1], ylim_vec[1],
                          icer_ci_fieller_vec[1]*xlim_vec[1]),
                      t=c('a', 'a', 'a', 'a', 'b', 'b', 'b', 'b'))

  create_icer_plot(slopes_vec=icer_ci_fieller_vec, intercept_vec=c(0,0),
                   xlim_vec=xlim_vec, ylim_vec=ylim_vec,
                   icer_marker=c(inc_eff, inc_cst),
                   title_lab=title_lab, x_lab=x_lab,
                   y_lab=y_lab, file_name=file_name, shade_df=shade)
}

fieller_ci_plot.ceamodel = function(cea_lst,
                                    title_lab="Bow Tie ICER Confidence Region
                                               (Fieller's theorem)",
                                    x_lab="Incremental Effects",
                                    y_lab="Incremental Costs",
                                    file_name="create_fieller_ci_plot.jpg") {

  fieller_ci_plot.default(cea_lst$incremental$icer_ci_fieller,
                          cea_lst$incremental$inc_cst,
                          cea_lst$incremental$inc_eff,
                          title_lab, x_lab, y_lab, file_name)
}
