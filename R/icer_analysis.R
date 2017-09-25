#' Statistical analysis on an ICER
#' 
#' Runs a statistical analysis on an ICER value of interest following an
#' incremental analysis. The ICER of interest is selected by specifying the
#' two conditions of interest with intv1 and intv2.
#' 
#' @param cea_lst A ceamodel class object created using a ceamodel_setup function.
#' If a ceaicer object is not included (from ceamodel_incremental), this will
#' be run first before an analysis of the ICER.
#' @param intv1 Reference to one of the conditions specified in the intervention
#' variable of cea_lst
#' @param intv1 Reference to one of the conditions specified in the intervention
#' variable of cea_lst, should be different from intv1
#' @param fieller_plot If TRUE, create and save a Fieller confidence interval
#' plot for the ICER
#' @param inb If TRUE, run an incremental net-benefit analysis
#' @param boot If TRUE, run a bootstrap analysis
#' @param ci_level, Percent level for the confidence interval, default at 95%
#' confidence interval.
#' @export
icer_analysis <- function(cea_lst=list(), intv1, intv2, fieller_plot = FALSE,
                          pvalues = FALSE, inb = FALSE, boot = FALSE, 
                          ci_level=95) {

  # Check for a ceamodel object be passed as cea_lst, stop execution if not
  if (!class(cea_lst) == "ceamodel") {
    stop(paste("The object passed as cea_lst is not a ceamodel object."))
  }
  # Next check for the object incremental in cea_lst, if this does not exist
  # run cea_incremental first
  if (!("incremental" %in% names(cea_lst))) {
    cea_lst <- ceamodel_incremental(cea_lst, table_print = FALSE)
  }
  
  # Goal here is to create an incremental analysis for specified intervention 
  # variables, those specified by intv1 and intv2
  # Check to see if these intervention values exist in the dataset, quit if not
  cea_alt_lst <- cea_lst
  if (!(intv1 %in% cea_lst$incremental$intv_vec) | 
      !(intv2 %in% cea_lst$incremental$intv_vec))
  {
    stop(paste("The value specified in either intv1 or intv2 does not exist."))
  }
  cea_alt_lst$incremental$intv1 <- intv1
  cea_alt_lst$incremental$intv2 <- intv2

  # Set the universal CI level z value based on ci_level
  cea_alt_lst$incremental$ci_level <- ci_level
  z_val <- get_zval(cea_alt_lst$incremental$ci_level)

  # create intervention related variables
  #N.intv_vec = length(intv_vec)

  cea_alt_lst$incremental$intv_vec <- rep(NA, cea_lst$incremental$N_intv_vec)
  cea_alt_lst$incremental$intv_vec[1] <-
    cea_alt_lst$incremental$intv1
  cea_alt_lst$incremental$intv_vec[2] <-
    cea_alt_lst$incremental$intv2
  if (cea_alt_lst$incremental$N_intv_vec>2) {
    cea_alt_lst$incremental$intv_vec[3:cea_alt_lst$incremental$N_intv_vec] <-
      cea_lst$incremental$intv_vec[cea_lst$incremental$intv_vec!=intv1 &
                                     cea_lst$incremental$intv_vec!=intv2]
  }

  cea_alt_lst$intv_vec_char = c()

  cea_alt_lst$incremental$intv_vec_char = c()
  for (i in 2:cea_alt_lst$incremental$N_intv_vec) {
    cea_alt_lst$cea_data[,paste("int", i-1, sep="")] <-
      ifelse(cea_alt_lst$cea_data[, cea_alt_lst$intv_char] ==
               cea_alt_lst$incremental$intv_vec[i], 1, 0)
    cea_alt_lst$incremental$intv_vec_char <-
      c(cea_alt_lst$incremental$intv_vec_char, paste("int", i-1, sep=""))
  }

  # At this point the two selected intervention variables are now the reference
  # and the variable int1 will represent the incrmental difference btw the two
  # Should be able to run everything else as before
  cea_alt_lst$incremental$sureg <- sureg(cea_alt_lst)

  cst_vec <- cea_alt_lst$incremental$sureg$const_cst +
    c(0, cea_alt_lst$incremental$sureg$inc_cst_vec) +
    sum(cea_alt_lst$incremental$sureg$avg_covt_cst_vec*
          cea_alt_lst$incremental$sureg$coef[((2*cea_alt_lst$incremental$N_intv_vec)+
                                            cea_alt_lst$incremental$sureg$n_covt_eff+1):
                                           length(cea_alt_lst$incremental$sureg$coef)])

  eff_vec <- cea_alt_lst$incremental$sureg$const_eff +
    c(0, cea_alt_lst$incremental$sureg$inc_eff_vec) +
    sum(cea_alt_lst$incremental$sureg$avg_covt_eff_vec *
          cea_alt_lst$incremental$sureg$coef[(cea_alt_lst$incremental$N_intv_vec + 1):
                                            (cea_alt_lst$incremental$N_intv_vec +
                                              cea_alt_lst$incremental$sureg$n_covt_eff)])

    intv_names <- paste(cea_alt_lst$intv_char, cea_alt_lst$incremental$intv_vec, sep="")
  icer_table <- create_icer_table(cst_vec, eff_vec, intv_names,
                                  cea_alt_lst$eff_more_better,
                                  cea_alt_lst$incremental$cost_order,
                                  table_print = FALSE)

  # Start Fieller solution to ICER CI
### remove .'s from below
  cea_alt_lst$incremental$inc_eff <- ifelse(cea_alt_lst$eff_more_better,
                   cea_alt_lst$incremental$sureg$inc_eff_vec[1],
                   -1*cea_alt_lst$incremental$sureg$inc_eff_vec[1])
  cea_alt_lst$incremental$inc_cst <-
    cea_alt_lst$incremental$sureg$inc_cst_vec[1]
  cea_alt_lst$incremental$var_inc_eff <-
    cea_alt_lst$incremental$sureg$var_inc_eff_vec[1]
  cea_alt_lst$incremental$var_inc_cst <-
    cea_alt_lst$incremental$sureg$var_inc_cst_vec[1]
  cea_alt_lst$incremental$covt_inc_cea <-
    cea_alt_lst$incremental$sureg$covt_inc_ce_vec[1]

  ae <- cea_alt_lst$incremental$var_inc_eff / cea_alt_lst$incremental$inc_eff^2
  ac <- cea_alt_lst$incremental$var_inc_cst / cea_alt_lst$incremental$inc_cst^2
  aec <- cea_alt_lst$incremental$covt_inc_cea /
    (cea_alt_lst$incremental$inc_eff * cea_alt_lst$incremental$inc_cst)

  cea_alt_lst$incremental$icer <-
    cea_alt_lst$incremental$inc_cst / cea_alt_lst$incremental$inc_eff

  # less cost-effective side (lower, upper)
  lower.tmp <- ((1 - (z_val^2) * aec-z_val *
                   sqrt(ae + ac - 2 * aec - (z_val^2) * (ae * ac - aec^2))) /
                  (1 - (z_val^2) * ae))
  upper.tmp <- ((1 - (z_val^2)*aec+z_val *
                   sqrt(ae + ac - 2 * aec - (z_val^2) * (ae * ac - aec^2))) /
                  (1 - (z_val^2) * ae))
  cea_alt_lst$incremental$icer_ci_fieller <-
    cea_alt_lst$incremental$icer * c(lower.tmp, upper.tmp)

  if (pvalues) {
    cea_alt_lst$incremental$inc_cst_pval <- 2 * pt(-1 * 
      abs(cea_alt_lst$incremental$inc_cst /
        sqrt(cea_alt_lst$incremental$var_inc_cst)), df = cea_alt_lst$N_total - 1)
    cea_alt_lst$incremental$inc_eff_pval <- 2 * pt(-1 * 
      abs(cea_alt_lst$incremental$inc_eff /
        sqrt(cea_alt_lst$incremental$var_inc_eff)), df = cea_alt_lst$N_total - 1)
  }
  
  # Add note about negative values and interpretation
  writeLines(paste("\nICER for", cea_alt_lst$incremental$intv1,
                   "vs.", cea_alt_lst$incremental$intv2, "=",
                   round(cea_alt_lst$incremental$icer, 2), "\n"))
  if (pvalues) {
    writeLines(paste("\nP-value for cost coefficient = ", 
                     round(cea_alt_lst$incremental$inc_cst_pval, 4), "\n"))
    writeLines(paste("P-value for effect coefficient = ", 
                     round(cea_alt_lst$incremental$inc_eff_pval, 4), "\n"))
  }
  writeLines(paste("\nICER ", cea_alt_lst$incremental$ci_level,
                   "% Confidence Interval (Fieller's Method)\n    ", "(",
                   round(cea_alt_lst$incremental$icer_ci_fieller[1], 2), ", ",
                   round(cea_alt_lst$incremental$icer_ci_fieller[2], 2),
                   ")\n", sep=""))

  # to set up the bow tie ICER confidence region, need to do the following
  # determine x/y beginning and end points
  # (really just x then get the y values for upper and lower from these)
  # make x twice the value of the base icer x with mirror image negative values
  if (fieller_plot) {
    fieller_ci_plot(cea_alt_lst$incremental$icer_ci_fieller,
                    inc_eff=cea_alt_lst$incremental$inc_eff,
                    inc_cst=cea_alt_lst$incremental$inc_cst)
  }

  # TODO: what steps to take if inb or boot are set to TRUE
  # if(inb) {
  #   cea_alt_lst$incremental$sureg$inc.cea$inc.inb = run.inb(cea_alt_lst, intv1, intv2)
  # }
  #
  # if (boot) {
  #   cea_alt_lst$incremental$sureg$inc.cea$inc.boot = run.inc.boot(cea_alt_lst, intv1, intv2)
  # }
  #
  return(cea_alt_lst)

}
