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
                          ci_level=95, return_sureg = FALSE) {

  # Check for a ceamodel object be passed as cea_lst, stop execution if not
  if (!("ceamodel" %in% class(cea_lst))) {
    stop(paste("The object passed as cea_lst is not a ceamodel object."))
  }
  # Next check for the object incremental in cea_lst, if this does not exist
  # run cea_incremental first
  if (!("ceamodel_incremental" %in% class(cea_lst$incremental)) & !return_sureg) {
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
  
  # Set the universal CI level z value based on ci_level
  cea_alt_lst$incremental$ci_level <- ci_level
  z_val <- get_zval(cea_alt_lst$incremental$ci_level)
  
  if (!("ceamodel_incremental" %in% class(cea_lst$incremental))) {
    cea_alt_lst$incremental$intv_vec <- rep(NA, cea_lst$incremental$N_intv_vec)
    cea_alt_lst$incremental$intv_vec[1] <- intv1
    cea_alt_lst$incremental$intv_vec[2] <- intv2
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
    # and the variable int1 will represent the incremental difference btw the two
    # Should be able to run everything else as before
    cea_alt_lst$incremental$sureg <- sureg(cea_alt_lst)
    
    if (return_sureg) return(cea_alt_lst$incremental$sureg)
  }

  # Start Fieller solution to ICER CI
  intv1_char <- paste(cea_lst$intv_char, intv1, sep = "_")
  intv2_char <- paste(cea_lst$intv_char, intv2, sep = "_")
  intv_char <- ifelse(intv1 > intv2, paste(intv2, intv1, sep = "_"), 
                      paste(intv1, intv2, sep = "_"))
  eff1 <- cea_lst$icer.table[intv1_char, "Effect"]
  eff2 <- cea_lst$icer.table[intv2_char, "Effect"]
  cst1 <- cea_lst$icer.table[intv1_char, "Cost"]
  cst2 <- cea_lst$icer.table[intv2_char, "Cost"]
  inc_eff <- ifelse(cea_lst$incremental$cost_order, 
                    ifelse(cst1 > cst2, eff1 - eff2, eff2 - eff1), 
                    ifelse(eff1 > eff2, eff1 - eff2, eff2 - eff1))
  inc_eff <- ifelse(cea_lst$eff_more_better, inc_eff, -1 * inc_eff)
  inc_cst <- ifelse(cea_lst$incremental$cost_order, 
                    ifelse(cst1 > cst2, cst1 - cst2, cst2 - cst1), 
                    ifelse(eff1 > eff2, cst1 - cst2, cst2 - cst1))
  
  var_inc_eff <- cea_lst$incremental$var_inc_eff_vec[intv_char]
  var_inc_cst <- cea_lst$incremental$var_inc_cst_vec[intv_char]
  covt_inc_cea <- cea_lst$incremental$cov_cea_vec[intv_char]
  
  ae <- var_inc_eff / inc_eff^2
  ac <- var_inc_cst / inc_cst^2
  aec <- covt_inc_cea /(inc_eff * inc_cst)

  icer <- inc_cst / inc_eff

  # less cost-effective side (lower, upper)
  lower.tmp <- ((1 - (z_val^2) * aec-z_val *
                   sqrt(ae + ac - 2 * aec - (z_val^2) * (ae * ac - aec^2))) /
                  (1 - (z_val^2) * ae))
  upper.tmp <- ((1 - (z_val^2)*aec+z_val *
                   sqrt(ae + ac - 2 * aec - (z_val^2) * (ae * ac - aec^2))) /
                  (1 - (z_val^2) * ae))
  icer_ci_fieller <- icer * c(lower.tmp, upper.tmp)

  # Set up regression coefficient table for incremental costs and effects
  icer_coef <- matrix(c(inc_cst, sqrt(var_inc_cst), inc_cst / sqrt(var_inc_cst), 
                      2 * pt(-1 * abs(inc_cst / sqrt(var_inc_cst)), 
                             df = cea_lst$N_total - 1),
                      inc_eff, sqrt(var_inc_eff), inc_eff / sqrt(var_inc_eff), 
                      2 * pt(-1 * abs(inc_eff / sqrt(var_inc_eff)), 
                             df = cea_lst$N_total - 1)),
                      nrow = 2, ncol = 4, byrow = TRUE)
  colnames(icer_coef) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(icer_coef) <- c("Inc. Costs", "Inc. Effects")
  
  icer_lst <- list(cst_char = cea_lst$cst_char,
                   eff_char = cea_lst$eff_char,
                   intv_char = cea_lst$intv_char,
                   covt_char = cea_lst$covt_char,
                   covt_cst_char = cea_lst$covt_cst_char,
                   covt_eff_char = cea_lst$covt_eff_char,
                   eff_more_better = cea_lst$eff_more_better,
                   cst_formula = cea_lst$cst_formula,
                   eff_formula = cea_lst$eff_formula,
                   N_total = cea_lst$N_total,
                   reg.types = cea_lst$reg.types,
                   icer_intv = c(intv1, intv2),
                   icer_coef = icer_coef,
                   var_inc_eff = var_inc_eff,
                   var_inc_cst = var_inc_cst,
                   covt_inc_cea = covt_inc_cea,
                   icer = icer,
                   icer_ci = icer_ci_fieller,
                   icer_ci_type = "fieller",
                   icer.table = cea_lst$icer.table,
                   ci_level = ci_level,
                   call = match.call())
  if ("stochastic" %in% class(cea_lst)) {
    class(icer_lst) <- c("stochastic", "icermodel")
  } 
  else if ("deterministic" %in% class(cea_lst)) {
    class(icer_lst) <- c("deterministic", "icermodel")
  }
  else {
    class(icer_lst) <- c("icermodel")
  }
  
  # to set up the bow tie ICER confidence region, need to do the following
  # determine x/y beginning and end points
  # (really just x then get the y values for upper and lower from these)
  # make x twice the value of the base icer x with mirror image negative values
  if (fieller_plot) {
    fieller_ci_plot(icer_ci_fieller, inc_eff, inc_cst, save.plot = TRUE)
  }
  return(icer_lst)
  
  # TODO: what steps to take if inb or boot are set to TRUE
  # if(inb) {
  #   cea_alt_lst$incremental$sureg$inc.cea$inc.inb = run.inb(cea_alt_lst, intv1, intv2)
  # }
  #
  # if (boot) {
  #   cea_alt_lst$incremental$sureg$inc.cea$inc.boot = run.inc.boot(cea_alt_lst, intv1, intv2)
  # }
  #

}
