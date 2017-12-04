#' Conduct basic incremental analyses using a ceamodel object and setup more
#' advanced incremental analyses.
#' 
#' @param cea_lst A ceamodel object created using a ceamodel_setup function.
#' @param cost_order If true, the order of options in an ICER table will be by 
#' increasing average cost. If false, the order of options in an ICER table
#' will be by increasing average effect.
#' @param table_print If true, the ICER table will be printed. If false, the
#' ICER table will not be printed.
#' @return An object of class ceamodel, with incremental analyses added to the
#' model
#' @examples 
#' ## Run an incremental analysis
#' ceamodel <- cea_setup(cost ~ treat, qaly ~ treat, clintrial_cea)
#' ceamodel <- ceamodel_incremental(ceamodel)
#' @export
ceamodel_incremental <- function(cea_lst = list(), cost_order = TRUE,
                                 table_print = TRUE) {

  # Check for a ceamodel object be passed as cea_lst, stop execution if not
  if (!("ceamodel" %in% class(cea_lst))) {
    stop(paste("The object passed as cea_lst is not a ceamodel object."))
  }
  
  # Set cost_order preference in the ceamodel object
  ### TODO may drop the class ceaicer line
  cea_lst$incremental <- list(cost_order = cost_order)
  class(cea_lst$incremental) <- "ceaicer"

  if ("deterministic" %in% class(cea_lst)) {
    # Only need to create cst_vec and eff_vec from data and intv_names
    cst_vec <- cea_lst$cea_data[, cea_lst$cst_char]
    eff_vec <- cea_lst$cea_data[, cea_lst$eff_char]
    intv_names <- cea_lst$cea_data[, cea_lst$intv_char]
    
  } else if ("stochastic" %in% class(cea_lst)) {
    # Create intervention related variables for regression analyses
    # Create vector and determine length of unique values in intervention variable
    #browser()
    cea_lst$incremental$intv_vec <-
      sort(unique(cea_lst$cea_data[, cea_lst$intv_char]))
    cea_lst$incremental$N_intv_vec <- length(cea_lst$incremental$intv_vec)
    
    # Create a unique binary variable for each value in intervention variable
    cea_lst$incremental$intv_vec_char <- c()
    for (i in 2:cea_lst$incremental$N_intv_vec) {
      cea_lst$cea_data[,paste("int", i-1, sep="")] <-
        ifelse(cea_lst$cea_data[, cea_lst$intv_char] ==
                 cea_lst$incremental$intv_vec[i], 1, 0)
      cea_lst$incremental$intv_vec_char <-
        c(cea_lst$incremental$intv_vec_char, paste("int", i-1, sep=""))
    }
    
    # Based on model specified in cea_lst (ceamodel object) and using newly
    #   created binary intervention variables, construct and run a seemingly
    #   unrelated regression model, the first step toward an incremental analysis
    cea_lst$incremental$sureg <- sureg(cea_lst)
    
    # Use the results of the regression analysis to determine average cost values
    #   by unique intervention variable value
    cst_vec <- cea_lst$incremental$sureg$const_cst +
      c(0, cea_lst$incremental$sureg$inc_cst_vec) +
      sum(cea_lst$incremental$sureg$avg_covt_cst_vec *
            cea_lst$incremental$sureg$coef[((2*cea_lst$incremental$N_intv_vec) +
                                              cea_lst$incremental$sureg$n_covt_eff+1):
                                             length(cea_lst$incremental$sureg$coef)])
    
    # Use the results of the regression analysis to determine average effect 
    #   values by unique intervention variable value
    eff_vec <- cea_lst$incremental$sureg$const_eff +
      c(0, cea_lst$incremental$sureg$inc_eff_vec) +
      sum(cea_lst$incremental$sureg$avg_covt_eff_vec*
            cea_lst$incremental$sureg$coef[(cea_lst$incremental$N_intv_vec+1):
                                             (cea_lst$incremental$N_intv_vec+
                                                cea_lst$incremental$sureg$n_covt_eff)])
    
    # Create unique intervention names
    intv_names <- paste(cea_lst$intv_char, cea_lst$incremental$intv_vec, sep="")
  }
  
  



  # Construct an ICER table based on the results of the regression analysis
  cea_lst$icer.table = create_icer_table(cst_vec, eff_vec, intv_names,
                                         cea_lst$eff_more_better,
                                         cea_lst$incremental$cost_order,
                                         table_print = table_print)

  # Return the cea_lst object with the incremental analyses added to the object
  return(cea_lst)
}

