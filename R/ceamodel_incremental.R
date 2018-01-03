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
#' ceamodel <- cea_setup(cost ~ treat, qaly ~ treat, clintrial_cea, 
#' incremental = FALSE)
#' ceamodel <- ceamodel_incremental(ceamodel)
#' ## Or as one step
#' ceamodel <- cea_setup(cost ~ treat, qaly ~ treat, clintrial_cea)
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
    cea_lst$incremental$intv_vec <-
      sort(unique(cea_lst$cea_data[, cea_lst$intv_char]))
    cea_lst$incremental$N_intv_vec <- length(cea_lst$incremental$intv_vec)
    
    # Create a unique binary variable for each value in intervention variable
    cea_lst$incremental$intv_vec_char <- c()
    for (i in 2:cea_lst$incremental$N_intv_vec) {
      cea_lst$cea_data[,paste(cea_lst$intv_char, 
                              cea_lst$incremental$intv_vec[i], sep="_")] <-
        ifelse(cea_lst$cea_data[, cea_lst$intv_char] ==
                 cea_lst$incremental$intv_vec[i], 1, 0)
      cea_lst$incremental$intv_vec_char <-
        c(cea_lst$incremental$intv_vec_char, 
          paste(cea_lst$intv_char, cea_lst$incremental$intv_vec[i], sep="_"))
    }
    
    # Need to run a cost regression and an effect regression, save both
    # then run sureg, if appropriate, to get the covariance in incremental
    # costs and effects
    # Need to create appropriate regression formulas using new binary variables
    
    # Cost regression first
    tmp_formula <- create_formula(cea_lst$cst_char, 
                                  cea_lst$incremental$intv_vec_char, 
                                  cea_lst$covt_cst_char)
    cea_lst$cst_reg <- glm(tmp_formula, family = cea_lst$reg.types[1], 
                           data = cea_lst$cea_data)
    tmp <- summary(cea_lst$cst_reg)
    cea_lst$cst_reg_coef <- tmp$coefficients
    
    # Effect regression next
    tmp_formula <- create_formula(cea_lst$eff_char, 
                                  cea_lst$incremental$intv_vec_char, 
                                  cea_lst$covt_eff_char)
    cea_lst$eff_reg <- glm(tmp_formula, family = cea_lst$reg.types[2], 
                           data = cea_lst$cea_data)
    tmp <- summary(cea_lst$eff_reg)
    cea_lst$eff_reg_coef <- tmp$coefficients
    
    # Based on model specified in cea_lst (ceamodel object) and using newly
    #   created binary intervention variables, construct and run a seemingly
    #   unrelated regression model, the first step toward an incremental analysis
    if (cea_lst$reg.types[1] == "gaussian" &
        cea_lst$reg.types[2] == "gaussian") {
      cea_lst$incremental$cov_cea_vec <- 
        rep(NA, choose(cea_lst$incremental$N_intv_vec, 2))
      cea_lst$incremental$var_inc_eff_vec <- 
        rep(NA, length(cea_lst$incremental$cov_cea_vec))
      cea_lst$incremental$var_inc_cst_vec <- 
        rep(NA, length(cea_lst$incremental$cov_cea_vec))
      names_tmp <- rep(NA, length(cea_lst$cov_cea_vec))
      ct = 1
      for (i in 1:(cea_lst$incremental$N_intv_vec - 1)) {
        ival <- cea_lst$incremental$intv_vec[i]
        for (j in (i + 1):cea_lst$incremental$N_intv_vec) {
          jval <- cea_lst$incremental$intv_vec[j]
          tmp <- icer_analysis(cea_lst, ival, jval, return_sureg = TRUE)
          cea_lst$incremental$cov_cea_vec[ct] <- tmp$covt_inc_ce_vec[1]
          names_tmp[ct] <- paste(ival, jval, sep = "_")
          cea_lst$incremental$var_inc_eff_vec[ct] <- tmp$var_inc_eff_vec[1]
          cea_lst$incremental$var_inc_cst_vec[ct] <- tmp$var_inc_cst_vec[1]
          ct = ct + 1
        }
      }
      names(cea_lst$incremental$cov_cea_vec) <- names_tmp
      names(cea_lst$incremental$var_inc_eff_vec) <- names_tmp
      names(cea_lst$incremental$var_inc_cst_vec) <- names_tmp
    }
    class(cea_lst$incremental) <- "ceamodel_incremental"

    # Use the results of the regression analysis to determine average cost values
    #   by unique intervention variable value
    if (length(cea_lst$covt_cst_char)>0) {
      avg_covt_cst_vec <- rep(NA, length(cea_lst$covt_cst_char))
      for (i in 1:length(cea_lst$covt_cst_char)) {
        avg_covt_cst_vec[i] <- mean(cea_lst$cea_data[, cea_lst$covt_cst_char[i]])
     }
    }
    else {
      avg_covt_cst_vec <- c()
   }

    if (length(cea_lst$covt_eff_char)>0) {
      avg_covt_eff_vec <- rep(NA, length(cea_lst$covt_eff_char))
      for (i in 1:length(cea_lst$covt_eff_char)) {
        avg_covt_eff_vec[i] <- mean(cea_lst$cea_data[, cea_lst$covt_eff_char[i]])
      }
    }
    else {
      avg_covt_eff_vec <- c()
    }

    cst_vec <- cea_lst$cst_reg$coefficients[1] +
      c(0, cea_lst$cst_reg$coefficients[c(2:cea_lst$incremental$N_intv_vec)]) +
      sum(avg_covt_cst_vec * 
            cea_lst$cst_reg$coefficients[c((1 + cea_lst$incremental$N_intv_vec):
                                            length(cea_lst$cst_reg$coefficients))])
    
    eff_vec <- cea_lst$eff_reg$coefficients[1] +
      c(0, cea_lst$eff_reg$coefficients[c(2:cea_lst$incremental$N_intv_vec)]) +
      sum(avg_covt_eff_vec * 
            cea_lst$eff_reg$coefficients[c((1 + cea_lst$incremental$N_intv_vec):
                                            length(cea_lst$eff_reg$coefficients))])
      
    # Create unique intervention names
    intv_names <- paste(cea_lst$intv_char, cea_lst$incremental$intv_vec, sep="_")
  }
  
  



  # Construct an ICER table based on the results of the regression analysis
  cea_lst$icer.table = create_icer_table(cst_vec, eff_vec, intv_names,
                                         cea_lst$eff_more_better,
                                         cea_lst$incremental$cost_order,
                                         table_print = table_print)

  # Return the cea_lst object with the incremental analyses added to the object
  return(cea_lst)
}

