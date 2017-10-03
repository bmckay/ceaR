
# First, need a function that creates a single iteration of the bootstrap.
# It would be most helpful if this new dataset was then a CEA object so 
# that available functions in ceaR could be used for the analysis
# Second, different things can then be done to this iteration including
# * ICER regression analyses
# * INB regression analyses

boot_iter <- function(cea_lst, stratvars = c()) {
  
  # Should I create subsetted datasets based on what is in stratvars?
  # Then boot each subsetted dataset separately and combine results together?
  # Or loop over all possible combinations of strats and boot each
  
  return(boot_lst)
}

boot_nb <- function(cea_lst, n_boot = 100) {

  # Start by performing a bootstrap iteration of all variables in cea_data

  cea_boot <- data.frame()
  for (i in 1:n_boot) {

    # in each bootstrap iteration, create a sample of people from dataset
    # need to iterate over each intv_char

    cea_data_boot <- data.frame()
    for (j in 1:cea_lst$incremental$N_intv_vec) {
      cea_data_tmp <- cea_lst$cea_data[cea_lst$cea_data[, cea_lst$intv_char] ==
                                         cea_lst$incremental$intv_vec[j], ]

      cea_data_boot <- rbind(cea_data_boot,
                             cea_data_tmp[sample.int(nrow(cea_data_tmp),
                                                     replace = TRUE), ])

    }
    rm(cea_data_tmp)

    #cea_boot = sureg(cea_data_boot)
    ceamodel_tmp <- cea_setup.formula(cea_lst$cst_formula, cea_lst$eff_formula,
                                      cea_data_boot,
                                      eff_more_better = cea_lst$eff_more_better)
    ceamodel_tmp <- ceamodel_incremental(ceamodel_tmp, table_print = FALSE)

    cst_vec <- ceamodel_tmp$incremental$sureg$const_cst +
      c(0, ceamodel_tmp$incremental$sureg$inc_cst_vec) +
      sum(ceamodel_tmp$incremental$sureg$avg_covt_cst_vec*
            ceamodel_tmp$incremental$sureg$coef[((2*ceamodel_tmp$incremental$N_intv_vec)+
                                                   ceamodel_tmp$incremental$sureg$n_covt_eff+1):
                                             length(ceamodel_tmp$incremental$sureg$coef)])

    eff_vec <- ceamodel_tmp$incremental$sureg$const_eff +
      c(0, ceamodel_tmp$incremental$sureg$inc_eff_vec) +
      sum(ceamodel_tmp$incremental$sureg$avg_covt_eff_vec*
            ceamodel_tmp$incremental$sureg$coef[(ceamodel_tmp$incremental$N_intv_vec+1):
                                             (ceamodel_tmp$incremental$N_intv_vec+
                                                ceamodel_tmp$incremental$sureg$n_covt_eff)])

    cea_boot <- rbind(cea_boot, c(cst_vec, eff_vec))

  }
  names(cea_boot) <- c(paste("cst", cea_lst$incremental$intv_vec, sep = "_"),
                       paste("eff", cea_lst$incremental$intv_vec, sep = "_"))

  cea_lst$incremental$cea_boot <- cea_boot
  return(cea_lst)

}

boot_nb_ceac <- function(cea_lst, n_lam = 101, lam_upper = NA) {

  # Create data for CEAC
  if (is.na(lam_upper)) lam_upper <- 5 * max(cea_lst$icer.table[, "ICER"],
                                             na.rm = TRUE)
  lam_seq <- seq(from=0, to=lam_upper, length.out=n_lam)
  nintv = cea_lst$incremental$N_intv_vec
  ceac_props <- matrix(nrow = n_lam, ncol = nintv)

  ict = 1
  for (i in lam_seq) {
    if (cea_lst$eff_more_better) {
      nb_tmp <- (cea_lst$incremental$cea_boot[, seq.int(from = nintv + 1,
                                                        to = 2 * nintv)] * i) -
        cea_lst$incremental$cea_boot[, seq.int(from = 1, to = nintv)]
    } else {
      nb_tmp <- (-1.0 * cea_lst$incremental$cea_boot[, seq.int(from = nintv + 1,
                                                               to = 2 * nintv)] * i) -
        cea_lst$incremental$cea_boot[, seq.int(from = 1, to = nintv)]
    }

    nb_tmp$max_intv <- NA
    for (j in 1:nrow(nb_tmp)) {
      nb_tmp$max_intv[j] <- which.max(nb_tmp[j, c(1:nintv)])
    }

    for (j in 1:nintv) {
      ceac_props[ict, j] <- sum(nb_tmp$max_intv == j) / length(nb_tmp$max_intv)
    }


    ict <- ict + 1
  }
  ceac_props <- data.frame(ceac_props)
  ceac_props$lambda <- lam_seq
  names(ceac_props) <- c(paste(cea_lst$intv_char, cea_lst$incremental$intv_vec,
                               sep = ""), "lambda")

  return(ceac_props)
}
