#' Conduct a seemingly unrelated regression analysis for incremental cost-
#' effectiveness analyses.
#' 
#' @param mf_lst A cea model object with incremental analysis specified.
#' @param intv1 Here
#' @param intv2 Here
#' 
#' @return An sureg model object

sureg = function(mf_lst=list(), intv1=NA, intv2=NA) {

  # Put together matrices for the seemingly unrelated regression analysis
  # X matrix is combination of intervention variable, constants, and covariates for costs and effects
  # To solve when there are more than one intervention variable, need to set this up as multiple binary variables in the ceamodel_frame function
  Z_mat <- cbind(rep(1, mf_lst$N_total), mf_lst$cea_data[, mf_lst$incremental$intv_vec_char], mf_lst$cea_data[, mf_lst$covt_eff_char])
  Z_mat <- matrix(as.numeric(unlist(Z_mat)), nrow=nrow(Z_mat))
  W_mat <- cbind(rep(1, mf_lst$N_total), mf_lst$cea_data[, mf_lst$incremental$intv_vec_char], mf_lst$cea_data[, mf_lst$covt_cst_char])
  W_mat <- matrix(as.numeric(unlist(W_mat)), nrow=nrow(W_mat))
  X_mat <- rbind(cbind(Z_mat, matrix(0, nrow=nrow(Z_mat), ncol=ncol(W_mat))), cbind(matrix(0, nrow=nrow(W_mat), ncol=ncol(Z_mat)), W_mat))

  y_vec <- c(mf_lst$cea_data[, mf_lst$eff_char], mf_lst$cea_data[, mf_lst$cst_char])

  # glmfit <- glm.fit(X_mat, y_vec)
  # glmfit$call <- "glm.fit(X_mat, y_vec)"
  # glmfit$method <- "glm.fit"
  # class(glmfit) <- c(glmfit$class, c("glm", "lm"))
  # names(glmfit$coefficients) <- c("Eff: (Intercept)", 
  #                                 mf_lst$incremental$intv_vec_char, 
  #                                 mf_lst$covt_eff_char, "Cst: (Intercept)", 
  #                                 mf_lst$incremental$intv_vec_char, 
  #                                 mf_lst$covt_cst_char)
  # glmfit

  qx <- qr(X_mat)
  coef <- solve.qr(qx, y_vec)
  coef_alt <- solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%y_vec
  df_eff <- mf_lst$N_total - ncol(Z_mat)
  df_cst <- mf_lst$N_total - ncol(W_mat)
  sigma2 <- sum((y_vec - X_mat%*%coef)^2)/max(df_eff, df_cst)

  res_vec <- (diag(2*mf_lst$N_total) - (X_mat%*%solve(t(X_mat)%*%X_mat)%*%t(X_mat))) %*% y_vec
  var_eff <- sum(res_vec[1:(mf_lst$N_total)]*res_vec[1:(mf_lst$N_total)]) / df_eff
  var_cst <- sum(res_vec[(mf_lst$N_total+1):(2*mf_lst$N_total)]*res_vec[(mf_lst$N_total+1):(2*mf_lst$N_total)]) / df_cst
  var_ce <- sum(res_vec[1:(mf_lst$N_total)]*res_vec[(mf_lst$N_total+1):(2*mf_lst$N_total)]) / max(df_cst, df_eff)
  sigma_mat <- matrix(c(var_eff, var_ce, var_ce, var_cst), nrow=2)
  var_coef_mat <- solve(t(X_mat)%*%kronecker(solve(sigma_mat),diag(mf_lst$N_total))%*%X_mat)
  mf_lst$var_coef_mat <- var_coef_mat

  # subtract off the number of interventions, covers the N_intv_vec-1 intervention variables and the constant
  n_covt_eff <- ncol(Z_mat)-mf_lst$incremental$N_intv_vec
  n_covt_cst <- ncol(W_mat)-mf_lst$incremental$N_intv_vec

  var_inc_eff_vec <- rep(NA, mf_lst$incremental$N_intv_vec-1)
  var_inc_cst_vec <- rep(NA, mf_lst$incremental$N_intv_vec-1)
  covt_inc_ce_vec <- rep(NA, mf_lst$incremental$N_intv_vec-1)
  for (i in 1:(mf_lst$incremental$N_intv_vec-1)) {
    var_inc_eff_vec[i] <- var_coef_mat[i+1,i+1]
    var_inc_cst_vec[i] <- var_coef_mat[mf_lst$incremental$N_intv_vec+n_covt_eff+i+1, mf_lst$incremental$N_intv_vec+n_covt_eff+i+1]
    covt_inc_ce_vec[i] <- var_coef_mat[i+1, mf_lst$incremental$N_intv_vec+n_covt_eff+i+1]
  }
  mf_lst$incremental$var_inc_eff_vec <- var_inc_eff_vec
  mf_lst$incremental$var_inc_cst_vec <- var_inc_cst_vec
  mf_lst$incremental$covt_inc_ce_vec <- covt_inc_ce_vec
  
  coef_gls <- solve(t(X_mat)%*%kronecker(solve(sigma_mat),diag(mf_lst$N_total))%*%X_mat)%*%t(X_mat)%*%kronecker(solve(sigma_mat),diag(mf_lst$N_total))%*%y_vec

  # At this point, the model is solved, now to create a return list similar to glm
  # To match structure of glm, list elements should be the following
  #   coefficients: vector with names (N.coeff)
  #   residuals: vector (N=)
  #   fitted.values: vector (N=)
  #   effects: vector (N=)
  #   R: matrix (N.coeffxN.coeff)
  #   rank: single number
  #   call, formula, terms
  # Change everything to create an sureg object that is saved in cea
  sureg <- list()
  sureg$coefficients <- coef
  names(sureg$coefficients) <- c(mf_lst$incremental$intv_vec_char,
                                 "(Intercept)",
                                 mf_lst$incremental$covt_eff_char,
                                 mf_lst$incremental$intv_vec_char,
                                 "(Intercept)",
                                 mf_lst$incremental$covt_cst_char)

  # coef vector is ordered as effects first and costs second
  # within effects and costs, the intervention 0/1 come first followed by the constant term, and then the covariates
  # N.int.vec is the costant term for effects
  # (2*N.int.vec)+N.cov.eff is the constant term for costs
  # there's a need to correct for covariates when getting the base values (not incremental)
  # get average of each covariate and add to icer.table
  if (length(mf_lst$covt_cst_vec)>0) {
    avg_covt_cst_char <- rep(NA, length(mf_lst$covt_cst_char))
    for (i in 1:length(mf_lst$covt_cst_char)) {
      avg_covt_cst_vec[i] <- mean(mf_lst$cea_data[, mf_lst$covt_cst_char[i]])
    }
  }
  else {
    avg_covt_cst_vec <- c()
  }

  if (length(mf_lst$covt_eff_char)>0) {
    avg_covt_eff_vec <- rep(NA, length(mf_lst$covt_eff_char))
    for (i in 1:length(mf_lst$covt_eff_char)) {
      avg_covt_eff_vec[i] <- mean(mf_lst$cea_data[, mf_lst$covt_eff_char[i]])
    }
  }
  else {
    avg_covt_eff_vec <- c()
  }

  inc_cst_vec <- coef[(mf_lst$incremental$N_intv_vec+n_covt_eff+1):(2*mf_lst$incremental$N_intv_vec+n_covt_eff-1)]

  inc_eff_vec <- coef[1:(mf_lst$incremental$N_intv_vec-1)]

  const_cst <- coef[2*mf_lst$incremental$N_intv_vec+n_covt_eff]

  const_eff <- coef[mf_lst$incremental$N_intv_vec]

  rank <- c(mf_lst$incremental$N_intv_vec + n_covt_cst + 1,
            mf_lst$incremental$N_intv_vec + n_covt_eff + 1)

  df.residual <- c((length(y_vec) / 2) -
    (mf_lst$incremental$N_intv_vec + n_covt_cst + 1),
    (length(y_vec) / 2) -
    (mf_lst$incremental$N_intv_vec + n_covt_eff + 1))

  # TODO Create this as an sureg model object and consider additional
  #    regression diagnostics to include in the returned object
  # Should make this comparable to what comes from a glm
  reg_lst <- list(coef              = coef,
                  coef_gls          = coef_gls,
                  var_eff           = var_eff,
                  var_cst           = var_cst,
                  var_ce            = var_ce,
                  var_coef_mat      = var_coef_mat,
                  n_covt_eff        = n_covt_eff,
                  n_covt_cst        = n_covt_cst,
                  var_inc_eff_vec   = var_inc_eff_vec,
                  var_inc_cst_vec   = var_inc_cst_vec,
                  covt_inc_ce_vec   = covt_inc_ce_vec,
                  avg_covt_cst_vec  = avg_covt_cst_vec,
                  avg_covt_eff_vec  = avg_covt_eff_vec,
                  inc_cst_vec       = inc_cst_vec,
                  inc_eff_vec       = inc_eff_vec,
                  const_cst         = const_cst,
                  const_eff         = const_eff,
                  rank              = rank,
                  df.residual       = df.residual)

  return(reg_lst)
}

