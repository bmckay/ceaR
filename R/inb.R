#' Run an incremental net benefit analysis
#' 
#' must have an object from ceamodel_incremental run first
#' if icer_analysis has been run then intv1 and intv2 exist and will be pulled
#' 
#' @param ceamodel Object of class ceamodel
#' @param intv1 Reference to one of the conditions specified in the intervention
#' variable of cea_lst
#' @param intv1 Reference to one of the conditions specified in the intervention
#' variable of cea_lst, should be different from intv1
#' @param n_lam Number of lambda values to run in analysis
#' @param lam_upper Upper limit of lambda values to run in analysis
#' @return An object of class XXX
#'
#' @export
inb <- function(ceamodel=list(), intv1=NA, intv2=NA, n_lam=101, lam_upper=NA) {

  if (is.null(ceamodel$incremental$intv1) |
      is.null(ceamodel$incremental$intv2)) {
    if (is.na(intv1) | is.na(intv2)) {
      stop(paste("The object supplied for ceamodel does not have an incremental
                 analysis or a specific set of interventions to compare."))
    } else {
      ceamodel$incremental$intv1 = intv1
      ceamodel$incremental$intv2 = intv2
    }
  }

  z_val <- get_zval(ceamodel$incremental$ci_level)
  # b = inc_eff * lambda - inc_cst
  # V(b) = (lambda^2)*V(inc_eff) + V(inc_cst) - 2*lambda*C(inc_eff, inc_cst)
  # confidence limits are b +/- 1_96*sqrt(V(b))
  # lambda are x points then would have a b_lam, b_lam_upper, b_lam_lower lines
  if (is.na(lam_upper)) lam_upper <- 5 * ceamodel$incremental$icer
  inc_eff <- ceamodel$incremental$inc_eff
  inc_cst <- ceamodel$incremental$inc_cst
  var_inc_eff <- ceamodel$incremental$var_inc_eff
  var_inc_cst <- ceamodel$incremental$var_inc_cst
  covt_inc_cea <- ceamodel$incremental$covt_inc_cea

  lam_seq <- seq(from=0, to=lam_upper, length.out=n_lam)
  b_lam <- rep(NA, n_lam)
  b_lam_inb <- rep(NA, n_lam)
  b_lam_var <- rep(NA, n_lam)
  b_lam_upper <- rep(NA, n_lam)
  b_lam_lower <- rep(NA, n_lam)
  b_lam_prob <- rep(NA, n_lam)

  for (i in 1:n_lam) {
    b_lam[i] <- lam_seq[i]
    b_lam_inb[i] <- lam_seq[i] * inc_eff - inc_cst
    b_lam_var[i] <- (lam_seq[i] ^ 2) * var_inc_eff + var_inc_cst -
      (2 * lam_seq[i] *covt_inc_cea)
    b_lam_upper[i] <- b_lam_inb[i] - z_val * sqrt(b_lam_var[i])
    b_lam_lower[i] <- b_lam_inb[i] + z_val * sqrt(b_lam_var[i])
    b_lam_prob[i] <- pnorm(b_lam_inb[i] / sqrt(b_lam_var[i]))
  }


  # Estimate the confidence intervals for the ICER
  # Start with the lower
  #   if starts negative and becomes positive than can estiamte directly
  #   if starts positive then need to run negative lambdas to find lower bound
  if (b_lam_lower[1] < 0 & b_lam_lower[n_lam] > 0) {
    tmp_val <- which(((b_lam_lower / abs(b_lam_lower)) +
                        c((b_lam_lower / abs(b_lam_lower))[2:n_lam], 0))==0)
    x1 <- b_lam[tmp_val]
    y1 <- b_lam_lower[tmp_val]
    x2 <- b_lam[tmp_val + 1]
    y2 <- b_lam_lower[tmp_val + 1]
    # simple method will be to solve for x-intercept on this equation of a line
    icer_lower <- x1 - (y1/((y2 - y1)/(x2 - x1)))
  }
  else {
    # simple method to solve for x-intercept using first two points
    x1 <- b_lam[1]
    y1 <- b_lam_lower[2]
    x2 <- b_lam[1]
    y2 <- b_lam_lower[2]
    # simple method will be to solve for x-intercept on this equation of a line
    icer_lower <- x1 - (y1 / ((y2 - y1)/(x2 - x1)))
  }

  if (b_lam_upper[n_lam] > 0) {
    # able to find upper limit as it crosses the x-axis
    tmp_val <- which(((b_lam_upper / abs(b_lam_upper)) +
                        c((b_lam_upper / abs(b_lam_upper))[2:n_lam], 0))==0)
    x1 <- b_lam[tmp_val]
    y1 <- b_lam_upper[tmp_val]
    x2 <- b_lam[tmp_val+1]
    y2 <- b_lam_upper[tmp_val+1]
    # simple method will be to solve for x-intercept on this equation of a line
    icer_upper <- x1 - (y1/((y2 - y1)/(x2 - x1)))
  }
  else {
    # cannot find upper limit yet, will first choose a much larger lambda value and check for a positive value
    # failure to find a positive value will assume tha that it is in dominated region
    b_lam_tmp <- b_lam[n_lam] * 10
    b_lam_inb_tmp <- b_lam_tmp * inc_eff - inc_cst
    b_lam_var_tmp <- (b_lam_tmp ^ 2) * var_inc_eff + var_inc_cst -
      2 * b_lam_tmp * covt_inc_cea
    b_lam_upper_tmp <- b_lam_inb_tmp - z_val*sqrt(b_lam_var_tmp)
    if (b_lam_upper_tmp > 0) {
      # figure out intercept
      b_lam_tmp <- lam_seq[n_lam]
      lam_step <- b_lam_tmp - lam_seq[n_lam-1]
      b_lam_upper_tmp <- b_lam_upper[n_lam]
      while (b_lam_upper_tmp < 0) {
        b_lam_upper_prev <- b_lam_upper_tmp
        b_lam_tmp <- lam_curr + lam_step
        b_lam_inb_tmp <- b_lam_tmp * inc_eff - inc_cst
        b_lam_var_tmp <- (b_lam_tmp ^ 2)*var_inc_eff + var_inc_cst -
          2 * b_lam_tmp * covt_inc_cea
        b_lam_upper_tmp <- b_lam_inb_tmp - z_val * sqrt(b_lam_var_tmp)
      }
      x2 <- b_lam_tmp
      y2 <- b_lam_upper_tmp
      x1 <- x2 - lam_step
      y1 <- b_lam_upper_prev

      # simple method will be to solve for x-intercept on this equation of a line
      icer_upper <- x1 - (y1 / ((y2 - y1)/(x2 - x1)))
    }
    else {
      icer_upper <- NA
    }
  }

  ceamodel$incremental$inb <- list(inb_data=data.frame(b_lam = b_lam,
                                    b_lam_inb = b_lam_inb,
                                    b_lam_var = b_lam_var,
                                    b_lam_upper = b_lam_upper,
                                    b_lam_lower = b_lam_lower,
                                    b_lam_prob = b_lam_prob),
                n_lam = n_lam,
                intv1 = intv1,
                intv2 = intv2,
                icer_lower = icer_lower,
                icer_upper = icer_upper)


  # create_inb_plot(inc_inb)
  # create_ceac_plot(inc_inb)

  return(ceamodel)
}
