#' Returns a Z-value based on confidence level
#' 
#' Returns a Z-value for use in confidence interval construction based on
#' a given level of confidence. The level of confidence is passed as an integer
#' representing the level of confidence. 
#' 
#' @param ci_level Level of confidence. The default value is 95, representing
#' a 95 percent level of confidence or an alpha of 0.05.
#' @return Z-value for the standard normal distribution
get_zval <- function(ci_level=95) {
  qnorm((1 - (100 - ci_level) / 200))
}

#' Creates a formula to be used in ceaR functions
#' 
#' Creates a formula for used in various ceaR functions based on a dependent
#' variable, intervention variable, and covariates. Each argument should be 
#' passed as a character vector and the object returned is of class formula.
#' 
#' @param dep Dependent variable
#' @param intv Intervention variable
#' @param covt Covariate variables
create_formula <- function(dep, intv, covt) {

  if (is.null(covt) | length(covt) == 0) {
    rtn <- as.formula(paste(dep, "~", paste(intv, collapse = " + "), 
                                  sep = " "))
  }
  else {
    rtn <- as.formula(paste(dep, "~", paste(intv, collapse = " + "), "+", 
                            paste(covt, collapse = " + ")))
  }
  return(rtn)
}
