#' Returns a Z-value based on confidence level
#' 
#' Returns a Z-value for use in confidence interval construction based on
#' a given level of confidence. The level of confidence is passed as an integer
#' representing the level of confidence. 
#' 
#' @param ci_level Level of confidence. The default value is 95, representing
#' a 95% level of confidence or an alpha of 0.05.
#' @return Z-value for the standard normal distribution
get_zval <- function(ci_level=95) {
  qnorm((1 - (100 - ci_level) / 200))
}
