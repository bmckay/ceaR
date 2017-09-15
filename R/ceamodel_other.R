get_zval <- function(ci_level=95) {
  qnorm((1 - (100 - ci_level) / 200))
}
