#' Conduct a two-part model for excess zeros in cost models
#' 
#' @export
twopart_model <- function(formula, family = gaussian, data) {
  
  F1 <- Formula::Formula(formula)
  mf1 <- model.frame(F1, data = data)

  depvar <- as.character(attr(F1, "lhs")[[1]])
  mf1$pos_ind <- ifelse(mf1[, depvar] == 0, 0, 1)
  logit.formula <- as.formula(paste("pos_ind ~", 
                                    terms(F1, lhs = FALSE, rhs = TRUE)[[2]]))
  
  mf1_pos <- dplyr::filter(mf1, pos_ind == 1)
  
  logistic.fit <- glm(logit.formula, mf1, family = binomial)
  gamma.fit <- glm(formula, mf1_pos, family = Gamma(link = "log"))
  
  
  coefs <- list(vals = rep(NA, 2))
  coefs$vals[2] <- (plogis(coef(logistic.fit)[[1]] + coef(logistic.fit)[[2]]) * 
                      exp(coef(gamma.fit)[[1]] + coef(gamma.fit)[[2]]))
  
  coefs$vals[1] <- (plogis(coef(logistic.fit)[[1]]) * exp(coef(gamma.fit)[[1]]))
  coefs$diff <- coefs$vals[2] - coefs$vals[1]
  
  return(coefs)
}
