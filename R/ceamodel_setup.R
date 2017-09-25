#' Create a ceamodel class object.
#' 
#' Creates a ceamodel class object that specifies cost and
#' effect data as well as intervention specifiers for a list of individual 
#' entities. The model may also specify covariate values to be used in 
#' estimating incremental costs and effects.
#' 
#' @param x an R object.
#' @return An object of class "ceamodel".
#' @examples 
#' ## Data Frame examples: basic, male as covariate
#' ceamodel <- cea_setup(clintrial_cea, "cost", "qaly", "treat")
#' ceamodel <- cea_setup(clintrial_cea, "cost", "qaly", "treat", "male")
#' 
#' ## Formulas examples: basic, male as covariate
#' ceamodel <- cea_setup(cost ~ treat, qaly ~ treat, clintrial_cea)
#' ceamodel <- cea_setup(cost ~ treat + male, qaly ~ treat + male, 
#'                       cea_data = clintrial_cea)
#' @export
cea_setup <- function(x, ...) UseMethod("cea_setup")

#' @param cst A vector of cost values.
#' @param eff A vector of effect values.
#' @param intv A vector of intervention or program assignment/membership.
#'         cst, eff, and intv must have the same length, greater than one.
#' @param covt If provided, a vector (or matrix) of covariate values. covt must
#'         have the same number of rows as cst and eff. The number of columns
#'         is equal to the number of unique covariates.
#' @param covt_cst If provided, a vector of integers. The integers correspond
#'         to column numbers of covt for which the covariates will be used for
#'         costs.
#' @param covt_eff If provided, a vector of integers. The integers correspond
#'         to column numbers of covt for which the covariates will be used for
#'         effects.
#' @param eff_more_better If TRUE, a greater value for effects indicates a
#'                    better outcome. If FALSE, a smaller value for effects
#'                    indicates a better outcome. Default is TRUE.
#' @param cst_char A character string representing the preferred name of the
#'             cost variable (values in cst).
#' @param eff_char A character string representing the preferred name of the
#'             effect variable (values in eff).
#' @param intv_char A character string representing the preferred name of the
#'              intervention variable (values in intv).
#' @param covt_char A character string (or vector of character strings)
#'                  representing the preferred name of the covariate
#'                  variables. The number of strings provided should equal
#'                  the number of columns in covt.
#' @describeIn cea_setup Default S3 method
#' @export
cea_setup.default <- function(cst, eff, intv,
                              covt = c(), covt_cst = c(), covt_eff = c(),
                              eff_more_better = TRUE, cst_char, eff_char,
                              intv_char, covt_char) {

  # if names are not provided for the cst, eff, and intv variables/vectors,
  #   names are automatically applied
  if (missing(cst_char)) cst_char <- "costs"
  if (missing(eff_char)) eff_char <- "effects"
  if (missing(intv_char)) intv_char <- "intervention"

  # check for a vector supplied to covt
  # if a vector is supplied, it automattically is applied as the covariates
  #   for both costs and effects, regardless of whether or not vectors
  #   were passed to covt_cst or covt_eff

  n_covt <- NCOL(covt)
  if (!is.null(covt)) {
    if (is.null(n_covt)) n_covt = 1
    if (n_covt == 1) covt <- data.frame(covt)
    covt_cst_data <- covt[, covt_cst]
    covt_cst_char <- covt_char[covt_cst]
    covt_eff_data <- covt[, covt_eff]
    covt_eff_char <- covt_char[covt_eff]
    if (is.null(covt_char)) {
      covt_char <- rep("", n_covt)
      for (i in 1:n_covt) {
        covt_char[i] <- paste("covt", i, sep = "_")
      }
    }
  } else {
    # do nothing, covt was not supplied
  }

  # check for covt_cst and covt_eff vectors
  #   these may have been created from covt in the previous code or
  #   these may have been provided in the function call

  n_covt_cst <- length(covt_cst)
  n_covt_eff <- length(covt_eff)

  # create the first instance of the ceamodel class object
  # the *_char objects are names of each of the objects
  cea = list(cst_char        = cst_char,
             eff_char        = eff_char,
             intv_char       = intv_char,
             covt_char       = covt_char,
             covt_cst_char   = covt_cst_char,
             covt_eff_char   = covt_eff_char,
             eff_more_better = eff_more_better)

  # a formula is created for both costs and effects based on variable names
  # in the ceamodel object
  cea$cst_formula <- paste(cea$cst_char, "~", cea$intv_char)
  if (is.null(n_covt_cst)) n_covt_cst <- 0
  if (n_covt_cst > 0) {
    tmp <- cea$covt_char[covt_cst]
    for (i in 1:n_covt_cst) {
      cea$cst_formula <- paste(cea$cst_formula, tmp[i], sep = " + ")
    }
  }
  cea$cst_formula <- as.formula(cea$cst_formula)
  cea$eff_formula <- paste(cea$eff_char, "~", cea$intv_char)
  if (is.null(n_covt_eff)) n_covt_eff <- 0
  if (n_covt_eff > 0) {
    tmp <- cea$covt_char[covt_eff]
    for (i in 1:n_covt_eff) {
      cea$eff_formula <- paste(cea$eff_formula, tmp[i], sep = " + ")
    }
  }
  cea$eff_formula <- as.formula(cea$eff_formula)

  # the dataframe used in analyses is now created using the vectors supplied
  #   in the function call
  # the order of variables in dataframe are the cost, effect, and intervention
  #   variables followed by the covariates (if supplied)
  if (n_covt > 0) {
    cea$cea_data <- data.frame(cst, eff, intv, covt)
  } else {
    cea$cea_data <- data.frame(cst, eff, intv)
  }

  # the names in the *_char elements are applied to the dataframe
  names(cea$cea_data) <- c(cea$cst_char, cea$eff_char, cea$intv_char,
                           cea$covt_char)

  # all rows are dropped from the dataframe for which a missing value occurs
  cea$cea_data <- cea$cea_data[complete.cases(cea$cea_data), ]

  # N_total is the total number of rows that will be included in future analyses
  cea$N_total <- nrow(cea$cea_data)

  # the ceamodel class is finalized and returned
  class(cea) <- "ceamodel"
  return(cea)

}

#' @describeIn cea_setup S3 method for class 'data.frame'
#' @export
cea_setup.data.frame <- function(cea_data=list(), cst_char, eff_char, intv_char,
                          covt_char_vec=c(), covt_cst=c(), covt_eff=c(),
                          eff_more_better=TRUE) {

  # If one or more cost covariates are provided, the names are saved and
  #   data passed to a vector/matirx from the dataframe
  if (!is.null(covt_cst)) {
    covt_cst_char <- covt_cst
    #covt_cst <- cea_data[, covt_char_vec[covt_cst]]
  }
  else {
    covt_cst_char <- c()
  }
  # If one or more effect covariates are provided, the names are saved and
  #   data passed to a vector/matirx from the dataframe
  if (!is.null(covt_eff)) {
    covt_eff_char <- covt_eff
    #covt_eff <- cea_data[, covt_char_vec[covt_eff]]
  }
  else {
    covt_eff_char <- c()
  }

  covt_char_vec <- unique(c(covt_cst, covt_eff))

  # call the default function above
  ceamodel_local <- cea_setup.default(cst             = cea_data[, cst_char],
                                      eff             = cea_data[, eff_char],
                                      intv            = cea_data[, intv_char],
                                      covt            = cea_data[, covt_char_vec],
                                      covt_cst        = which(covt_char_vec ==
                                                                covt_cst_char),
                                      covt_eff        = which(covt_char_vec ==
                                                                covt_eff_char),
                                      eff_more_better = eff_more_better,
                                      cst_char        = cst_char,
                                      eff_char        = eff_char,
                                      intv_char       = intv_char,
                                      covt_char       = covt_char_vec)

}

#' @describeIn cea_setup S3 method for class 'formula'
#' @export
cea_setup.formula <- function(formula_cst=formula, formula_eff=formula,
                              cea_data=list(), eff_more_better = TRUE) {

  # convert formulas to character vectors for parsing
  formula_cst_char <- as.character(formula_cst)
  formula_eff_char <- as.character(formula_eff)
  if (formula_cst_char[1] != "~") {
    # this actually fails at the point of entering the formula
    stop(paste("Formula does not inlcude a ~ character distinguishing costs and
               effects from intervention and covariate variables."))
  }
  if (formula_eff_char[1] != "~") {
    # this actually fails at the point of entering the formula
    stop(paste("Formula does not inlcude a ~ character distinguishing costs and
               effects from intervention and covariate variables."))
  }

  # the first part of the formula shows up in the second position of the vector
  # cst_char <- stringi::stri_trim_both(formula_cst_char[2])
  # eff_char <- stringi::stri_trim_both(formula_eff_char[2])
  cst_char <- trimws(formula_cst_char[2])
  eff_char <- trimws(formula_eff_char[2])
  # check for the correspondence in intervention variables between equations
  #   and check for covariates
  cst_char_vec <- strsplit(formula_cst_char[3], "+", fixed=TRUE)
  tmp_cst <- as.character(cst_char_vec[[1]])
  #int_cst_char <- stringi::stri_trim_both(cst_char_vec[[1]][1])
  # int_cst_char <- stringi::stri_trim_both(tmp_cst[1])
  int_cst_char <- trimws(tmp_cst[1])
  if (length(cst_char_vec[[1]]) > 1) {
    #cov_cst_vec <- stringi::stri_trim_both(cst_char_vec[[1]][2:length(cst_char_vec[[1]])])
    # cov_cst_vec <- stringi::stri_trim_both(tmp_cst[2:length(cst_char_vec[[1]])])
    cov_cst_vec <- trimws(tmp_cst[2:length(cst_char_vec[[1]])])
  } else {
    cov_cst_vec <- c()
  }

  # find covariates
  eff_char_vec <- strsplit(formula_eff_char[3], "+", fixed=TRUE)
  tmp_eff <- as.character(eff_char_vec[[1]])
  #int_eff_char <- stringi::stri_trim_both(eff_char_vec[[1]][1])
  # int_eff_char <- stringi::stri_trim_both(tmp_eff[1])
  int_eff_char <- trimws(tmp_eff[1])
  if (length(eff_char_vec[[1]]) > 1) {
    #cov_eff_vec <- stringi::stri_trim_both(eff_char_vec[[1]][2:length(eff_char_vec[[1]])])
    # cov_eff_vec <- stringi::stri_trim_both(tmp_eff[2:length(eff_char_vec[[1]])])
    cov_eff_vec <- trimws(tmp_eff[2:length(eff_char_vec[[1]])])
  } else {
    cov_eff_vec <- c()
  }

  if (int_cst_char != int_eff_char) {
    stop(paste("Cost and effect formulas have different
               intervention variables."))
  } else {
    int_char <- int_cst_char
  }

  covt_char_vec <- unique(c(cov_eff_vec, cov_cst_vec))
  var_char_vec <- c(cst_char, eff_char, int_char, covt_char_vec)

  # call default function to create ceamodel class object
  ceamodel_local <- cea_setup.default(cst             = cea_data[, cst_char],
                                      eff             = cea_data[, eff_char],
                                      intv            = cea_data[, int_char],
                                      covt            = cea_data[, covt_char_vec],
                                      covt_cst        = which(covt_char_vec ==
                                                                cov_cst_vec),
                                      covt_eff        = which(covt_char_vec ==
                                                                cov_eff_vec),
                                      eff_more_better = eff_more_better,
                                      cst_char        = cst_char,
                                      eff_char        = eff_char,
                                      intv_char       = int_char,
                                      covt_char       = covt_char_vec)
}

