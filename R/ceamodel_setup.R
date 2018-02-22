#' Create a ceamodel class object.
#' 
#' Creates a ceamodel class object that specifies cost and
#' effect data as well as intervention specifiers for a list of individual 
#' entities. The model may also specify covariate values to be used in 
#' estimating incremental costs and/or effects.
#' 
#' @param x an R object.
#' @return An object of class "ceamodel".
#' @examples 
#' ## Data Frame examples: basic, male as covariate for effects only
#' ceamodel <- cea_setup(clintrial_cea, "cost", "qaly", "treat")
#' ceamodel <- cea_setup(clintrial_cea, "cost", "qaly", "treat", 
#' covt_eff = "male")
#' 
#' ## Formula examples: basic, male as covariate for effects only
#' ceamodel <- cea_setup(cost | qaly ~ 1, "treat", clintrial_cea)
#' ceamodel <- cea_setup(cost | qaly ~ 1 | male, "treat", clintrial_cea)
#' @export
cea_setup <- function(x, ...) UseMethod("cea_setup")

#' @param cst A vector of cost values.
#' @param eff A vector of effect values.
#' @param intv A vector of intervention or program assignment/membership.
#'         The vectors cst, eff, and intv must have the same length, greater 
#'         than one.
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
#' @param call.txt Only supplied when one of the cea_setup methods calls this
#'                 default function.
#' @param incremental Defaults to TRUE and runs incremental analysis upon setup. 
#' @param cost_order If true, the order of options in an ICER table will be by 
#' increasing average cost. If false, the order of options in an ICER table
#' will be by increasing average effect.
#' @param cst_type Defaults to linear regression (gaussian) but can be set
#'                 to any family object available to glm.
#' @param eff_type Defaults to linear regression (gaussian) but can be set
#'                 to any family object available to glm.
#' @param intv_cats A vector of data that classify the intervention options
#' @param intv_cats_char One or more variables that classify the intervention
#'                       options
#' @describeIn cea_setup Default S3 method
#' @export
cea_setup.default <- function(cst, eff, intv,
                              covt = c(), covt_cst = c(), covt_eff = c(),
                              eff_more_better = TRUE, cst_char, eff_char,
                              intv_char, covt_char, call.txt, 
                              incremental = TRUE, cost_order = TRUE,
                              cst_type = "gaussian", eff_type = "gaussian",
                              intv_cats, intv_cats_char = c()) {

  #### TO DO: Check intv_cats_char to ensure these are actual variables in cea_data
  
  # if names are not provided for the cst, eff, and intv variables/vectors,
  #   names are automatically applied
  if (missing(cst_char)) cst_char <- "costs"
  if (missing(eff_char)) eff_char <- "effects"
  if (missing(intv_char)) intv_char <- "intervention"
  #if (missing(covt_char)) covt_char <- rep()

  # check for a vector supplied to covt
  # if a vector is supplied, it automattically is applied as the covariates
  #   for both costs and effects, regardless of whether or not vectors
  #   were passed to covt_cst or covt_eff

  if (is.null(covt)) {
    n_covt <- 0
  }
  else if (is.null(NCOL(covt))) {
    n_covt <- 0
  }
  else {
    n_covt <- NCOL(covt)
  }
  #if (n_covt == 0) covt = NULL
  if (n_covt > 0) {
    #if (is.null(n_covt)) n_covt <- 1
    if (n_covt == 1) covt <- data.frame(covt)
    covt_cst_data <- dplyr::select(covt, covt_cst)
    covt_cst_char <- covt_char[covt_cst]
    covt_eff_data <- dplyr::select(covt, covt_eff)
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

  if (!exists("covt_cst_char")) covt_cst_char <- NULL
  if (!exists("covt_eff_char")) covt_eff_char <- NULL
  
  # create the first instance of the ceamodel class object
  # the *_char objects are names of each of the objects
  cea = list(cst_char        = cst_char,
             eff_char        = eff_char,
             intv_char       = intv_char,
             intv_cats_char  = intv_cats_char,
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
  if (!is.null(intv_cats_char)) {
    cea$cea_data <- cbind(cea$cea_data, intv_cats)
  }

  # the names in the *_char elements are applied to the dataframe
  names(cea$cea_data) <- c(cea$cst_char, cea$eff_char, cea$intv_char,
                           cea$covt_char, cea$intv_cats_char)

  # all rows are dropped from the dataframe for which a missing value occurs
  cea$cea_data <- cea$cea_data[complete.cases(cea$cea_data), ]

  # N_total is the total number of rows that will be included in future analyses
  cea$N_total <- nrow(cea$cea_data)

  # the ceamodel class is finalized and returned
  if (is.null(call)) {
    cea$call <- "Was NULL"
  } else {
    cea$call <- call.txt
  }
  if (length(unique(cea$cea_data[, cea$intv_char])) == cea$N_total) {
    class(cea) <- c("deterministic", "ceamodel")
  } else {
    class(cea) <- c("stochastic", "ceamodel")
    # cost regression (default now to linear regression)
    cea$reg.types <- c(cst_type, eff_type)
  }
  
  cea = ceamodel_incremental(cea_lst = cea, cost_order = cost_order)
  
  return(cea)

}

#' @describeIn cea_setup S3 method for class 'data.frame'
#' @export
cea_setup.data.frame <- function(cea_data = list(), cst_char, eff_char, 
                          intv_char,
                          covt_char_vec=c(), covt_cst=c(), covt_eff=c(),
                          eff_more_better=TRUE, incremental = TRUE, 
                          cost_order = TRUE,
                          cst_type = "gaussian", eff_type = "gaussian",
                          intv_cats, intv_cats_char = c()) {

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
                                      intv_cats_char       = intv_cats_char,
                                      covt            = cea_data[, covt_char_vec],
                                      covt_cst        = which(covt_char_vec %in%
                                                                covt_cst_char),
                                      covt_eff        = which(covt_char_vec %in%
                                                                covt_eff_char),
                                      eff_more_better = eff_more_better,
                                      cst_char        = cst_char,
                                      eff_char        = eff_char,
                                      intv_char       = intv_char,
                                      covt_char       = covt_char_vec,
                                      incremental     = incremental,
                                      cost_order      = cost_order,
                                      cst_type        = cst_type,
                                      eff_type        = eff_type)

}

#' @describeIn cea_setup S3 method for class 'formula'
#' @export
cea_setup.formula <- function(formula_cea = formula, intv, cea_data = list(), 
                              eff_more_better = TRUE, incremental = TRUE,
                              cost_order = TRUE,
                              cst_type = "gaussian", eff_type = "gaussian",
                              intv_cats = c()) {

  # convert formulas to character vectors for parsing
  formula_char <- as.character(formula_cea)
  if (formula_char[1] != "~") {
    # this actually fails at the point of entering the formula
    stop(paste("Formula does not inlcude a ~ character distinguishing costs and
               effects from covariate variables."))
  }
  if (length(formula_char) != 3) {
    stop(paste("Formula provided does not contain three parts. Should be 
               costs | effects ~ covariates (or 1 if no covariates)"))
  }
  
  # the first part of the formula (costs | effects) shows up in the second 
  # position of the vector
  csteff_char <- strsplit(formula_char[2], "|", fixed = TRUE)
  csteff_char <- trimws(csteff_char[[1]])
  if (length(csteff_char) < 2) {
    stop(paste("Formula does not provide a cost and effect variable separated
               by | to the left of the tilde ~."))
  } else if (length(csteff_char) > 2) {
    stop(paste("Formula should only include a cost and effect variable
               separated by | to the left of the tilde ~...nothing else"))
  }
  cst_char <- csteff_char[1]
  eff_char <- csteff_char[2]

  # the second part of the formula shows up in the third position of the vector
  cov_char <- strsplit(formula_char[3], "|", fixed = TRUE)
  cov_char <- trimws(cov_char[[1]])
  if (length(cov_char) == 1) {
    # in this situation there is either a "1" or a list of covariates that apply
    # to both costs and effects
    # check first for the no covariate situation
    if (cov_char == "1") {
      cst_cov_char <- NULL
      eff_cov_char <- NULL
    } else {
      cov_char <- strsplit(cov_char, "+", fixed = TRUE)
      cst_cov_char <- trimws(cov_char[[1]])
      eff_cov_char <- cst_cov_char
    }
  }
  else if (length(cov_char) == 2) {
    # in this situation there are separate covariates for costs and effects
    # may have a 1 in one or both positions
    # cost covariates first
    if (cov_char[1] == "1") {
      cst_cov_char <- NULL
    }
    else {
      cst_cov_char <- strsplit(cov_char[1], "+", fixed = TRUE)
      cst_cov_char <- trimws(cst_cov_char[[1]])
    }
    
    # effect covariates next
    if (cov_char[2] == "1") {
      eff_cov_char <- NULL
    }
    else {
      eff_cov_char <- strsplit(cov_char[2], "+", fixed = TRUE)
      eff_cov_char <- trimws(eff_cov_char[[1]])
    }
  }

  covt_char_vec <- unique(c(cst_cov_char, eff_cov_char))
  if (is.null(covt_char_vec)) {
    covt <- NULL
  }
  else {
    covt <- cea_data[, covt_char_vec]
  }
  
  # Now deal with the intervention variable passed for argument "intv"
  if (length(intv) > 1){
    stop(paste("Please pass only a single variable representing intervention or
               group assignment of observations."))
  }
  int_char <- trimws(intv)
  
  var_char_vec <- c(cst_char, eff_char, int_char, covt_char_vec)

  # call default function to create ceamodel class object
  ceamodel_local <- cea_setup.default(cst             = cea_data[, cst_char],
                                      eff             = cea_data[, eff_char],
                                      intv            = cea_data[, int_char],
                                      covt            = covt,
                                      covt_cst        = which(covt_char_vec %in%
                                                                cst_cov_char),
                                      covt_eff        = which(covt_char_vec %in%
                                                                eff_cov_char),
                                      eff_more_better = eff_more_better,
                                      cst_char        = cst_char,
                                      eff_char        = eff_char,
                                      intv_char       = int_char,
                                      covt_char       = covt_char_vec,
                                      call.txt        = match.call(),
                                      incremental     = incremental,
                                      cost_order      = cost_order,
                                      cst_type        = cst_type,
                                      eff_type        = eff_type,
                                      intv_cats       = cea_data[, intv_cats],
                                      intv_cats_char  = intv_cats)
}

#' @export
cea_setup_deterministic <- function(cea_data = list(), cst_char, eff_char, 
                                    intv_char, cst_stdev_char = c(), 
                                    eff_stdev_char, corr_char = c(),
                                    eff_more_better = TRUE, incremental = TRUE, 
                                    cost_order = TRUE) {
  
  if(!tibble::has_name(cea_data, cst_char)) {
    stop(paste("The variable name supplied to cst_char,", cst_char, ", does not
               exist in dataframe provided to cea_data."))
  }
  if(!tibble::has_name(cea_data, eff_char)) {
    stop(paste("The variable name supplied to eff_char,", eff_char, ", does not
               exist in dataframe provided to cea_data."))
  }
  if(!tibble::has_name(cea_data, cst_char)) {
    stop(paste("The variable name supplied to intv_char,", intv_char, ", does not
               exist in dataframe provided to cea_data."))
  }
  if(!is.null(cst_stdev_char) & !tibble::has_name(cea_data, cst_stdev_char)) {
    stop(paste("The variable name supplied to cst_stdev_char,", cst_stdev_char, 
               ", does not exist in dataframe provided to cea_data."))
  }
  if(!is.null(eff_stdev_char) & !tibble::has_name(cea_data, eff_stdev_char)) {
    stop(paste("The variable name supplied to eff_stdev_char,", eff_stdev_char, 
               ", does not exist in dataframe provided to cea_data."))
  }
  if(!is.null(corr_char) & !tibble::has_name(cea_data, corr_char)) {
    stop(paste("The variable name supplied to corr_char,", corr_char, 
               ", does not exist in dataframe provided to cea_data."))
  }
}
