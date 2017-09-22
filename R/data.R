#' Example CEA clinical trial data
#' 
#' A dataset that includes cost, QALYs, and six explanatory variables that
#' can be used to conduct multivariable analyses of cost and QALYs by
#' generalized linear models and can also be used for other example CEAs. This
#' dataset is provided by the Health Services Research Unit and used in the text
#' "Economic Evaluation in Clinical Trials" by Glick HA, Doshi JA, Sonnad SS,
#' and Polsky D. Oxford University Press, 2007.
#' 
#' @format A data frame with 500 rows and 9 variables:
#' \describe{
#'   \item{id}{unique id for each patient in trial}
#'   \item{treat}{intervention variable, 1 for treatment, 0 for control}
#'   \item{cost}{total cost, in dollars}
#'   \item{qaly}{QALYs, quality-adjusted life-years}
#'   \item{dissev}{disease severity, range from 0.025 to 0.729}
#'   \item{race}{race}
#'   \item{blcost}{baseline cost, in dollars}
#'   \item{blqaly}{baseline QALY, quality-adjusted life-years}
#'   \item{male}{gender, 1 for male, 0 for female}
#' }
#' @source \url{http://www.uphs.upenn.edu/dgimhsr/eeinct_multiv.htm}
"clintrial_cea"

#' Example CEA colorectal cancer screening
#' 
#' A dataset that includes costs and life-years gained at the intervnetion level
#' (i.e. not individual level data) that can be used to conduct simple CEAs.
#' This dataset is provided in the text
#' "Applied Methods of Cost-effectiveness Analysis" by Gray, Clarke, 
#' Wolstenholme, and Wordsowrth. Oxford University Press, 2011. The data is
#' based on the study Gyrd-Hansen, D. (1997). "Is it cost effective to introduce
#' screening programmes for colorectal cancer? Illustrating the principles
#' of optimal resource allocation. Health Policy, 41, 189-199.
#' 
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{ID}{unique id for each screening option}
#'   \item{Program}{textual description of each screening option}
#'   \item{cost}{total cost, in pounds}
#'   \item{qaly}{life-years gained}
#' }
"clintrial_cea"
