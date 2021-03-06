% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{clintrial_cea}
\alias{clintrial_cea}
\title{Example CEA clinical trial data}
\format{A data frame with 500 rows and 9 variables:
\describe{
  \item{id}{unique id for each patient in trial}
  \item{treat}{intervention variable, 1 for treatment, 0 for control}
  \item{cost}{total cost, in dollars}
  \item{qaly}{QALYs, quality-adjusted life-years}
  \item{dissev}{disease severity, range from 0.025 to 0.729}
  \item{race}{race}
  \item{blcost}{baseline cost, in dollars}
  \item{blqaly}{baseline QALY, quality-adjusted life-years}
  \item{male}{gender, 1 for male, 0 for female}
}}
\source{
\url{http://www.uphs.upenn.edu/dgimhsr/eeinct_multiv.htm}
}
\usage{
clintrial_cea
}
\description{
A dataset that includes cost, QALYs, and six explanatory variables that
can be used to conduct multivariable analyses of cost and QALYs by
generalized linear models and can also be used for other example CEAs. This
dataset is provided by the Health Services Research Unit and used in the text
"Economic Evaluation in Clinical Trials" by Glick HA, Doshi JA, Sonnad SS,
and Polsky D. Oxford University Press, 2007.
}
\keyword{datasets}
