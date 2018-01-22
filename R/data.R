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
#' of optimal resource allocation." Health Policy, 41, 189-199.
#' 
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{ID}{unique id for each screening option}
#'   \item{Program}{textual description of each screening option}
#'   \item{cost}{total cost, in pounds}
#'   \item{qaly}{life-years gained}
#' }
"colocancer_cea"

#' Clinical trial costs and outcomes from Briggs et al. article
#' 
#' A dataset of the means and standard deviations for a placebo and drug
#' treatment as used in Briggs, Wonderling, and Mooney. (1997). "Pulling 
#' cost-effectiveness analysis up by its bootstraps: a non-parametric approach 
#' to confidence interval estimation." Health Economics, 6, 327-340. Also used
#' in Briggs and Finn. (1998). "Confidence intervals or surfaces? Uncertainty on
#' the cost-effectiveness plane." Health Economics, 7, 723-740.
#' 
#' @format A data frame with 2 rows and 7 variables:
#' \describe{
#'   \item{group}{placebo or drug}
#'   \item{cost_mean}{mean cost estimates}
#'   \item{survival_mean}{mean survival estimates (in years)}
#'   \item{cost_stdev}{standard deviation of mean cost estimates}
#'   \item{survival_stdev}{standard deviation of mean survival estimates}
#'   \item{cost_surv_corr}{correlation between costs and survival}
#'   \item{n}{number of patients in each group}
#' }
"briggs_pulling_boots"

#' Prostate cancer screening simulations study based on ERSPC data
#' 
#' Results from Heijnsdijk, Carvalho, Auvinen et al. (2015). "Cost-effectiveness
#' of prostate cancer screening: a simulation study based on ERSPC data." 
#' JNCI, 107(1), dju366. The results of the European Randomized Study of 
#' Screening for Prostate Cancer (ERSPC) trial showed a statistically 
#' significant prostate cancer mortality reduction for the men screened in the 
#' intervention arm. Based on this data, the authors developed a 
#' microsimulation model to test a number of screening scenarios. All numbers
#' are given per 1000 simulated men.
#' 
#' @format A data frame with 6 rows and 14 variables:
#' \describe{
#'   \item{strategy}{generic indicator of screening strategy}
#'   \item{description}{description of screening strategy}
#'   \item{num_tests}{number of screening tests per 1000 men}
#'   \item{num_men}{number of unique men screened}
#'   \item{cancers_diag}{number of cancers diagnosed per 1000 men}
#'   \item{cancers_screened}{number of cancers detected through screening per
#'   1000 men}
#'   \item{cancers_overdiagnosed}{number of cancers diagnosed through screening 
#'   that would not have caused problems without screening per 1000 men}
#'   \item{death_prostate_cancer}{number of deaths from prostate cancer per
#'   1000 men}
#'   \item{ly_gained}{life-years gained (relative to no screening) per 1000 men}
#'   \item{qaly_gained}{qalys gained (relative to no screening) per 1000 men}
#' }
"prostate_simulation"

#' Cost-effectiveness of a state tobacco quitline
#' 
#' Examines the cost effectiveness of offering callers single session versus 
#' multisession counselling, with or without free nicotine patches (nrt).
#' Published in Hollis, McAfee, Fellows et al. (2007). "The effectiveness and
#' cost-effectiveness of telephone counselling and the nicotine patch in a state
#' tobacco quitline." Tobacco Control, 16(Suppl 1), i53-i59. Brief, moderate, 
#' and intensive refer to the length and frequency of counselling sessions.
#' NRT referrs to nicotine replacement therapy in the form of patches. The
#' outcome refers to the proportion of the sample that was abstinent at two
#' different time points.
#' 
#' @format A dataframe with 6 observations and 6 variables:
#' \describe{
#'   \item{intervention}{intervention strategy including counselling intensity
#'   and inclusion or not of NRT}
#'   \item{abstinence_6mos}{proportion of the sample abstinent at 6 months 
#'   follow-up}
#'   \item{abstinence_12mos}{proportion of the sample abstinent at 12 months 
#'   follow-up}
#'   \item{cost_mean}{mean cost per participant}
#'   \item{cost_sd}{standard deviation of the mean cost per participant}
#'   \item{n}{number of participants}
#' }
"quitline_cea"

#' Cost-effectiveness example dataset
#' 
#' Small dataset of three generic interventions with mean costs and life-years 
#' gained.
#' 
#' @format A dataframe with 3 observations and 3 variables:
#' \describe{
#'   \item{Intervention}{indicators of intervention}
#'   \item{costs}{mean costs of intervention}
#'   \item{lys_gained}{mean life-years gained of each intervention}
#' }
"example1"

#' Cost-effectiveness example dataset
#' 
#' Small dataset of three generic interventions with mean costs and qalys.
#' 
#' @format A dataframe with 5 observations and 3 variables:
#' \describe{
#'   \item{Intervention}{indicators of intervention}
#'   \item{costs}{mean costs of intervention}
#'   \item{qalys}{mean qalys of each intervention}
#' }
"example2"

#' Colorectal cancer screening cost-effectiveness analysis
#' 
#' Results from a cost-utility analysis using a Markov model of screening for 
#' colorectal cancer. Strategies compared inlcude guaiac-based
#' fecal occult blood test (FOBT) or fecal immunochemical test (FIT) annually, 
#' fecal DNA every 3 years, flexible sigmoidoscopy or computed
#' tomographic colonography every 5 years, and colonoscopy every 10 years.
#' Published in Heitman, Hilsden et al. (2010). "Colorectal Cancer Screening for 
#' Average-Risk North Americans: An Economic Evaluation." PLoS Med 7(11): 
#' e1000370. doi:10.1371/journal.pmed.1000370
#' 
#' @format A dataframe with 11 observations and 3 variables:
#' \describe{
#'   \item{strategy}{Screening strategy}
#'   \item{costs}{Average costs in 2008 Canadian dollars}
#'   \item{qalys}{Average quality-adjusted life-years following screening}
#' }
"colorectal_screening"