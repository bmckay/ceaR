#' @export
boot_cea_fnc <- function(cea_lst, data, indices) {

  tmp_lst <- cea_lst
  tmp_lst$cea_data <- data[indices, ]
  tmp_lst$cst_reg <- glm(tmp_lst$incremental$cst_formula, 
                         family = tmp_lst$reg.types[1], 
                         data = tmp_lst$cea_data)
  tmp_lst$eff_reg <- glm(tmp_lst$incremental$eff_formula, 
                         family = tmp_lst$reg.types[2], 
                         data = tmp_lst$cea_data)
  cea_estimates(tmp_lst)
}

#' @export
boot_cea <- function(cea_lst, R) {
  
  cea_lst$boot <- boot::boot(data = cea_lst$cea_data, 
                             statistic = boot_cea_fnc, R = R, 
                             strata = cea_lst$cea_data[, cea_lst$intv_char], 
                             cea_lst = cea_lst)
  #cea_lst$boot$data <- NULL
  cea_lst$boot$results <- tibble::as_tibble(cea_lst$boot$t)
  tmp_names <- rep("", 2 * cea_lst$incremental$N_intv_vec)
  
  for (i in 1:cea_lst$incremental$N_intv_vec) {
    tmp_names[i] <- paste("cst", cea_lst$intv_char, 
                          cea_lst$incremental$intv_vec[i], sep = "_")
    tmp_names[cea_lst$incremental$N_intv_vec + i] <- 
      paste("eff", cea_lst$intv_char, cea_lst$incremental$intv_vec[i], 
            sep = "_")
  }
  colnames(cea_lst$boot$results) <- tmp_names
  
  # Goal here is to determine base intervention and incremental costs and effects
  base_intv <- rownames(cea_lst$icer.table)[1]
  base_cst <- paste("cst", base_intv, sep = "_")
  base_eff <- paste("eff", base_intv, sep = "_")
  rm(base_intv)
  other_intv <- colnames(cea_lst$boot$results)
  other_intv <- other_intv[other_intv != base_cst]
  other_intv <- other_intv[other_intv != base_eff]
  other_cst <- other_intv[1:(cea_lst$incremental$N_intv_vec - 1)]
  other_eff <- other_intv[cea_lst$incremental$N_intv_vec:
                          (2 * (cea_lst$incremental$N_intv_vec - 1))]
  
  base_data <- dplyr::select(cea_lst$boot$results, base_cst, base_eff)
  other_data <- dplyr::select(cea_lst$boot$results, other_cst, other_eff)
  
  # Reorders columns from all costs and then all effects to cost/effect pairs
  reorder_vec <- c()
  for (i in 1:(cea_lst$incremental$N_intv_vec - 1)) {
    reorder_vec <- c(reorder_vec, i, (cea_lst$incremental$N_intv_vec - 1 + i))
  }
  other_data <- other_data[, reorder_vec]
  inc_data <- other_data - rep(base_data, cea_lst$incremental$N_intv_vec - 1)

  if (!cea_lst$eff_more_better) {
    # account for wrong order of subtraction above
    # occurs in the even numbered columns
    for (i in seq.int(2, (2 * (cea_lst$incremental$N_intv_vec - 1)), 2)) {
      inc_data[, c(i)] <- -1.0 * inc_data[, c(i)]
    }
  }
  
  # Create appropriatelly laid out dataset
  cst_vec <- c()
  eff_vec <- c()
  boot_num <- c()
  intv_vec <- c()
  for (i in 1:(cea_lst$incremental$N_intv_vec - 1)) {
    cst_vec <- c(cst_vec, inc_data[, c(2 * i - 1)])
    eff_vec <- c(eff_vec, inc_data[, c(2 * i)])
    boot_num <- c(boot_num, seq.int(from = 1, to = nrow(inc_data), by = 1))
    intv_vec <- c(intv_vec, 
              rep(stringr::str_sub(colnames(inc_data)[2 * i - 1], 5, -1), 
              nrow(inc_data)))
  }
  inc_data_long <- tibble::data_frame(cst = cst_vec,
                              eff = eff_vec,
                              boot_num = boot_num,
                              intv = intv_vec)
  cea_lst$boot$results_inc <- inc_data_long
  class(cea_lst$boot) <- "ceamodel.boot"
  return(cea_lst)
}

# plot_boot <- function(cea_lst) {
#   
#   base_intv <- rownames(cea_lst$icer.table)[1]
#   base_cst <- paste("cst", base_intv, sep = "_")
#   base_eff <- paste("eff", base_intv, sep = "_")
#   rm(base_intv)
#   other_intv <- colnames(cea_lst$boot$results)
#   other_intv <- other_intv[other_intv != base_cst]
#   other_intv <- other_intv[other_intv != base_eff]
#   other_cst <- other_intv[1:(cea_lst$incremental$N_intv_vec - 1)]
#   other_eff <- other_intv[cea_lst$incremental$N_intv_vec:
#                             (2 * (cea_lst$incremental$N_intv_vec - 1))]
#   rm(other_intv)
#   
#   for (i in other_cst) {
#     cea_lst$boot$results <-
#       dplyr::mutate(cea_lst$boot$results, paste(i, "inc", sep = "_") = i - base_cst)
#   }
#   for (i in other_eff) {
#     cea_lst$boot$results %>%
#       dplyr::mutate(paste(i, "inc", sep = "_") = i - base_eff)
#   }
# }

# Challenge is needing to use boot_cea to get average costs and effects from each, 
# will need to look at ceamodel_incremental
# Probably need to just pass the ceamodel object rather than the formulas and 
# regression types