#' Create an ICER table based on a set of average cost and effect values by
#' a set of unique intervention variables.
#' 
#' @param cst_vec A vector of average cost values the same length as intv_names.
#' @param eff_vec A vector of average effect values the same length as 
#'   intv_names.
#' @param intv_names A vector of character strings representing intervention
#'   names for each unique intervention to include a row in the ICER table.
#' @param eff_more_better If TRUE, a greater value for effects indicates a
#'                    better outcome. If FALSE, a smaller value for effects
#'                    indicates a better outcome. Default is TRUE.
#' @param cost_order If true, the order of options in an ICER table will be by 
#'   increasing average cost. If false, the order of options in an ICER table
#'   will be by increasing average effect.
#' @param table_print If true, the ICER table will be printed to the Console.
#'   If false, priting of the ICER table to the Console will be suppressed.
#' @return Here

create_icer_table <- function(cst_vec, eff_vec, intv_names,
                              eff_more_better=TRUE, cost_order=TRUE,
                              table_print=TRUE) {

  # icer_table_mat columns are
  #   names refer to the factor version of a given intervention value
  #   1. costs
  #   2. effects
  #   3. incremental costs
  #   4. incremental effects
  #   5. icer
  #   6. indicator of dominance
  icer_table_mat = matrix(NA, nrow=length(cst_vec), ncol=6)
  colnames(icer_table_mat) = c("Cost", "Effect", "Inc_Cost", "Inc_Effect",
                               "ICER", "Dominance")
  rownames(icer_table_mat) = intv_names

  icer_table_mat[, 1] = cst_vec
  icer_table_mat[, 2] = eff_vec

  # Order in terms of increasing cost
  icer_table_mat = icer_table_mat[c(order(icer_table_mat[, 1])), ]

  # Calculate incremental costs, effects, and ICERs
  icer_table_mat = icer_table_calc_incremental(icer_table_mat, eff_more_better)

  # Iterate until strongly dominated options are removed
  while_cont = TRUE
  while (while_cont) {
    row_strong = icer_table_strong(icer_table_mat)
    if (row_strong==0) {
      while_cont = FALSE
    }
    else {
      icer_table_mat[row_strong, c(3, 4, 5)] = NA
      icer_table_mat[row_strong, 6] = 1
      icer_table_mat = icer_table_calc_incremental(icer_table_mat, eff_more_better)
    }
  }

  # Iterate until weakly dominated options are removed
  while_cont = TRUE
  while (while_cont) {
    row_weak = icer_table_weak(icer_table_mat)
    if (row_weak==0) {
      while_cont = FALSE
    }
    else {
      icer_table_mat[row_weak, c(3, 4, 5)] = NA
      icer_table_mat[row_weak,6 ] = 2
      icer_table_mat = icer_table_calc_incremental(icer_table_mat, eff_more_better)
    }
  }

  # Sort matrix by increasing effect if cost_order is FALSE
  if (!cost_order) {
    if (eff_more_better) {
      icer_table_mat = icer_table_mat[c(order(icer_table_mat[, 2])),]
    }
    else {
      icer_table_mat = icer_table_mat[c(order(icer_table_mat[, 2], decreasing = TRUE)),]
    }
  }

  if (table_print) {
    print(icer_table_mat)
    print("Dominance: 1=intervention/option strongly dominated, 2=intervention/option weakly dominated")
  }

  #print(icer_table_mat)
  return(icer_table_mat)

}

icer_table_calc_incremental = function(icer_table_mat, eff_order) {

  prev_row = 0
  i = 1
  # Begin by looking or the first non-dominated row
  while (prev_row==0) {
    if (is.na(icer_table_mat[i, 6])) {
      prev_row = i
    }
    else {
      i = i + 1
    }
  } # upon exit prev_row equals the first non-dominated row

  for (i in (prev_row+1):nrow(icer_table_mat)) {
    # Check to ensure this row is not dominated, enter here if is is a non-dominated row
    if (is.na(icer_table_mat[i, 6])) {
      icer_table_mat[i, 3] = icer_table_mat[i, 1] - icer_table_mat[prev_row, 1]
      if (eff_order) {
        icer_table_mat[i, 4] = icer_table_mat[i, 2] - icer_table_mat[prev_row, 2]
      }
      else {
        icer_table_mat[i, 4] = icer_table_mat[prev_row, 2] - icer_table_mat[i, 2]
      }
      icer_table_mat[i, 5] = icer_table_mat[i, 3] / icer_table_mat[i, 4]
      prev_row = i
    }
  }
  return(icer_table_mat)
}

icer_table_strong = function(icer_table_mat) {
  #browser()
  for (i in 2:nrow(icer_table_mat)) {
    if (is.na(icer_table_mat[i, 6])) {
      if (icer_table_mat[i,4] < 0) {
        return(i)
      }
    }
  }
  return(0)
}

icer_table_weak = function(icer_table_mat) {

  num_row = nrow(icer_table_mat)
  prev_row = 0
  i = 2
  # Begin by looking for the first non-dominated row
  while (prev_row==0) {
    if (i>num_row) {
      prev_row = num_row
    }
    else if (is.na(icer_table_mat[i, 6])) {
      prev_row = i
    }
    else {
      i = i + 1
    }
  } # upon exit prev_row equals the first non-dominated row

  if (prev_row==nrow(icer_table_mat)) {
    return(0)
  }

  for (i in (prev_row+1):nrow(icer_table_mat)) {
    if (is.na(icer_table_mat[i, 6])) {
      if (icer_table_mat[i, 5] < icer_table_mat[prev_row, 5]) {
        return(prev_row)
      }
    }
  }
  return(0)
}
