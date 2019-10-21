# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Infer CSNEM
#' @param log_likelihood_matrix A matrix where each row corresponds to an effect and each column corresponds to
#' an action, where each cell corresponds to the log likelihood ratio of the effect being differentially expressed
#' as a result of that action.
#' @param k The number of contexts
#'
#' @return CSNEM (with equivalent nodes merged)
#'
#' @export
infer_csnem <- function (log_likelihood_matrix, k){

  n_actions <- ncol(log_likelihood_matrix)

  big_nem <- infer_constrained_nem(log_likelihood_matrix, k)

  return (getCompactCSNEM(big_nem, n_actions, k))
}
