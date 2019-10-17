# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Infer CSNEM
#' @param log_likelihood_matrix A matrix where each row corresponds to an effect and each column corresponds to
#' an action, where each cell corresponds to the log likelihood ratio of the effect being differentially expressed
#' as a result of that action.
#' @param k The number of contexts
#'
#' @return nem object
#'
#' @export
#' @import nem
#' @import Matrix
infer_csnem <- function (log_likelihood_matrix, k){

  n_actions <- ncol(log_likelihood_matrix)
  n_effects <- nrow(log_likelihood_matrix)

  big_matrix <- makeBigRMatrix(log_likelihood_matrix, k)

  contextualized_actions <- colnames( big_matrix )

  # Set parameters for MC-EMiNEM
  inference <- "mc.eminem"

  # action graph prior
  action_prior <- Matrix( 0.2, n_actions, n_actions )
  diag( action_prior ) <- 1

  control = set.default.parameters(
    contextualized_actions,
    type = "CONTmLLBayes", # For continuous values like we have
    mcmc.nsamples = 5e3, # Number of MCMC iterations. Default = 1e6
    mcmc.nburnin = 1.5e4, # MCMC burnin period. Default = 1e6
    #Pe. # "Hidden" prior (attachments?)
    Pm = as.matrix( .bdiag( rep( list( action_prior ), k ) ) ),
    #eminem.maxsteps, # Max number of em steps. Default = 1000
    eminem.sdVal = k * n_actions, # Number of edges to change in one MCMC step. Default = 1 (check paper)
    #Pm.frac_edges, # Expected fraction of edges in action graph. Default = 0.2
    eminem.changeHfreq = 5e3, # the Empirical Bayes step is performed every <changeHfreq> steps
    #prob.cutoff, # Probability cutoff for edges in graph(?) Default = 0.5
    lambda = 0.5 # (ep in runMCMC) Sparsity prior for acceptance rate (check paper)
  )

  control$lowMemFootprint <- TRUE # Option in my fork of nem. Should do nothing with official nem package.

  # Run NEM

  big_nem <- nem( D = big_matrix, inference = inference, control = control )
}
