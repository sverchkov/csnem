# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Infer effect matrix from NEM mixture
#'
#' @param big.nem the big (nk x nk) nem
#' @param n.actions the number of (actual) actions n
#' @param k the number of contexts
#' @return list of: adjacency and accessibility matrices for new nem without redundant action nodes,
#' effect mapping matrix, name mapping list from the big nem to the compact
#'
#' @export
getCompactCSNEM <- function ( big.nem, n.actions, k ) {

  actions <- big.nem$control$Sgenes
  effects <- names( big.nem$LLperGene[[1]] )
  attachments = matrix( data = FALSE, nrow = length( actions ), ncol = length( effects ), dimnames = list( actions, effects ) )
  for ( a in actions ){
    map.pos = big.nem$mappos[[a]]
    attachments[ a, map.pos ] = TRUE
  }

  big.adj <- transitive.closure( big.nem$graph, mat = TRUE )

  good.adj <- reduceAdj( big.adj )

  # Remap effects to new names
  new.attachments <-
    matrix(
      data = FALSE,
      nrow = nrow( good.adj$adj ),
      ncol = length( effects ),
      dimnames = list( rownames( good.adj$adj ), effects )
    )
  # Attachment history
  attachment.history <- NULL

  for ( e in effects ){
    for ( act in names( which( attachments[,e] ) ) ){
      a <- good.adj$name.map[[ act ]]

      attachment.history <- rbind( attachment.history, c( effect = e, old.attachment = act, new.attachment = a ) )

      new.attachments[a,e] <- TRUE
    }
  }

  list(
    adj = transitive.reduction( good.adj$adj ),
    attachments = new.attachments,
    acc = good.adj$adj,
    name.map = good.adj$name.map,
    edge.history = good.adj$edge.history,
    attachment.history = attachment.history
  )
}
