# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Reduce a repetetive big.adj to a non-redundant csnem with ancestry naming convention
#' @param big.adj a named adjacency matrix with possibly redundant nodes
#' @return a named list with the new adjacency matrix and the name mapping
reduceAdj <- function( big.adj ) {
  old.names <- colnames( big.adj )
  name.map <- Map( function( node ){
    me <- stripSuffix( node )
    ancestors <- stripSuffix( names( which( big.adj[,node] != 0 ) ) )
    context <- setdiff( ancestors, me )
    if ( length( context ) > 0 )
      paste0( me, " [", paste( context, collapse = " "), "]" )
    else
      me
  }, old.names )
  names( name.map ) = old.names

  new.names = unique( name.map )
  new.size = length( new.names )

  new.adj = matrix( data = FALSE, nrow = new.size, ncol = new.size, dimnames = list( new.names, new.names ) )
  edge.history <- NULL
  for ( a in old.names )
    for ( b in old.names )
      if ( big.adj[a,b] != 0 ){
        new.adj[ name.map[[a]], name.map[[b]] ] <- TRUE
        edge.history <- rbind( edge.history, c( old.src = a, old.dest = b, new.src = name.map[[a]], new.dest = name.map[[b]] ) )
      }

  list( adj = new.adj, name.map = name.map, edge.history = edge.history )
}
