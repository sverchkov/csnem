# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Make the big R matrix
#' @param small.R.matrix the "small" R matrix, the one where each action appears once
#' @param k the number of times to append the small R matrix to itself
#' @return the small R matrix appended to itself k times with column names remapped to <name>_i for i in 1...k
#'
#' @import purrr
makeBigRMatrix <- function ( small.R.matrix, k ){
  1:k %>%
    map(
      function (i) {
        R_part <- small.R.matrix
        colnames( R_part ) <- paste0( colnames( small.R.matrix ), "_", i )
        return ( R_part )
      }) %>%
    reduce(cbind)
}
