# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Strip everything including and following a delimiter from strings
#' @param in.str an input string or an array of input strings
#' @param delim the delimiter
#' @return every element of in.str taken until the first delimiter occurence exclusive
#'
#' @import purrr
stripSuffix <- function( in.str, delim = "_" ){
  strsplit(in.str, delim, fixed = TRUE)  %>%
    map(function(x) x[1]) %>%
    reduce(c)
}
