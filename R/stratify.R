
#' @title stratified_permblock
#' 
#' @slot strata named \link[base]{list} of \link[base]{character} \link[base]{vector}s.
# name clash \link[survival]{strata} !  
#' 
#' @slot sep \link[base]{character} scalar, symbol to separate multiple strata names
#' 
#' @export
setClass(Class = 'stratified_permblock', contains = 'permblock', slots = c(
  strata = 'list',
  sep = 'character'
), prototype = prototype(
  sep = ' \u058d '#, # ' / ',
), validity = function(object) {
  strata <- object@strata
  if (!all(vapply(strata, FUN = is.vector, mode = 'character', FUN.VALUE = NA))) stop('illegal ``@strata')
})






#' @title stratify
#' 
#' @param x ..
#' 
#' @param sep \link[base]{character} scalar, symbol to separate multiple strata names
#' 
#' @param ... ..
#' 
#' @name stratify
#' @export
stratify <- function(x, ..., sep) UseMethod(generic = 'stratify')

#' @rdname stratify
#' @returns 
#' Function [stratify.permblock] returns a \linkS4class{stratified_permblock} object.
#' @examples
#' permblock(treatment = c('treatment', 'control'), ratio = 1:2, n = 20L) |>
#'  stratify(cohort = c('non_Oral', 'Oral'), inst = c('Ohio', 'Jeff'))
#' @export stratify.permblock
#' @export
stratify.permblock <- function(x, ..., sep = ' \u058d ') {
  new(Class = 'stratified_permblock', x, strata = list(...), sep = sep)
}