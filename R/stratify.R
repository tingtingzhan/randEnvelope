
#' @title Stratified Permuted Block Randomization
#' 
#' @description
#' Stratified permuted block randomization.
#' 
#' @slot strata \link[base]{data.frame}, e.g., \link[base]{expand.grid} from a \link[base]{list} of \link[base]{character} \link[base]{vector}s.
#' 
#' @slot label \link[base]{character} \link[base]{vector}, strata labels
#' 
#' @export
setClass(Class = 'stratified_permblock', contains = 'permblock', slots = c(
  strata = 'data.frame',
  label = 'character'
))






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
#' Function [stratify.permblock()] returns a \linkS4class{stratified_permblock} object.
#' @export stratify.permblock
#' @export
stratify.permblock <- function(x, ..., sep = ' \u058d ') {
  strata <- expand.grid(..., stringsAsFactors = FALSE)
  label <- do.call(what = paste, args = c(as.list.data.frame(strata), list(sep = sep)))
  new(Class = 'stratified_permblock', 
      x, 
      strata = strata, label = label)
}



#' @importFrom cli col_magenta style_bold
#' @export
print.stratified_permblock <- function(x, ...) {
  print.permblock(x, ...) |>
    gsub(pattern = 'Permuted block', replacement = 'Stratified permuted block') |>
    gsub(pattern = 'are generated', replacement = sprintf(
      fmt = 'are generated per stratum of %s', 
      names(x@strata) |> col_magenta() |> style_bold() |> paste0(collapse = ' and ')
    ))
}



#' @title Show \linkS4class{stratified_permblock}
#' 
#' @param object \linkS4class{stratified_permblock}
#' 
#' @export
setMethod(f = show, signature = 'stratified_permblock', definition = function(object) {
  object |> print.stratified_permblock() |> message()
})




