
#' @title Stratified Permuted Block Randomization
#' 
#' @description
#' Stratified permuted block randomization.
#' 
#' @slot strata \link[base]{data.frame}
#' 
#' @slot label \link[base]{character} \link[base]{vector}, strata labels
#' 
#' @export
setClass(Class = 'stratified', contains = 'permblock', slots = c(
  strata = 'data.frame',
  label = 'character'
))






#' @title stratify
#' 
#' @param x a \linkS4class{permblock}
#' 
#' @param sep \link[base]{character} scalar, symbol to separate multiple strata names
#' 
#' @param ... one or more \link[base]{character} \link[base]{vector}s  
#' to be \link[base]{expand.grid} into the slot `@strata`
#' 
#' @returns 
#' All methods of the `S3` generic function [stratify()] 
#' return \linkS4class{stratified} \linkS4class{permblock}.
#' 
#' @name stratify
#' @export
stratify <- function(x, ..., sep) UseMethod(generic = 'stratify')

#' @rdname stratify
#' @export stratify.permblock
#' @export
stratify.permblock <- function(x, ..., sep = ' \u058d ') {
  strata <- expand.grid(..., stringsAsFactors = FALSE)
  label <- strata |>
    as.list.data.frame() |>
    c(. = _, list(sep = sep)) |>
    do.call(what = paste, args = _)
  new(Class = 'stratified', 
      x, 
      strata = strata, label = label)
}



#' @export
print.stratified <- function(x, ...) {
  x |> 
    print.permblock(...) |>
    gsub(pattern = 'Permuted block', replacement = 'Stratified permuted block') |>
    gsub(pattern = 'are generated', replacement = sprintf(
      fmt = 'are generated per stratum of %s', 
      names(x@strata) |> col_magenta() |> style_bold() |> paste0(collapse = ' and ')
    ))
}



#' @title Show \linkS4class{stratified}
#' 
#' @param object \linkS4class{stratified}
#' 
#' @export
setMethod(f = show, signature = 'stratified', definition = \(object) {
  object |> 
    print.stratified() |> 
    message()
})




