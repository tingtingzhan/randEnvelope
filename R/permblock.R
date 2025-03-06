

#' @title Permuted Block Randomization Schedule
#' 
#' @description
#' Permuted block randomization schedule, non-stratified.
#' 
#' @slot arm \link[base]{character} \link[base]{vector}, name of arms, 
#' such as `'tx1'`, `'tx2'`, `'control'`, etc.
#' 
#' @slot ratio \link[base]{integer} \link[base]{vector}, 
#' planned allocation ratios of arms.
#' End user should make sure the greatest common divisor of `ratio` is 1.
#' Default to balanced design `1:1: ... :1`.
#' 
#' @slot n \link[base]{integer} scalar, smallest sample size to be guaranteed.
#' 
#' @slot multiplier \link[base]{integer} \link[base]{vector}
#' to determine the permuted block sizes.
#' Default `2:3`, i.e., block size multipliers of 2 and 3 are permuted.
#' 
#' @name permblock
#' @aliases permblock-class
#' @export
setClass(Class = 'permblock', slots = c(
  arm = 'character',
  ratio = 'integer',
  n = 'integer',
  multiplier = 'integer'
), prototype = prototype(
  multiplier = 2:3
))



setMethod(f = initialize, signature = 'permblock', definition = function(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  if (!length(x@ratio)) {
    x@ratio <- rep(1L, times = length(x@arm))
  }

  return(x)
  
})


setValidity(Class = 'permblock', method = function(object) {
  arm <- object@arm
  ratio <- object@ratio
  n <- object@n
  m <- object@multiplier
  if (!is.vector(arm, mode = 'any') || !(ntx <- length(arm))) stop('`@arm` must be character vector')
  if (!is.vector(ratio, mode = 'integer') || any(ratio <= 0L)) stop('`@ratio` must be positive integer vector')
  if (length(ratio) && (ntx != length(ratio))) stop('`@arm` and `@ratio` must be same length')
  if (length(n) != 1L || n <= 0) stop('`@n` must be positive integer scalar')
  if (!is.vector(m, mode = 'integer') || anyNA(m) || any(m <= 1L) || anyDuplicated.default(m) || length(m) <= 1L) stop('`@multiplier` must be length>1L integer vector')
})





#' @rdname permblock
#' 
#' @param ... slots of S4 object \linkS4class{permblock}
#' 
#' @examples
#' permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L)
#' @export 
permblock <- function(...) new(Class = 'permblock', ...)


#' @importFrom cli col_blue col_cyan col_green col_magenta col_yellow style_bold style_hyperlink style_underline
#' @export
print.permblock <- function(x, ...) {
  return(paste0(
    'Permuted block' |> col_magenta() |> style_bold(), 
    ' randomization schedule is generated using ', 
    'R' |> col_blue() |> style_bold() |> style_hyperlink(url = 'https://cran.r-project.org'), 
    '. Block-size multipliers of ',
    x@multiplier |> sprintf(fmt = '\u00d7%d') |> col_green() |> style_bold() |> paste0(collapse = ' and '),
    ' are permuted. A ',
    x@ratio |> paste(collapse = ':') |> col_cyan() |> style_bold(), 
    ' allocation ratio is applied to ',
    x@arm |> col_yellow() |> style_bold() |> paste0(collapse = ' and '),
    ' arms within each block. ',
    x@n |> style_underline() |> style_bold(),
    ' records are generated.'
  ))
}


#' @title Show \linkS4class{permblock}
#' 
#' @param object \linkS4class{permblock}
#' 
#' @export
setMethod(f = show, signature = 'permblock', definition = function(object) {
  object |> print.permblock() |> message()
})



