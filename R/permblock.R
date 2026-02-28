

#' @title `S4` Class `'permblock'`: Permuted Block Randomization
#' 
#' @description
#' Permuted block randomization (non-stratified).
#' 
#' @slot arm \link[base]{character} \link[base]{vector}, name of study arms, 
#' such as `'treatment1'`, `'treatment1'`, `'control'`, etc.
#' 
#' @slot ratio \link[base]{integer} \link[base]{vector}, 
#' planned allocation ratios of arms.
#' End user should make sure that the greatest common divisor of `ratio` is 1.
#' Default to balanced design `1:1: ... :1`.
#' 
#' @slot n \link[base]{integer} scalar, smallest sample size to be guaranteed.
#' 
#' @slot multiplier \link[base]{integer} \link[base]{vector}
#' permuted block size multipliers.
#' Default `2:3`, i.e., block size multipliers of 2 and 3 are permuted.
#' 
#' @keywords internal
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



setMethod(f = initialize, signature = 'permblock', definition = \(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  if (!length(x@ratio)) {
    x@ratio <- rep(1L, times = length(x@arm))
  }

  return(x)
  
})


setValidity(Class = 'permblock', method = \(object) {
  arm <- object@arm
  ratio <- object@ratio
  n <- object@n
  m <- object@multiplier
  if (!is.vector(arm, mode = 'any') || !(ntx <- length(arm))) stop('`@arm` must be character vector')
  if (!is.vector(ratio, mode = 'integer') || any(ratio <= 0L)) stop('`@ratio` must be positive integer vector')
  if (length(ratio) && (ntx != length(ratio))) stop('`@arm` and `@ratio` must be same length')
  if (length(n) != 1L || n <= 0) stop('`@n` must be positive integer scalar')
  if (!is.vector(m, mode = 'integer') || anyNA(m) || any(m <= 0L) || anyDuplicated.default(m) || length(m) <= 1L) stop('`@multiplier` must be length>1L integer vector')
})





#' @rdname permblock
#' 
#' @param ... slots of the S4 object \linkS4class{permblock}
#' 
#' @export 
permblock <- function(...) new(Class = 'permblock', ...)


#' @title print.permblock
#' 
#' @param x ..
#' 
#' @param ... ..
#' 
#' @export print.permblock
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
setMethod(f = show, signature = 'permblock', definition = \(object) {
  object |> 
    print.permblock() |> 
    message()
})



