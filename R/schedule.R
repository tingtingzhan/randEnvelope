
#' @title Create an Randomization Schedule
#' 
#' @param x see **Usage**
#' 
#' @returns
#' All `S3` methods of the generic function [schedule()] 
#' return an R object of `'schedule'`, 
#' which inherits from the class \link[base]{data.frame}.
#' 
#' @name schedule
#' @export
schedule <- function(x) UseMethod(generic = 'schedule')


#' @rdname schedule
#' @importFrom utils head
#' @export schedule.permblock
#' @export
schedule.permblock <- function(x) {
  out <- data.frame(
    Sequence = seq_len(x@n), 
    Assignment = x |> rpermblock() |> head(n = x@n),
    row.names = NULL, check.names = FALSE
  )
  class(out) <- c('schedule', class(out)) |> 
    unique.default()
  return(out)
  
}



#' @title Random Generation based on \linkS4class{permblock}
#' 
#' @param x a \linkS4class{permblock}
#' 
#' @details
#' 1. \link[base]{sample}, *with* replacement,  a block-size multiplier.
#' 
#' 2. \link[base]{sample}, *without* replacement, in the selected block.
#' 
#' @returns 
#' The function [rpermblock()] returns a \link[base]{character} 
#' \link[base]{vector} with \link[base]{length} no less than `@n`.
#' 
#' @note
#' The function \link[base]{sample} is not an `S3` generic function.
#' 
#' The function [rpermblock()] is named in the fashion of the function \link[stats]{rnorm}.
#' 
#' @export
rpermblock <- function(x) {
  
  min_sz <- sum(x@ratio) * min(x@multiplier) # min block-size
  max_blk <- ceiling(x@n / min_sz) # max block-number
  unt <- rep(x@arm, times = x@ratio) # unit-block content
  
  x@multiplier |>
    lapply(FUN = \(m) unt |> rep(times = m)) |> # multiplied-blocks
    sample(size = max_blk, replace = TRUE) |> # select `max_blk` multiplied-blocks
    lapply(FUN = \(i) { # sample-without-repacement in each multiplied-blocks
      sample(x = i, size = length(i), replace = FALSE) 
    }) |>
    unlist()
  
}






#' @rdname schedule
#' @export schedule.stratified
#' @export
schedule.stratified <- function(x) {

  k <- nrow(x@strata) # number of combined-strata

  out <- k |>
    replicate(
      n = _, 
      expr = schedule.permblock(x), 
      simplify = FALSE
    ) |>
    do.call(what = rbind.data.frame, args = _) |>
    data.frame(
      x@strata[rep(seq_len(k), each = x@n), , drop = FALSE], 
      row.names = NULL, check.names = FALSE
    )
    
  attr(out, which = 'label') <- rep(x@label, each = x@n)
  class(out) <- c('schedule', class(out)) |> 
    unique.default()
  return(out)
  
}








