
#' @title schedule
#' 
#' @param x ..
#' 
#' @param ... additional information to be appended to the randomization schedule.
#' Use with extreme caution!
#' 
#' @returns
#' Functions [schedule.permblock] and [schedule.stratified_permblock]
#' both returns a [schedule] object
#' 
#' @name schedule
#' @export
schedule <- function(x, ...) UseMethod(generic = 'schedule') # .blocks, 


#' @rdname schedule
#' @export schedule.permblock
#' @export
schedule.permblock <- function(x, ...) {
  out <- data.frame(
    Sequence = seq_len(x@n), 
    Assignment = x |> rpermblock(),
    ...,
    row.names = NULL, check.names = FALSE
  )
  names(out)[1:2] <- c(x@nm_sequence, x@nm_arm)
  class(out) <- c('schedule', class(out))
  return(out)
  
}



#' @title Random Generation based on \linkS4class{permblock}
#' 
#' @param x a \linkS4class{permblock}
#' 
#' @returns 
#' Function [rpermblock()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @details
#' First select a block-size multiplier.
#' 
#' Then do a \link[base]{sample}ing without replacement in the selected block.
#' 
#' @export
rpermblock <- function(x) {
  b_ <- rep(x@arm, times = x@ratio)
  b <- lapply(x@multiplier, FUN = function(m) rep(b_, times = m))
  nmax <- ceiling(x@n / min(lengths(b))) # max number of blocks needed
  id <- sample.int(n = length(x@multiplier), size = nmax, replace = TRUE) # randomly selected multipliers
  ret0 <- b[id] |> 
    lapply(FUN = function(i) {
      sample(x = i, size = length(i), replace = FALSE)
    }) |>
    unlist()
  return(ret0[seq_len(x@n)])
}






#' @rdname schedule
#' @export schedule.stratified_permblock
#' @export
schedule.stratified_permblock <- function(x, ...) { # .blocks = get_block(x), 

  k <- .row_names_info(x@strata, type = 2L) # number of combined-strata

  # ?base::replicate or ?base::lapply both mess up with `...` !!
  tmp <- list()
  for (i in seq_len(k)) {
    suppressMessages(tmp[[i]] <- schedule.permblock(x, ...)) # .blocks = .blocks, 
  }
  
  out <- data.frame(
    do.call(what = rbind.data.frame, args = tmp),
    x@strata[rep(seq_len(k), each = x@n), , drop = FALSE], 
    row.names = NULL, check.names = FALSE
  )
  attr(out, which = 'label') <- rep(x@label, each = x@n)
  class(out) <- c('schedule', class(out))
  return(out)
  
}








