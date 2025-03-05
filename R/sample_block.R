

#' @title Sample from Blocks
#' 
#' @param x \link[base]{list} of depth-2, randomized blocks
#' 
#' @param n positive \link[base]{integer} scalar, sample size
#' 
#' @examples
#' pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L)
#' pb |> get_block() |> sample_block(n = 30L) # base::set.seed if needed
#' @keywords internal
#' @export
sample_block <- function(x, n) {
  size0 <- min(lengths(unlist(x, recursive = FALSE))) # minimum block size
  nx <- length(x)
  max_b <- ceiling(n / size0) # maximum of blocks needed
  bid <- sample.int(n = nx, size = max_b, replace = TRUE) # indices of block size
  
  tmp <- .mapply(FUN = sample, dots = list(
    x = x, 
    size = tabulate(bid, nbins = nx)
  ), MoreArgs = list(replace = TRUE))
  
  ret <- vector(mode = 'list', length = max_b)
  for (i in seq_len(nx)) {
    ret[bid == i] <- tmp[[i]] # degenerated okay
  } # smart!
  return(unlist(ret)[seq_len(n)])
}


#' @title Permuted Blocks
#' 
#' @param x \linkS4class{permblock} object
#' 
#' @examples
#' pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L)
#' pb |> get_block()
#' @keywords internal
#' @importFrom combinat permn
#' @export
get_block <- function(x) {
  # `x` is 'permblock'
  lapply(x@multiplier, FUN = function(m) {
    rep(x@arm, times = x@ratio * m) |>
      permn() |> # could be slow!
      unique.default() 
  }) # all potential blocks, by multipliers
}

