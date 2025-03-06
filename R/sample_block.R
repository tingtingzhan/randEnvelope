

#' @title Sample from Blocks
#' 
#' @param x \link[base]{list} of depth-2, randomized blocks
#' 
#' @param n positive \link[base]{integer} scalar, sample size
#' 
#' @examples
#' (pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L))
#' pb |> get_block() |> sample_block(n = 30L) # base::set.seed if needed
#' @keywords internal
#' @export
sample_block <- function(x, n) {
  size0 <- min(lengths(unlist(x, recursive = FALSE))) # minimum block size
  nx <- length(x)
  nb <- ceiling(n / size0) # maximum number of blocks needed
  b <- sample.int(n = nx, size = nb, replace = TRUE) # indices of block size
  
  tmp <- .mapply(FUN = sample, dots = list(
    x = x, 
    size = tabulate(b, nbins = nx)
  ), MoreArgs = list(replace = TRUE))
  
  ret <- vector(mode = 'list', length = nb)
  for (i in seq_len(nx)) {
    ret[b == i] <- tmp[[i]] # degenerated okay
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

