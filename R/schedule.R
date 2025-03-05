
#' @title schedule
#' 
#' @param x ..
#' 
#' @param ... ..
#' 
#' @name schedule
#' @importFrom cli col_black col_blue col_cyan col_green col_magenta col_yellow style_bold style_hyperlink
#' @export
schedule <- function(x, ...) UseMethod(generic = 'schedule')

#' @importFrom combinat permn
getblocks <- function(x) {
  # `x` is 'permblock'
  lapply(x@multiplier, FUN = function(m) {
    rep(x@treatment, times = x@ratio * m) |>
      permn() |>
      unique.default() # could be slow!
  }) # all potential blocks, by multipliers
}

#' @rdname schedule
#' 
#' @param .blocks ..
#' 
#' @examples
#' pb = permblock(treatment = c('intervention', 'control'), ratio = 1:2, n = 20L)
#' set.seed(1251); r1 = pb |> schedule()
#' r1
#' @export
schedule.permblock <- function(x, .blocks = getblocks(x), ...) {
  
  txt_ratio <- sprintf(
    fmt = 'Block size multipliers of %s are permuted. A %s allocation ratio is applied to %s arms within each block.', 
    paste0('\u00d7', x@multiplier) |> col_green() |> style_bold() |> paste0(collapse = ' and '), 
    x@ratio |> paste(collapse = ':') |> col_cyan() |> style_bold(), 
    x@treatment |> col_yellow() |> style_bold() |> paste0(collapse = ' and '))
  
  txt_n <- sprintf(
    fmt = '%s records are generated.', 
    x@n |> col_black() |> style_bold())
  
  msg <- paste0(
    'Permuted block' |> col_magenta() |> style_bold(), 
    ' randomization schedule is generated using ', 
    'R' |> col_blue() |> style_bold() |> style_hyperlink(url = 'https://cran.r-project.org'), 
    '. ', 
    txt_ratio, 
    ' ',
    txt_n)
  message(msg)
  
  out <- data.frame(
    Sequence = seq_len(x@n), 
    Assignment = perm_block_(blocks = .blocks, n = x@n))
  
  attr(out, which = 'message') <- msg
  class(out) <- c('schedule', class(out))
  return(invisible(out))
  
}

# debug(perm_block_); perm_block_(blocks = blocks, n = 40L)
perm_block_ <- function(blocks, n) {
  min_bsize <- min(lengths(unlist(blocks, recursive = FALSE)))
  n_bsize <- length(blocks)
  max_b <- ceiling(n / min_bsize) # maximum of blocks needed
  id_bsize <- sample.int(n = n_bsize, size = max_b, replace = TRUE) # indices of block size
  
  tmp <- .mapply(FUN = function(blocks_, size_) {
    sample(blocks_, size = size_, replace = TRUE)
  }, dots = list(
    blocks_ = blocks, 
    size_ = tabulate(id_bsize, nbins = n_bsize)
  ), MoreArgs = NULL)
  
  ret <- vector(mode = 'list', length = max_b)
  for (i in seq_len(n_bsize)) {
    ret[id_bsize == i] <- tmp[[i]] # degenerated okay
  }
  return(unlist(ret)[seq_len(n)])
}

#' @rdname schedule
#' @examples
#' set.seed(1325); r2 = pb |> 
#'   stratify(cohort = c('young', 'old'), state = c('PA', 'NJ')) |> 
#'   schedule()
#' r2
#' @export
schedule.stratified_permblock <- function(x, .blocks = getblocks(x), ...) {

  sgrid <- do.call(what = expand.grid, args = c(x@strata, stringsAsFactors = FALSE))
  strata_labels <- do.call(what = paste, args = c(sgrid, list(sep = x@sep)))
  k <- .row_names_info(sgrid, type = 2L) # number of combined-strata

  suppressMessages(tmp <- replicate(n = k, expr = schedule.permblock(x, .blocks = .blocks), simplify = FALSE))
    
  msg <- attr(tmp[[1L]], which = 'message', exact = TRUE) |>
    gsub(pattern = 'Permuted block', replacement = 'Stratified permuted block') |>
    gsub(pattern = 'are generated', replacement = sprintf(
      fmt = 'are generated per stratum of %s', 
      names(x@strata) |> col_magenta() |> style_bold() |> paste0(collapse = ' and ')
    ))
  message(msg)
  
  out <- data.frame(
    do.call(what = rbind.data.frame, args = tmp),
    sgrid[rep(seq_len(k), each = x@n), , drop = FALSE], 
    strata_labels = rep(strata_labels, each = x@n),
    row.names = NULL, check.names = FALSE
  )
  
  attr(out, which = 'message') <- msg
  class(out) <- c('schedule', class(out))
  return(invisible(out))
  
}








