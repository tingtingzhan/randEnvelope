
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
#' @importFrom cli col_black col_blue col_cyan col_green col_magenta col_yellow style_bold style_hyperlink
#' @export
schedule <- function(x, .blocks, ...) UseMethod(generic = 'schedule')


#' @rdname schedule
#' 
#' @param .blocks ..
#' 
#' @examples
#' pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L)
#' set.seed(1251); pb |> schedule()
#' set.seed(1251); pb |> schedule(study.name = 'CDC')
#' @export schedule.permblock
#' @export
schedule.permblock <- function(x, .blocks = get_block(x), ...) {
  
  msg <- paste0(
    'Permuted block' |> col_magenta() |> style_bold(), 
    ' randomization schedule is generated using ', 
    'R' |> col_blue() |> style_bold() |> style_hyperlink(url = 'https://cran.r-project.org'), 
    '. ', 
    'Block-size multipliers of ',
    paste0('\u00d7', x@multiplier) |> col_green() |> style_bold() |> paste0(collapse = ' and '),
    ' are permuted. A ',
    x@ratio |> paste(collapse = ':') |> col_cyan() |> style_bold(), 
    ' allocation ratio is applied to ',
    x@arm |> col_yellow() |> style_bold() |> paste0(collapse = ' and '),
    ' arms within each block. ',
    x@n |> col_black() |> style_bold(),
    ' records are generated.'
  )
  message(msg)
  
  out <- data.frame(
    Sequence = seq_len(x@n), 
    Assignment = sample_block(x = .blocks, n = x@n),
    ...,
    row.names = NULL, check.names = FALSE
  )
  attr(out, which = 'message') <- msg
  class(out) <- c('schedule', class(out))
  return(out)
  
}



#' @rdname schedule
#' @examples
#' set.seed(1325); pb |> 
#'   stratify(cohort = c('young', 'old'), state = c('PA', 'NJ')) |> 
#'   schedule()
#' @export schedule.stratified_permblock
#' @export
schedule.stratified_permblock <- function(x, .blocks = get_block(x), ...) {

  sgrid <- do.call(what = expand.grid, args = c(x@strata, stringsAsFactors = FALSE))
  strata_labels <- do.call(what = paste, args = c(sgrid, list(sep = x@sep)))
  k <- .row_names_info(sgrid, type = 2L) # number of combined-strata

  # suppressMessages(tmp <- replicate(n = k, expr = schedule.permblock(x, .blocks = .blocks, ...), simplify = FALSE)) # mess up with `...` !!
  tmp <- list()
  for (i in seq_len(k)) {
    suppressMessages(tmp[[i]] <- schedule.permblock(x, .blocks = .blocks, ...))
  }
  
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
  return(out)
  
}








