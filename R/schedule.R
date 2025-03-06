
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
#' @importFrom cli col_blue col_cyan col_green col_magenta col_yellow style_bold style_hyperlink style_underline
#' @export
schedule <- function(x, .blocks, ...) UseMethod(generic = 'schedule')


#' @rdname schedule
#' 
#' @param .blocks ..
#' 
#' @examples
#' (pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L))
#' set.seed(1251); pb |> schedule()
#' set.seed(1251); pb |> schedule(study.name = 'CDC')
#' @export schedule.permblock
#' @export
schedule.permblock <- function(x, .blocks = get_block(x), ...) {
  out <- data.frame(
    Sequence = seq_len(x@n), 
    Assignment = sample_block(x = .blocks, n = x@n),
    ...,
    row.names = NULL, check.names = FALSE
  )
  class(out) <- c('schedule', class(out))
  return(out)
  
}



#' @rdname schedule
#' @examples
#' (spb = pb |> 
#'   stratify(cohort = c('young', 'old'), state = c('PA', 'NJ')))
#' set.seed(1325); spb |> schedule()
#' @export schedule.stratified_permblock
#' @export
schedule.stratified_permblock <- function(x, .blocks = get_block(x), ...) {

  k <- .row_names_info(x@strata, type = 2L) # number of combined-strata

  # ?base::replicate or ?base::lapply both mess up with `...` !!
  tmp <- list()
  for (i in seq_len(k)) {
    suppressMessages(tmp[[i]] <- schedule.permblock(x, .blocks = .blocks, ...))
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








