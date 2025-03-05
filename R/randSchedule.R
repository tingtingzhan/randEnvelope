

#' @title (Stratified) Permuted Block Randomization Schedules
#' 
#' @description ..
#' 
#' @param treatment \link[base]{character} \link[base]{vector}, name of treatments, 
#' such as `'tx1'`, `'tx2'`, `'control'`, etc.
#' 
#' @param tx_ratio \link[base]{integer} \link[base]{vector}, 
#' planned allocation ratios of the `treatment` arms.
#' End user should make sure the greatest common divisor of `tx_ratio` is 1.
#' Default to balanced design `1:1: ... :1`.
#' 
#' @param strata (optional) named \link[base]{list} of \link[base]{character} \link[base]{vector}s to denote the strata.
# name clash \link[survival]{strata} !  
#' Default to no stratification (missing argument).
#' 
#' @param strata_sep \link[base]{character} scalar, symbol to separate multiple strata names
#' 
#' @param n \link[base]{integer} scalar, smallest sample size to be guaranteed.
#' In stratified randomization, this is the size *per stratum*.
#' 
#' @param mblock \link[base]{integer} \link[base]{vector} of \link[base]{length} \eqn{>1}, 
#' *multipliers* to determine the permuted block sizes.
#' Default `2:3`, i.e., block size multipliers of 2 and 3 are permuted.
#' 
#' @param title \link[base]{character} scalar, name of study
#' @param scientist \link[base]{character} scalar, (email of) the principal investigator
#' @param statistician \link[base]{character} scalar, (email of) the statistician
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' \describe{
#' 
#' \item{Simple randomization}{Based on a single sequence of random assignments.}
#' 
#' \item{Block randomization}{Randomize subjects into groups of equal sample sizes (balance in sample size across groups).
#' Blocks are small and balanced (or with pre-determined group-ratio) with predetermined group assignments. 
#' \enumerate{
#' \item{Block size is pre-determined and is a small multiple of the number of groups (e.g. *2, *3, *4, etc if balanced with group);}
#' \item{All possible combinations of groups within the block are calculated;}
#' \item{Blocks are then randomly chosen.}
#' }}
#' 
#' \item{Stratified randomization}{Control and balance the influence of covariates (i.e. pre-determined subjects’ baseline characteristics) among groups
#' \enumerate{
#' \item{Generate a separate block for each combination of covariates (with subjects assigned to the appropriate block of covariates);}
#' \item{Simple randomization within each block to assign subjects into groups.}
#' }}
#' 
#' }
#' 
#' 
#' @returns 
#' 
#' Function [randSchedule] returns a \link[base]{data.frame}
#'  
#' @references 
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3136079/}
#' 
#' @examples 
#' tx = c('treatment', 'control')
#' txr = c(1L, 2L)
#' strt = list(cohort = c('non_Oral', 'Oral'), institute = c('Ohio', 'Jeff'))
#' 
#' r1 = randSchedule(tx, txr, n = 40L)
#' r2 = randSchedule(tx, txr, strata = strt, n = 30L)
#' 
#' if (TRUE) {
#' file.remove(randEnvelope(r1))
#' file.remove(randEnvelope(r2[1:10,]))
#' }
#' 
#' @importFrom cli col_black col_blue col_cyan col_green col_magenta col_yellow style_bold style_hyperlink
#' @importFrom combinat permn
#' @importFrom Hmisc capitalize
#' @export
randSchedule <- function(
    treatment, 
    tx_ratio = rep(1L, times = ntx), 
    strata, 
    strata_sep = ' \u058d ', # ' / ',
    n = stop('number of patients (per stratum) is required'), 
    mblock = 2:3,
    title = 'Study Title',
    scientist = 'Principal.Investigator@jefferson.edu',
    statistician = 'Tingting.Zhan@jefferson.edu',
    ...
) {
  
  if (!is.vector(treatment, mode = 'any') || !(ntx <- length(treatment))) stop('treatment must be character vector')
  if (!is.vector(tx_ratio, mode = 'integer') || any(tx_ratio <= 0L)) stop('`tx_ratio` must be positive integer vector')
  if (ntx != length(tx_ratio)) stop('treatment and tx_ratio must be same length')
  
  if (!is.vector(mblock, mode = 'integer') || anyNA(mblock) || any(mblock <= 1L) || anyDuplicated.default(mblock) || length(mblock) <= 1L) stop('`mblock` must be length>1L integer vector')
  blocks <- lapply(mblock, FUN = function(i) {
    unique.default(permn(rep(treatment, times = tx_ratio * i))) # could be slow!
  }) # all potential blocks, by multipliers
  
  nstrata <- if (!missing(strata)) {
    if (!is.recursive(strata)) strata <- list(stratum = strata)
    length(strata)
  } else 0L
  
  txt_nm <- trimws(sprintf(fmt = '%s permuted block', if (nstrata) 'stratified' else ''))
  
  txt_ratio <- sprintf(
    fmt = 'block size multipliers of %s are permuted. A %s allocation ratio is applied to %s arms within each block.', 
    paste0(style_bold(col_green(paste0('\u00d7', mblock))), collapse = ' and '), 
    style_bold(col_cyan(paste(tx_ratio, collapse = ':'))), 
    paste0(style_bold(col_yellow(treatment)), collapse = ' and '))
  
  txt_strata <- if (nstrata) {
    sprintf(
      fmt = 'The randomization is stratified by %s. %d records are generated per stratum.', 
      paste0('`', names(strata), '`', collapse = ' and '),
      n)
  } else sprintf(fmt = 'A total of %s records are generated.', style_bold(col_black(n)))
  
  out_txt <- paste0(
    style_bold(col_magenta(capitalize(txt_nm))), 
    ' randomization schedule is generated using ', 
    style_hyperlink(text = style_bold(col_blue('R')), url = 'https://cran.r-project.org'), '. ', 
    capitalize(txt_ratio), 
    ' ',
    txt_strata)
  message(out_txt)

  if (!nstrata) {
    
    out <- data.frame(
      Sequence = seq_len(n), 
      Assignment = perm_block_(blocks = blocks, n = n))
    
  } else {
    
    sgrid <- do.call(expand.grid, args = c(strata, stringsAsFactors = FALSE))
    n_strata <- .row_names_info(sgrid, type = 2L)
    strata_labels <- do.call(paste, args = c(sgrid, list(sep = strata_sep)))
    
    out <- data.frame(
      Sequence = rep(seq_len(n), times = n_strata),
      sgrid[rep(seq_len(n_strata), each = n), , drop = FALSE], 
      strata_labels = rep(strata_labels, each = n),
      Assignment = unlist(replicate(n = n_strata, expr = perm_block_(blocks = blocks, n = n), simplify = FALSE)), 
      row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
    
  }
  
  attr(out, which = 'txt') <- out_txt
  
  attr(out, which = 'title') <- title
  attr(out, which = 'scientist') <- scientist
  attr(out, which = 'statistician') <- statistician
  class(out) <- c('randSchedule', class(out))
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








#' @title write.csv.randSchedule
#' 
#' @description
#' ..
#' 
#' @param x [randSchedule] object
#' 
#' @param file,quote,sep,row.names,qmethod,... parameters of \link[utils]{write.table}
#' 
#' @importFrom utils write.table
#' @export
write.csv.randSchedule <- function(
    x,
    file, 
    quote = FALSE,
    sep = ',',
    row.names = FALSE,
    qmethod = 'double',
    ...
) {
  dir.create(path = dirname(file), recursive = TRUE)
  write.table(x = x, file = file, quote = quote, sep = sep, row.names = row.names, qmethod = qmethod, ...)
}






if (FALSE) {
  packageDate('combinat') # 2012-10-29
  p1 = combinat::permn # much faster
  packageDate('gtools') # 2023-11-19
  p2 = gtools::permutations
  library(microbenchmark)
  microbenchmark(p1(4), p2(4, 4))
}

