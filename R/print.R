

#' @title Print Randomization Schedule, Envelopes and Inserts
#' 
#' @description 
#' Create randomization envelopes and inserts 
#' using the package \CRANpkg{ggplot2}.
#' 
#' @param x a [schedule] object
#' 
#' @param path \link[base]{character} scalar
#' 
#' @param which \link[base]{character} scalar, either one of
#' \describe{
#' \item{`'schedule'`}{(default) the randomization schedule in a `.csv` file, 
#' to be uploaded to a REDCap data base. 
#' This procedure is fast.}
#' \item{`'envelope'` or `'insert'`}{the randomization envelopes and inserts 
#' in `.pdf` files, printed using the package \CRANpkg{ggplot2}. 
#' This procecure could be slow for large sample size.}
#' \item{`'all'`}{all options above}
#' }
#'  
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of function \link[parallel]{detectCores}.
#' 
#' @param title \link[base]{character} scalar, name of study
#' 
#' @param scientist,statistician (R objects convertible to) \link[utils]{person} objects,
#' the principal investigator and the statistician
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns 
#' 
#' The `S3` method [print.schedule()] returns ..
#' 
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot annotate element_blank element_rect theme xlim ylim
#' @importFrom utils person as.person write.table
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply makeCluster stopCluster
#' @export print.schedule
#' @export
print.schedule <- function(
    x, 
    path = tempdir(),
    which = c('schedule', 'envelope', 'insert', 'all'),
    mc.cores = getOption('cores'),
    title = 'Study Title',
    scientist = person(given = 'Principal', family = 'Investigator', email = 'Principal.Investigator@jefferson.edu'),
    statistician = person(given = 'Tingting', family = 'Zhan', email = 'Tingting.Zhan@jefferson.edu'),
    ...
) {
  
  if (!inherits(x, what = 'schedule')) stop('`x` must be \'schedule\'.')
  
  scientist <- scientist |> as.person()
  statistician <- statistician |> as.person()
  
  dir.create(path, showWarnings = FALSE)
  which <- match.arg(which)
  file_envelope <- tempfile(pattern = 'Envelope_', tmpdir = path, fileext = '.pdf')
  file_insert <- tempfile(pattern = 'Insert_', tmpdir = path, fileext = '.pdf')
  file_schedule <- tempfile(pattern = 'Schedule_', tmpdir = path, fileext = '.csv')
  
  if (which %in% c('all', 'schedule')) {
    
    x |> 
      write.table(
        x = _, file = file_schedule, 
        quote = FALSE, sep = ',', row.names = FALSE, qmethod = 'double')
    
    'randomization {.href [schedule](file://{path.expand(path = file_schedule)})}' |>
      cli_text()
    
  }
  
  if (which %in% c('all', 'envelope', 'insert')) {

    bg <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme(
      #axis.ticks = element_blank(), # no need in addition to `axis.ticks.length` 
      axis.ticks.length = unit(0, units = 'cm'),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(.05, times = 4L), units = 'cm'),
      panel.border = element_rect(fill = NA, colour = 'grey70')
    ) 
    
    bg_envelope <- bg + 
      annotate(geom = 'text', label = title, size = 7*2, fontface = 'bold', x = .5, y = .75) +
      annotate(geom = 'text', label = scientist |> format(include = 'email'), size = 4.5*2,  x = .5, y = .65) +
      annotate(geom = 'text', label = 'Open Randomization Envelopes in Sequence', size = 3*2, x = .5, y = .25, color = 'grey50')
    
    bg_insert <- bg +
      annotate(geom = 'text', label = title, size = 6, fontface = 'bold', x = .5, y = .9) +
      annotate(geom = 'text', label = paste0('Principle Investigator:  ', scientist |> format()), size = 4.5, x = .5, y = .85) +
      annotate(geom = 'text', label = paste0('Statistician:  ', statistician |> format()), size = 4.5, x = .5, y = .8) +
      annotate(geom = 'text', label = 'Patient Initials: ______________________', size = 4.5, x = .5, y = .42) +
      annotate(geom = 'text', label = 'Study ID / MRN: ______________________', size = 4.5, x = .5, y = .35) +
      annotate(geom = 'text', label = 'Clinician Signature:  _______________________________', size = 4.5, x = .65, y = .2) +
      annotate(geom = 'text', label = 'Date:            mm /           dd /                     yyyy', size = 4.5, x = .7, y = .13) +
      annotate(geom = 'text', label = 'Put completed form back into envelope, seal and keep for records', size = 4, x = .6, y = .08, colour = 'grey50')
    
    n <- nrow(x)
    seqn <- n |>
      seq_len()
    label <- attr(x, which = 'label', exact = TRUE)
    
    # https://www.avery.com/templates/5164
    # Biostat Jefferson order, 2024
    # use this aspect ratio, even if printing on #10 envelopes directly!!
    cairo_pdf(filename = file_envelope, 
              width = (4 + 3/16) * 1.75, height = (3 + 5/16) * 1.75)
    
    fn_envelope <- \(i) {
      p <- bg_envelope + 
        (if (length(label)) annotate(geom = 'label', label = label[i], size = 5*2, fontface = 'bold', x = .5, y = .5, fill = 'grey95')) +
        annotate(geom = 'label', label = paste0('Sequence #:  ', x[[1L]][i]), size = 5*2, fontface = 'bold', x = .5, y = .35, fill = 'grey95')
      print(p)
      if (!(i %% 10L)) message('\r', i, '/', n, ' 10# envelopes created', appendLF = FALSE)
    }
    
    noout_ <- seqn |>
      lapply(FUN = fn_envelope)
    if (FALSE) {
      # as of 2026-02-27
      # on Mac, the pdf file created by parallel::mclapply is damaged!!!
      switch(
        EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
        unix = { 
          noout_ <- seqn |>
            mclapply(FUN = fn_envelope, mc.cores = mc.cores)
        }, 
        windows = {
          i <- NULL # just to suppress devtools::check NOTE
          registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
          noout_ <- foreach(i = seqn, .options.multicore = list(cores = mc.cores)) %dopar% fn_envelope(i)
          stopCluster(cl)
        })
    }

    dev.off()
    message('\r                                \r', appendLF = FALSE)
    # link is activated (OLD and NEW), but RStudio tries to open this pdf file in RStudio.
    # tzh does not know how to specify desired program to open, as for now
    # cli_text(sprintf(fmt = '\r %d {.href [10# envelopes](file://{\'%s\'})}', n, file_envelope)) # OLD: error under Windows; okay on Mac
    n |>
      sprintf(fmt = '\r \u00d7%d {.href [10# envelopes](file://{path.expand(path = file_envelope)})}') |>
      cli_text() # NEW
    
    cairo_pdf(filename = file_insert, width = 8.5, height = 11) # US letter
    
    fn_insert <- \(i) {
      p <- bg_insert + 
        (if (length(label)) annotate(geom = 'label', label = label[i], size = 5.5, fontface = 'bold', x = .5, y = .72, fill = 'grey95')) +
        annotate(geom = 'label', label = paste0('Sequence #:  ', x[[1L]][i]), size = 5.5, fontface = 'bold', x = .5, y = .6, fill = 'grey95') +
        annotate(geom = 'label', label = paste0('Assignment:  ', x[[2L]][i]), size = 5.5, fontface = 'bold', x = .5, y = .55, fill = 'grey95') 
      print(p)
      if (!(i %% 10L)) message('\r', i, '/', n, ' inserts created', appendLF = FALSE)
    }
    
    noout_ <- seqn |>
      lapply(FUN = fn_insert)
    if (FALSE) {
      # as of 2026-02-27
      # on Mac, the pdf file created by parallel::mclapply is damaged!!!
      switch(
        EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
        unix = { 
          noout_ <- seqn |>
            mclapply(FUN = fn_insert, mc.cores = mc.cores)
        },
        windows = {
          i <- NULL # just to suppress devtools::check NOTE
          registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
          noout_ <- foreach(i = seqn, .options.multicore = list(cores = mc.cores)) %dopar% fn_insert(i)
          stopCluster(cl)
        })
    }
    dev.off()
    message('\r                                \r', appendLF = FALSE)
    n |>
      sprintf(fmt = '\r \u00d7%d {.href [inserts](file://{path.expand(path = file_insert)})}') |>
      cli_text()
  
    system(command = paste0('open ', file_insert))
    system(command = paste0('open ', file_envelope))
    
  }
  
  system(command = paste0('open ', path))
  
  return(invisible(c(file_schedule, file_envelope, file_insert)))
  
}




