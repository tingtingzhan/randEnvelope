

#' @title Print Randomization Schedule, Envelopes and Inserts
#' 
#' @description 
#' Create randomization envelopes and inserts using \link[ggplot2]{ggplot}.
#' 
#' @param x a [schedule] object
#' 
#' @param path \link[base]{character} scalar
#' 
#' @param title \link[base]{character} scalar, name of study
#' @param scientist \link[base]{character} scalar, (email of) the principal investigator
#' @param statistician \link[base]{character} scalar, (email of) the statistician
#' @param ... ..
#' 
#' @returns 
#' 
#' Function [print.schedule] returns ..
#' 
#' @examples 
#' # see ?schedule
#' 
#' @importFrom cli cli_text
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot annotate element_blank element_rect theme xlim ylim
#' @importFrom utils write.table
#' @export print.schedule
#' @export
print.schedule <- function(
    x, 
    path = tempdir(),
    title = 'Study Title',
    scientist = 'Principal.Investigator@jefferson.edu',
    statistician = 'Tingting.Zhan@jefferson.edu',
    ...
) {
  
  if (!inherits(x, what = 'schedule')) stop('`x` must be \'schedule\'.')
  
  dir.create(path, showWarnings = FALSE)
  file_envelope <- tempfile(pattern = 'Envelope_', tmpdir = path, fileext = '.pdf')
  file_insert <- tempfile(pattern = 'Insert_', tmpdir = path, fileext = '.pdf')
  file_schedule <- tempfile(pattern = 'Schedule_', tmpdir = path, fileext = '.csv')
  
  x0 <- x
  if (length(x$strata_labels)) x0$strata_labels <- NULL
  write.table(x = x0, file = file_schedule, quote = FALSE, sep = ',', row.names = FALSE, qmethod = 'double')
  cli_text('randomization {.href [schedule](file://{path.expand(path = file_schedule)})}')

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
    annotate(geom = 'text', label = scientist, size = 4.5*2,  x = .5, y = .65) +
    annotate(geom = 'text', label = 'Open Randomization Envelopes in Sequence', size = 3*2, x = .5, y = .25, color = 'grey50')
  
  bg_insert <- bg +
    annotate(geom = 'text', label = title, size = 6, fontface = 'bold', x = .5, y = .9) +
    annotate(geom = 'text', label = paste0('Principle Investigator:  ', scientist), size = 4.5, x = .5, y = .85) +
    annotate(geom = 'text', label = paste0('Statistician:  ', statistician), size = 4.5, x = .5, y = .8) +
    annotate(geom = 'text', label = 'Patient Initials: ______________________', size = 4.5, x = .5, y = .42) +
    annotate(geom = 'text', label = 'Study ID / MRN: ______________________', size = 4.5, x = .5, y = .35) +
    annotate(geom = 'text', label = 'Clinician Signature:  _______________________________', size = 4.5, x = .65, y = .2) +
    annotate(geom = 'text', label = 'Date:            mm /           dd /                     yyyy', size = 4.5, x = .7, y = .13) +
    annotate(geom = 'text', label = 'Put completed form back into envelope, seal and keep for records', size = 4, x = .6, y = .08, colour = 'grey50')
  
  n <- .row_names_info(x, 2L)
  
  # https://www.avery.com/templates/5164
  # Biostat Jefferson order, 2024
  # use this aspect ratio, even if printing on #10 envelopes directly!!
  cairo_pdf(filename = file_envelope, 
            width = (4 + 3/16) * 1.75, height = (3 + 5/16) * 1.75)
  noout_ <- lapply(seq_len(length.out = n), FUN = function(i) {
    p <- bg_envelope + 
      (if (length(x$strata_labels)) annotate(geom = 'label', label = x$strata_labels[i], size = 5*2, fontface = 'bold', x = .5, y = .5, fill = 'grey95')) +
      annotate(geom = 'label', label = paste0('Sequence #:  ', x$Sequence[i]), size = 5*2, fontface = 'bold', x = .5, y = .35, fill = 'grey95')
    print(p)
    if (!(i %% 10L)) message('\r', i, '/', n, ' 10# envelopes created', appendLF = FALSE)
  })
  dev.off()
  message('\r                                \r', appendLF = FALSE)
  
  # link is activated (OLD and NEW), but RStudio tries to open this pdf file in RStudio.
  # tzh does not know how to specify desired program to open, as for now
  # cli_text(sprintf(fmt = '\r %d {.href [10# envelopes](file://{\'%s\'})}', n, file_envelope)) # OLD: error under Windows; okay on Mac
  cli_text(sprintf(fmt = '\r \u00d7%d {.href [10# envelopes](file://{path.expand(path = file_envelope)})}', n)) # NEW
  
  cairo_pdf(filename = file_insert, width = 8.5, height = 11) # US letter
  noout_ <- lapply(seq_len(length.out = n), FUN = function(i) {
    p <- bg_insert + 
      (if (length(x$strata_labels)) annotate(geom = 'label', label = x$strata_labels[i], size = 5.5, fontface = 'bold', x = .5, y = .72, fill = 'grey95')) +
      annotate(geom = 'label', label = paste0('Sequence #:  ', x$Sequence[i]), size = 5.5, fontface = 'bold', x = .5, y = .6, fill = 'grey95') +
      annotate(geom = 'label', label = paste0('Assignment:  ', x$Assignment[i]), size = 5.5, fontface = 'bold', x = .5, y = .55, fill = 'grey95') 
    print(p)
    if (!(i %% 10L)) message('\r', i, '/', n, ' inserts created', appendLF = FALSE)
  })
  dev.off()
  message('\r                                \r', appendLF = FALSE)
  cli_text(sprintf(fmt = '\r \u00d7%d {.href [inserts](file://{path.expand(path = file_insert)})}', n))
  
  system(command = paste0('open ', path))
  
  return(invisible(c(file_envelope, file_insert)))
  
}




