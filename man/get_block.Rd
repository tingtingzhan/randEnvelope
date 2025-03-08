% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_block.R
\name{get_block}
\alias{get_block}
\title{Permuted Blocks}
\usage{
get_block(x)
}
\arguments{
\item{x}{\linkS4class{permblock} object}
}
\description{
Permuted Blocks
}
\examples{
pb = permblock(arm = c('intervention', 'control'), ratio = 1:2, n = 20L)
pb |> get_block()
}
\keyword{internal}
