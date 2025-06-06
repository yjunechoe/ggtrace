% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflows-inspect.R, R/aliases.R
\name{ggtrace_inspect_on_error}
\alias{ggtrace_inspect_on_error}
\alias{inspect_on_error}
\title{Get information about a ggproto method on error}
\usage{
ggtrace_inspect_on_error(x, method, ...)

inspect_on_error(x, method, ...)
}
\arguments{
\item{x}{A ggplot object}

\item{method}{A function or a ggproto method.
The ggproto method may be specified using any of the following forms:
\itemize{
\item \code{ggproto$method}
\item \code{namespace::ggproto$method}
\item \code{namespace:::ggproto$method}
}}

\item{...}{Unused.}
}
\value{
A list of three elements: \code{counter}, \code{args}, and \code{env}.
}
\description{
Get information about a ggproto method on error
}
\examples{
library(ggplot2)
erroring_barplot <- ggplot(mtcars, aes(mpg, hp)) +
  stat_summary() +
  geom_bar()
inspect_on_error(erroring_barplot, StatCount$setup_params)
inspect_on_error(erroring_barplot, ggplot2:::Layer$compute_statistic)
}
