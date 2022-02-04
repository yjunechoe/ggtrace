ggeval_silent <- function(x) {
  asNamespace("ggplot2")$ggplot_gtable(asNamespace("ggplot2")$ggplot_build(x))
}
