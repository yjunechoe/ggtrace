.onLoad <- function(...) {
  suppressPackageStartupMessages(requireNamespace("ggplot2"))
}

.onAttach <- function(...) {
  packageStartupMessage("Package {ggplot2} loaded")
}
