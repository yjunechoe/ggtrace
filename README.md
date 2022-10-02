<!-- README.md is generated from README.Rmd. Please edit that file -->

# **{ggtrace}** <img class="logo" src="man/figures/logo.png" align="right" style="width:120px;" />

#### **Programmatically explore, debug, and manipulate ggplot internals**

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.5.2-gogreen.svg)](https://github.com/yjunechoe/ggtrace)
<!-- badges: end -->

![](https://i.imgur.com/8JJiyqb.png)

### **Installation**

You can install the development version from
[GitHub](https://github.com/yjunechoe/ggtrace/) with:

    # install.packages("remotes")
    remotes::install_github("yjunechoe/ggtrace")

    library(ggtrace) # v0.5.2

More on the 📦 package website: <https://yjunechoe.github.io/ggtrace>

### **Description**

`{ggtrace}` embodies an opinionated approach to learning about ggplot
internals. The internals is a difficult topic, so I recommend watching
the following presentations on `{ggtrace}` before getting started on any
kind of code:

-   [Presentation at
    rstudio::conf(2022)](https://www.rstudio.com/conference/2022/talks/cracking-open-ggplot-internals-ggtrace/)

-   [Presentation at useR!
    2022](https://www.youtube.com/watch?v=2JX8zu4QxMg&t=2959s)

Read more about the philosophy behind `{ggtrace}` in the [Getting
Started](https://yjunechoe.github.io/ggtrace/articles/getting-started.html)
vignette, and see examples in the
[Overview](https://yjunechoe.github.io/ggtrace/articles/overview.html)
vignette.

### **Example usage**

Plot in polar coordinates:

    library(ggplot2) # v3.3.6

    polar_plot <- ggplot(mtcars ,aes(hp, mpg)) +
      geom_point() +
      geom_smooth(method = "lm") +
      expand_limits(y = c(0, 60)) +
      coord_polar(start = 0, theta = "y")

    polar_plot

<img src="man/figures/README-polar-plot-1.png" width="100%" />

Clipping the plot panel with `{ggtrace}`:

    ggtrace::with_ggtrace(
      x = polar_plot + theme(aspect.ratio = 1/.48),
      method = Layout$render,
      trace_steps = 5L,
      trace_expr = quote({
        panels[[1]] <- editGrob(panels[[1]], vp = viewport(xscale = c(0.48, 1)))
      }),
      out = "g"
    )

<img src="man/figures/README-polar-plot-clipped-1.png" width="100%" />
