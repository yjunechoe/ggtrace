<!-- README.md is generated from README.Rmd. Please edit that file -->

# **{ggtrace}** <img class="logo" src="man/figures/logo.png" align="right" style="width:120px;" />

#### **Programmatically explore, debug, and manipulate ggplot internals**

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.5.2-gogreen.svg)](https://github.com/yjunechoe/ggtrace)
<!-- badges: end -->

![](https://i.imgur.com/kpTffyw.jpg)

### **Installation**

You can install the development version from
[GitHub](https://github.com/yjunechoe/ggtrace/) with:

    # install.packages("remotes")
    remotes::install_github("yjunechoe/ggtrace")

    library(ggtrace) # v0.5.3

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

## **Example usage**

    library(ggplot2) # v3.4.0

### 1) **Crop polar coordinate plots**

Plot in polar coordinates:

    polar_plot <- ggplot(mtcars, aes(hp, mpg)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x) +
      expand_limits(y = c(0, 60)) +
      coord_polar(start = 0, theta = "y")

    polar_plot

<img src="man/figures/README-polar-plot-1.png" width="100%" />

Clipping the plot panel with `{ggtrace}` by highjacking the
`Layout$render()` method:

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

See implementation in
[`MSBMisc::crop_coord_polar()`](https://mattansb.github.io/MSBMisc/reference/crop_coord_polar.html).

### 2) **Highjack the drawing context**

Flashy example adopted from my [UseR!
talk](https://yjunechoe.github.io/ggtrace-user2022/#/for-grid-power-users):

    library(palmerpenguins)
    p <- na.omit(palmerpenguins::penguins) |> 
      ggplot(aes(x = species, y = flipper_length_mm)) +
      geom_boxplot(aes(fill = species), width = .7) +
      facet_wrap(~ year)

    ggtrace_highjack_return(
      p, Geom$draw_panel, cond = TRUE,
      value = quote({
        y_pos <- .25 * ._counter_ #<- internal counter tracking nth time the method is called
        grobTree( circleGrob(y = y_pos, gp = gpar(fill = linearGradient())), # R >= 4.1
                  editGrob(returnValue(), vp = viewport(clip = circleGrob(y = y_pos))) )
      }))

<img src="man/figures/README-flashy-1.png" width="100%" />

<!-- ### **Extract legends** -->
<!-- ```{r legend-plot} -->
<!-- p_legend <- ggplot(mtcars, aes(hp, mpg, color = factor(cyl))) + -->
<!--   geom_point() + -->
<!--   geom_smooth(method = "lm", formula = y ~ x) -->
<!-- p_legend -->
<!-- ``` -->
<!-- For more control over legends, we can use `ggplot2::guide_*(override.aes = ...)`: -->
<!-- ```{r legends-make} -->
<!-- p_legend1 <- p_legend + -->
<!--   scale_color_discrete( -->
<!--     name = "cyl", -->
<!--     guide = guide_legend(override.aes = list(shape = NA, fill = NA)) -->
<!--   ) + -->
<!--   theme(legend.key = element_rect(fill = "white")) -->
<!-- p_legend2 <- p_legend + -->
<!--   scale_color_discrete( -->
<!--     name = NULL, labels = c("observation", "linear fit", "95% interval"), -->
<!--     guide = guide_legend( -->
<!--       override.aes = list( -->
<!--         shape = c(16, NA, NA), color = c("black", "black", NA), -->
<!--         linetype = c(NA, 1, NA), fill = c(NA, NA, "grey60") -->
<!--       ) -->
<!--     ) -->
<!--   ) + -->
<!--   theme(legend.key = element_rect(fill = "white")) -->
<!-- library(patchwork) -->
<!-- p_legend1 + p_legend2 -->
<!-- ``` -->
<!-- Using `ggtrace_inspect_return()`, we can grab the return value from the legend-making function for each plot and then plot them over the panel using `{patchwork}`: -->
<!-- ```{r legends-inset} -->
<!-- legend1 <- ggtrace_inspect_return(p_legend1, ggplot2:::guide_gengrob.legend) -->
<!-- legend2 <- ggtrace_inspect_return(p_legend2, ggplot2:::guide_gengrob.legend) -->
<!-- (p_legend + guides(color = guide_none())) + -->
<!--   inset_element(legend1, .7, .8, .7, .8) + -->
<!--   inset_element(legend2, .85, .8, .85, .8) -->
<!-- ``` -->
