
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **{ggtrace}**

<!-- badges: start -->
<!-- badges: end -->

An experimental package for programmatically debugging ggproto methods.

## **Why {ggtrace}?**

-   **Lightweight** ⚡
    -   The only dependency is `{rlang}` (not even `{ggplot2}`!)
    -   There isn’t a lot of code in the package - most of the heavy
        lifting is done by `base::trace()`
-   **User-friendly** ❤
    -   Everything happens in your local session - no need to fork a
        repo to inspect/edit the internals!
    -   Multiple expressions can be passed in for evaluation inside
        method body at specified steps
    -   Output of evaluated expressions are available for inspection
        outside of the debugging environment
    -   Calls `untrace()`/`gguntrace()` on itself on exit by default,
        for extra safety.
-   **Flexible** 🛠
    -   You can *programmatically* debug with `ggtrace()` or
        *interactively* debug with `ggedit()`
    -   Plays nice with existing debugging tools (e.g., `browser()`).
    -   Since `ggtrace()` doesn’t rely on interactivity or side effects,
        it’s ideal for making targeted `{reprex}`-es
    -   Works with other object oriented systems in R (e.g., R6), not
        just ggproto!
-   **Powerful** 💪
    -   You can return the method’s whole environment with `ggtrace()`
        for further inspection
    -   You can test changes to the source code with `ggedit()`, which
        is restored upon `gguntrace()`
    -   You can insert`browser()` calls inside deep parts of the method
        body with `ggedit()`

More on the 📦 pkgdown website: <https://yjunechoe.github.io/ggtrace>

## **Installation**

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yjunechoe/ggtrace")
```

## **Usage**

``` r
library(ggtrace) # v0.1.2
```

## **Example 1 - `compute_layer` method from `PositionJitter`**

### **Step 1. Make plot**

``` r
library(ggplot2) # v3.3.5

jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
  geom_point(position = position_jitter(width = 0.2, seed = 2021))
jitter_plot
```

<img src="man/figures/README-ex-1-setup-1.png" width="100%" />

### **Step 2. Inspect callstack of the ggproto method**

``` r
ggbody(PositionJitter$compute_layer)
#> [[1]]
#> `{`
#> 
#> [[2]]
#> trans_x <- if (params$width > 0) function(x) jitter(x, amount = params$width)
#> 
#> [[3]]
#> trans_y <- if (params$height > 0) function(x) jitter(x, amount = params$height)
#> 
#> [[4]]
#> x_aes <- intersect(ggplot_global$x_aes, names(data))
#> 
#> [[5]]
#> x <- if (length(x_aes) == 0) 0 else data[[x_aes[1]]]
#> 
#> [[6]]
#> y_aes <- intersect(ggplot_global$y_aes, names(data))
#> 
#> [[7]]
#> y <- if (length(y_aes) == 0) 0 else data[[y_aes[1]]]
#> 
#> [[8]]
#> dummy_data <- new_data_frame(list(x = x, y = y), nrow(data))
#> 
#> [[9]]
#> fixed_jitter <- with_seed_null(params$seed, transform_position(dummy_data, 
#>     trans_x, trans_y))
#> 
#> [[10]]
#> x_jit <- fixed_jitter$x - x
#> 
#> [[11]]
#> y_jit <- fixed_jitter$y - y
#> 
#> [[12]]
#> transform_position(data, function(x) x + x_jit, function(x) x + 
#>     y_jit)
```

### **Step 3. `ggtrace()` - including the last line with keyword `~step`**

``` r
ggtrace(
  method = PositionJitter$compute_layer,
  trace_steps = c(1, 1, 9, 12),
  trace_exprs = rlang::exprs(
    data,            # What does the data passed in look like?
    params,          # What do the initial parameters look like?
    dummy_data,      # What is `dummy_data` defined at Step 8?
    ~step            # What does the last line evaluate to?
                     # - i.e., what is returned by the method?
  ),
  .print = FALSE     # Don't print evaluated expressions to console
)

# plot not printed to save space
jitter_plot
#> Tracing method compute_layer from <PstnJttr> ggproto.
#> 
#>  [Step 1]> data 
#> 
#>  [Step 1]> params 
#> 
#>  [Step 9]> dummy_data 
#> 
#>  [Step 12]> transform_position(data, function(x) x + x_jit, function(x) x + y_jit) 
#> 
#> Untracing method compute_layer from <PstnJttr> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

### **Step 4. Inspect trace dump**

``` r
jitter_tracedump <- last_ggtrace()

lapply(jitter_tracedump, nrow)
#> $`[Step 1]> data`
#> [1] 1000
#> 
#> $`[Step 1]> params`
#> NULL
#> 
#> $`[Step 9]> dummy_data`
#> [1] 1000
#> 
#> $`[Step 12]> transform_position(data, function(x) x + x_jit, function(x) x + y_jit)`
#> [1] 1000

lapply(jitter_tracedump, head)
#> $`[Step 1]> data`
#>   x    y PANEL group
#> 1 5 61.5     1     5
#> 2 4 59.8     1     4
#> 3 2 56.9     1     2
#> 4 4 62.4     1     4
#> 5 2 63.3     1     2
#> 6 3 62.8     1     3
#> 
#> $`[Step 1]> params`
#> $`[Step 1]> params`$width
#> [1] 0.2
#> 
#> $`[Step 1]> params`$height
#> [1] 0.04
#> 
#> $`[Step 1]> params`$seed
#> [1] 2021
#> 
#> 
#> $`[Step 9]> dummy_data`
#>   x    y
#> 1 5 61.5
#> 2 4 59.8
#> 3 2 56.9
#> 4 4 62.4
#> 5 2 63.3
#> 6 3 62.8
#> 
#> $`[Step 12]> transform_position(data, function(x) x + x_jit, function(x) x + y_jit)`
#>          x        y PANEL group
#> 1 4.980507 61.50684     1     5
#> 2 4.113512 59.77872     1     4
#> 3 2.083873 56.86655     1     2
#> 4 3.952698 62.42703     1     4
#> 5 2.054530 63.29763     1     2
#> 6 3.080538 62.77536     1     3
```

## **Example 2 - `draw_group` method from `GeomSmooth`**

### **Step 1. Make plot**

``` r
smooth_plot <- ggplot(mtcars, aes(mpg, hp)) +
  geom_point() +
  stat_smooth()
smooth_plot
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="man/figures/README-ex-2-setup-1.png" width="100%" />

### **Step 2. Inspect callstack of the ggproto method**

``` r
ggbody(GeomSmooth$draw_group)
#> [[1]]
#> `{`
#> 
#> [[2]]
#> ribbon <- transform(data, colour = NA)
#> 
#> [[3]]
#> path <- transform(data, alpha = NA)
#> 
#> [[4]]
#> ymin = flipped_names(flipped_aes)$ymin
#> 
#> [[5]]
#> ymax = flipped_names(flipped_aes)$ymax
#> 
#> [[6]]
#> has_ribbon <- se && !is.null(data[[ymax]]) && !is.null(data[[ymin]])
#> 
#> [[7]]
#> gList(if (has_ribbon) GeomRibbon$draw_group(ribbon, panel_params, 
#>     coord, flipped_aes = flipped_aes), GeomLine$draw_panel(path, 
#>     panel_params, coord))
```

### **Step 3. `ggtrace()` - get the `gList()`**

``` r
ggtrace(
  method = GeomSmooth$draw_group,
  trace_steps = 7,
  trace_exprs = quote(~step), # Grab the gList() object it returns
  .print = FALSE
)

# plot not printed to save space
smooth_plot
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
#> Tracing method draw_group from <GeomSmth> ggproto.
#> 
#>  [Step 7]> gList(if (has_ribbon) GeomRibbon$draw_group(ribbon, panel_params, coord,
#>    flipped_aes = flipped_aes), GeomLine$draw_panel(path, panel_params, coord)) 
#> 
#> Untracing method draw_group from <GeomSmth> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

### **Step 4. Inspect trace dump**

Get grobs in the gList and do some weird stuff with it

``` r
smooth_tracedump <- last_ggtrace()

smooth_gList <- smooth_tracedump[[1]]

smooth_gList
#> (gTree[geom_ribbon.gTree.132], polyline[GRID.polyline.133])

library(grid)

grid.ls(smooth_gList)
#> geom_ribbon.gTree.132
#>   GRID.polygon.129
#>   GRID.polyline.130
#> GRID.polyline.133

grid.newpage()
grid.draw(gTree(children = smooth_gList, vp = viewport()))
```

<img src="man/figures/README-ex-2-last-a-1.png" width="20%" />

``` r
# The weird stuff

smooth_ribbon_polygon <- editGrob(
  smooth_gList[1][[1]],
  "polygon",
  grep = TRUE,
  gp = gpar(fill = "#b742ce", alpha = 0.7, lwd = 3, col = "black")
)
smooth_ribbon_gTree <- gTree(
  children = gList(
    smooth_ribbon_polygon,
    textGrob("Weee", x = .7, gp = gpar(col = "red", fontsize = unit(10, "pt")))
  ),
  vp = viewport(width = 1, height = 1, default.units = "in", angle = 30)
)

grid.newpage()
grid.draw(smooth_ribbon_gTree)
```

<img src="man/figures/README-ex-2-last-a-2.png" width="20%" />

I guess you could use this for some fancy *data-driven legends* or
something but it’s meant to be exploratory not practical

``` r
library(patchwork)
smooth_plot +
  inset_element(
    wrap_elements(full = smooth_ribbon_gTree) +
      theme(plot.background = element_rect(fill = NA, color = NA)),
    left = 0.5, bottom = 0.5, right = 0.8, top = 0.8
  )
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="man/figures/README-ex-2-last-b-1.png" width="100%" />

But I will just add that the above way of “intercepting” and retrieving
a grob is kinda nice because you know what will be there ahead of the
time from your knowledge of that Geom (whereas if you wanted to grab a
grob after the ggplot is built, you’d have to navigate the whole
`ggplotGrob(smooth_plot)[["grobs"]]`).

## **Example 3 - `compute_panel` method from `StatBoxplot`**

### **Step 1. Make plot**

``` r
boxplot_plot <- ggplot(diamonds[1:500,], aes(cut, depth)) +
  geom_boxplot()
boxplot_plot
```

<img src="man/figures/README-ex-3-setup-1.png" width="100%" />

### **Step 2. Inspect callstack of the ggproto method**

Actually, `"compute_panel"` method is not defined for `StatBoxplot`,
which means that it’s being inherited.

``` r
ggbody(StatBoxplot$compute_panel)
#> Error in get(method, obj): object 'compute_panel' not found
```

`StatBoxplot` is a child of the parent ggproto `Stat`, and the
`"compute_panel"` method is inherited from `Stat` as well, so that’s
what we want to trace instead:

``` r
class(StatBoxplot)
#> [1] "StatBoxplot" "Stat"        "ggproto"     "gg"
```

``` r
ggbody(Stat$compute_panel)
#> [[1]]
#> `{`
#> 
#> [[2]]
#> if (empty(data)) return(new_data_frame())
#> 
#> [[3]]
#> groups <- split(data, data$group)
#> 
#> [[4]]
#> stats <- lapply(groups, function(group) {
#>     self$compute_group(data = group, scales = scales, ...)
#> })
#> 
#> [[5]]
#> stats <- mapply(function(new, old) {
#>     if (empty(new)) 
#>         return(new_data_frame())
#>     unique <- uniquecols(old)
#>     missing <- !(names(unique) %in% names(new))
#>     cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
#> }, stats, groups, SIMPLIFY = FALSE)
#> 
#> [[6]]
#> rbind_dfs(stats)
```

### **Step 3. `ggtrace()` - retrieve the parent environment**

`Stat$compute_panel` does split-apply-combine (Steps 3, 4-5, 6).

Let’s return the split and the combine:

``` r
ggtrace(
  Stat$compute_panel,
  trace_steps = c(3, 6, 6),
  trace_exprs = list(
    quote(~step),         # What are the splits?
    quote(~step),         # What does the combined result look like?
    quote(environment())  # Grab the method environment
  ),
  .print = FALSE
)

# plot not printed to save space
boxplot_plot
#> Tracing method compute_panel from <Stat> ggproto.
#> 
#>  [Step 3]> groups <- split(data, data$group) 
#> 
#>  [Step 6]> rbind_dfs(stats) 
#> 
#>  [Step 6]> environment() 
#> 
#> Untracing method compute_panel from <Stat> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

### **Step 4. Inspect trace dump**

``` r
boxplot_tracedump <- last_ggtrace()

# The splits
sapply(boxplot_tracedump[[1]], nrow)
#>   1   2   3   4   5 
#>  26  51 127 144 152
lapply(boxplot_tracedump[[1]], head)
#> $`1`
#>     x    y PANEL group
#> 9   1 65.1     1     1
#> 92  1 55.1     1     1
#> 98  1 66.3     1     1
#> 124 1 64.5     1     1
#> 125 1 65.3     1     1
#> 129 1 64.4     1     1
#> 
#> $`2`
#>    x    y PANEL group
#> 3  2 56.9     1     2
#> 5  2 63.3     1     2
#> 11 2 64.0     1     2
#> 18 2 63.4     1     2
#> 19 2 63.8     1     2
#> 21 2 63.3     1     2
#> 
#> $`3`
#>    x    y PANEL group
#> 6  3 62.8     1     3
#> 7  3 62.3     1     3
#> 8  3 61.9     1     3
#> 10 3 59.4     1     3
#> 20 3 62.7     1     3
#> 22 3 63.8     1     3
#> 
#> $`4`
#>    x    y PANEL group
#> 2  4 59.8     1     4
#> 4  4 62.4     1     4
#> 13 4 60.4     1     4
#> 15 4 60.2     1     4
#> 16 4 60.9     1     4
#> 27 4 62.5     1     4
#> 
#> $`5`
#>    x    y PANEL group
#> 1  5 61.5     1     5
#> 12 5 62.8     1     5
#> 14 5 62.2     1     5
#> 17 5 62.0     1     5
#> 40 5 61.8     1     5
#> 41 5 61.2     1     5

# Manually calculating some boxplot parameters
lapply(boxplot_tracedump[[1]], function(group) {
  quantile(group$y, c(0, 0.25, 0.5, 0.75, 1))
})
#> $`1`
#>     0%    25%    50%    75%   100% 
#> 53.100 58.850 64.800 65.775 68.100 
#> 
#> $`2`
#>    0%   25%   50%   75%  100% 
#> 56.90 60.20 63.30 63.95 65.20 
#> 
#> $`3`
#>   0%  25%  50%  75% 100% 
#> 57.5 60.7 62.0 63.1 64.0 
#> 
#> $`4`
#>     0%    25%    50%    75%   100% 
#> 58.000 60.700 61.500 62.325 63.000 
#> 
#> $`5`
#>    0%   25%   50%   75%  100% 
#> 58.80 61.30 61.75 62.20 62.90

# The combined result
boxplot_tracedump[[2]]
#>   ymin lower middle  upper ymax               outliers notchupper notchlower x
#> 1 53.1 58.85  64.80 65.775 68.1                          66.94580   62.65420 1
#> 2 56.9 60.20  63.30 63.950 65.2                          64.12967   62.47033 2
#> 3 57.5 60.70  62.00 63.100 64.0                          62.33649   61.66351 3
#> 4 58.3 60.70  61.50 62.325 63.0 58.0, 58.0, 58.2, 58.0   61.71396   61.28604 4
#> 5 60.1 61.30  61.75 62.200 62.9       58.8, 59.9, 59.9   61.86534   61.63466 5
#>   width relvarwidth flipped_aes PANEL group
#> 1  0.75    5.099020       FALSE     1     1
#> 2  0.75    7.141428       FALSE     1     2
#> 3  0.75   11.269428       FALSE     1     3
#> 4  0.75   12.000000       FALSE     1     4
#> 5  0.75   12.328828       FALSE     1     5

# Evaluate the expression in Step 6 with the cached method environment
eval(
  ggbody(Stat$compute_panel)[[6]],
  envir = boxplot_tracedump[[3]]
)
#>   ymin lower middle  upper ymax               outliers notchupper notchlower x
#> 1 53.1 58.85  64.80 65.775 68.1                          66.94580   62.65420 1
#> 2 56.9 60.20  63.30 63.950 65.2                          64.12967   62.47033 2
#> 3 57.5 60.70  62.00 63.100 64.0                          62.33649   61.66351 3
#> 4 58.3 60.70  61.50 62.325 63.0 58.0, 58.0, 58.2, 58.0   61.71396   61.28604 4
#> 5 60.1 61.30  61.75 62.200 62.9       58.8, 59.9, 59.9   61.86534   61.63466 5
#>   width relvarwidth flipped_aes PANEL group
#> 1  0.75    5.099020       FALSE     1     1
#> 2  0.75    7.141428       FALSE     1     2
#> 3  0.75   11.269428       FALSE     1     3
#> 4  0.75   12.000000       FALSE     1     4
#> 5  0.75   12.328828       FALSE     1     5

# What was inside the method environment?
ls(envir = boxplot_tracedump[[3]])
#> [1] "data"   "groups" "scales" "self"   "stats"

# Manually call the compute_group method from StatBoxplot to apply
# transformation to the first group using the method environment
eval(
  quote(StatBoxplot$compute_group(groups[[1]], scales, ...)),
  envir = boxplot_tracedump[[3]]
)
#>   ymin lower middle  upper ymax outliers notchupper notchlower x width
#> 1 53.1 58.85   64.8 65.775 68.1             66.9458    62.6542 1  0.75
#>   relvarwidth flipped_aes
#> 1     5.09902       FALSE
```

## **Example 4 - `compute_group` method from `StatSina` {ggforce}**

### **Step 1. Make plot**

``` r
library(ggforce) # v.0.3.3

sina_plot <- ggplot(diamonds[diamonds$cut == "Ideal",][1:50,], aes(cut, depth)) +
  geom_violin() +
  geom_sina(seed = 2021)
sina_plot
```

<img src="man/figures/README-ex-4-setup-1.png" width="100%" />

### **Step 2. Inspect callstack of the ggproto method**

``` r
ggbody(StatSina$compute_group)
#> [[1]]
#> `{`
#> 
#> [[2]]
#> if (nrow(data) == 0) return(NULL)
#> 
#> [[3]]
#> if (nrow(data) < 3) {
#>     data$density <- 0
#>     data$scaled <- 1
#> } else if (method == "density") {
#>     range <- range(data$y, na.rm = TRUE)
#>     bw <- calc_bw(data$y, bw)
#>     dens <- compute_density(data$y, data$w, from = range[1], 
#>         to = range[2], bw = bw, adjust = adjust, kernel = kernel)
#>     densf <- stats::approxfun(dens$x, dens$density, rule = 2)
#>     data$density <- densf(data$y)
#>     data$scaled <- data$density/max(dens$density)
#>     data
#> } else {
#>     bin_index <- cut(data$y, bins, include.lowest = TRUE, labels = FALSE)
#>     data$density <- tapply(bin_index, bin_index, length)[as.character(bin_index)]
#>     data$density[data$density <= bin_limit] <- 0
#>     data$scaled <- data$density/max(data$density)
#> }
#> 
#> [[4]]
#> if (length(unique(data$x)) > 1) {
#>     width <- diff(range(data$x)) * maxwidth
#> } else {
#>     width <- maxwidth
#> }
#> 
#> [[5]]
#> data$width <- width
#> 
#> [[6]]
#> data$n <- nrow(data)
#> 
#> [[7]]
#> data$x <- mean(range(data$x))
#> 
#> [[8]]
#> data
```

### **Step 3. `ggtrace()` - with one expression evaluated at multiple steps**

``` r
ggtrace(
  method = StatSina$compute_group,
  trace_steps = c(1, 8),
  trace_exprs = quote(data), # What does the data look like at start and end?
  .print = FALSE
)

# plot not printed to save space
sina_plot
#> Tracing method compute_group from <StatSina> ggproto.
#> 
#>  [Step 1]> data 
#> 
#>  [Step 8]> data 
#> 
#> Untracing method compute_group from <StatSina> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

### **Step 4. Inspect trace dump**

``` r
sina_tracedump <- last_ggtrace()

# The returned data has some new columns
waldo::compare(sina_tracedump[[1]], sina_tracedump[[2]])
#> `old` is length 4
#> `new` is length 8
#> 
#> `names(old)[2:4]`: "y" "PANEL" "group"                               
#> `names(new)[2:8]`: "y" "PANEL" "group" "density" "scaled" "width" "n"
#> 
#> `old$x` is an S3 object of class <mapped_discrete/numeric>, an integer vector
#> `new$x` is a double vector (1, 1, 1, 1, 1, ...)
#> 
#> `old$density` is absent
#> `new$density` is a double vector (0.46323885621947, 0.213496721362797, 0.497309776120959, 0.543803948542487, 0.539050969762089, ...)
#> 
#> `old$scaled` is absent
#> `new$scaled` is a double vector (0.845470465475205, 0.38965896311279, 0.907654274371047, 0.992512116219465, 0.983837318913703, ...)
#> 
#> `old$width` is absent
#> `new$width` is a double vector (0.9, 0.9, 0.9, 0.9, 0.9, ...)
#> 
#> `old$n` is absent
#> `new$n` is an integer vector (50, 50, 50, 50, 50, ...)
```
