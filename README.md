
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {ggtrace}

<!-- badges: start -->
<!-- badges: end -->

An experimental package for programmatically debugging ggproto methods
with `trace()`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yjunechoe/ggtrace")
```

## Usage

``` r
library(ggtrace)
library(ggplot2) # v3.3.5
#> Warning: package 'ggplot2' was built under R version 4.1.1
```

### Example - `compute_layer` method from `PositionJitter`

``` r
jitter_plot <- ggplot(diamonds[1:1000,], aes(cut, depth)) +
  geom_point(position = position_jitter(width = 0.2, seed = 2021))
jitter_plot
```

<img src="man/figures/README-ex-1-setup-1.png" width="100%" />

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

``` r
ggtrace(
  method = PositionJitter$compute_layer,
  trace_steps = c(1, 1, 9, 12),
  trace_exprs = rlang::exprs(
    head(data),        # What does the data passed in look like?
    params,            # What do the initial parameters look like?
    head(dummy_data),  # What is `dummy_data` defined at Step 8?
    head(~line)        # What does the last line evaluate to?
                       # - i.e., what is returned by the method?
  )
)

# plot not printed to save space
jitter_plot
#> Tracing method compute_layer from <PstnJttr> ggproto.
#> 
#>  [Step 1]> head(data) 
#>   x    y PANEL group
#> 1 5 61.5     1     5
#> 2 4 59.8     1     4
#> 3 2 56.9     1     2
#> 4 4 62.4     1     4
#> 5 2 63.3     1     2
#> 6 3 62.8     1     3
#> 
#>  [Step 1]> params 
#> $width
#> [1] 0.2
#> 
#> $height
#> [1] 0.04
#> 
#> $seed
#> [1] 2021
#> 
#> 
#>  [Step 9]> head(dummy_data) 
#>   x    y
#> 1 5 61.5
#> 2 4 59.8
#> 3 2 56.9
#> 4 4 62.4
#> 5 2 63.3
#> 6 3 62.8
#> 
#>  [Step 12]> head(~line) 
#>          x        y PANEL group
#> 1 4.980507 61.50684     1     5
#> 2 4.113512 59.77872     1     4
#> 3 2.083873 56.86655     1     2
#> 4 3.952698 62.42703     1     4
#> 5 2.054530 63.29763     1     2
#> 6 3.080538 62.77536     1     3
#> 
#> Untracing method compute_layer from <PstnJttr> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

``` r
last_ggtrace()
#> $`[Step 1]> head(data)`
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
#> $`[Step 9]> head(dummy_data)`
#>   x    y
#> 1 5 61.5
#> 2 4 59.8
#> 3 2 56.9
#> 4 4 62.4
#> 5 2 63.3
#> 6 3 62.8
#> 
#> $`[Step 12]> head(~line)`
#>          x        y PANEL group
#> 1 4.980507 61.50684     1     5
#> 2 4.113512 59.77872     1     4
#> 3 2.083873 56.86655     1     2
#> 4 3.952698 62.42703     1     4
#> 5 2.054530 63.29763     1     2
#> 6 3.080538 62.77536     1     3
```

``` r
ggtrace(
  method = PositionJitter$compute_layer,
  trace_steps = c(1, 1, 9, 12),
  trace_exprs = rlang::exprs(
    data,
    params,
    dummy_data,
    ~line
  ),
  .print = FALSE
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
#>  [Step 12]> ~line 
#> 
#> Untracing method compute_layer from <PstnJttr> ggproto.
#> Call `last_ggtrace()` to get the trace dump.
```

``` r
lapply(last_ggtrace(), head)
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
#> $`[Step 12]> ~line`
#>          x        y PANEL group
#> 1 4.980507 61.50684     1     5
#> 2 4.113512 59.77872     1     4
#> 3 2.083873 56.86655     1     2
#> 4 3.952698 62.42703     1     4
#> 5 2.054530 63.29763     1     2
#> 6 3.080538 62.77536     1     3
```
