<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrace <img class="logo" src="man/figures/logo.png" align="right" style="width:120px;" />

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.7.7-gogreen.svg)](https://github.com/yjunechoe/ggtrace)
[![test-with-dev-ggplot2](https://github.com/yjunechoe/ggtrace/actions/workflows/test-with-dev-ggplot2.yaml/badge.svg)](https://github.com/yjunechoe/ggtrace/actions/workflows/test-with-dev-ggplot2.yaml)
[![R-CMD-check](https://github.com/yjunechoe/ggtrace/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yjunechoe/ggtrace/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/yjunechoe/ggtrace/graph/badge.svg)](https://app.codecov.io/gh/yjunechoe/ggtrace)
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/yjunechoe/ggtrace/) with:

``` r
# install.packages("remotes")
remotes::install_github("yjunechoe/ggtrace")
```

More on the 📦 package website: <https://yjunechoe.github.io/ggtrace>

## Getting started

`{ggtrace}` is a **functional interface to ggplot2 internals**. A little
bit of knowledge about `{ggplot2}` internals is required to make the
most use out of `{ggtrace}`. You can watch one of these videos for a
quick showcase.

- [Talk at JSM 2023](https://youtu.be/613Q0j6Kjm0?feature=shared)
  ([paper](https://yjunechoe.github.io/static/papers/Choe_2022_SublayerGG.pdf))

- [Talk at
  rstudio::conf(2022)](https://www.youtube.com/watch?v=dUBnitXf5mk&list=PL9HYL-VRX0oTOwqzVtL_q5T8MNrzn0mdH&index=38)
  ([materials](https://github.com/yjunechoe/ggtrace-rstudioconf2022)).

- [Talk at useR!
  2022](https://www.youtube.com/watch?v=2JX8zu4QxMg&t=2959s)
  ([materials](https://github.com/yjunechoe/ggtrace-user2022)).

## Description

Broadly speaking, `{ggtrace}` was designed with two goals in mind:

### 1. Understanding Sublayer Modularity

To help users write more expressive layer code using [delayed aesthetic
evaluation](https://ggplot2.tidyverse.org/reference/aes_eval.html). The
family of `layer_*()` extractor functions return snapshots of layer data
in the internals, to help scaffold a mental model of `{ggplot2}`
internals as a data wrangling pipeline. See `` ?`sublayer-data` ``

<p align="center">

<img src="https://i.imgur.com/OlLmz8r.png" width="550" alt="Sublayer modularity diagram">
</p>

### 2. Facilitating the User-Developer Transition

To empower users to start developing their own extensions This is
achieved via a family of `inspect_*()`, `capture_*()`, and
`highjack_*()` workflow functions, which provide a functional interface
into the object-oriented design of the internals (the `<ggproto>` OOP).

<p align="center">

<img src="https://i.imgur.com/kpTffyw.jpg" width="450" alt="User-developer transition diagram">
</p>

You can read the full motivation behind `{ggtrace}` in the [Pedagogical
Philosophy](https://yjunechoe.github.io/ggtrace/articles/pedagogical-philosophy.html)
vignette.

## Playground

The best way to learn `{ggtrace}` and `{ggplot2}` internals is by
playing around and breaking things as you go.

In that spirit, you can take `{ggtrace}` for a spin on the [ggplot2
Layer Explorer](https://yjunechoe.github.io/ggplot2-layer-explorer/)
shiny app.

<https://github.com/user-attachments/assets/d885ccb5-6ebb-435b-a717-4c0955c1da4b>
