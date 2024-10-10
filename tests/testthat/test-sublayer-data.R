library(ggplot2)

test_that("sublayer data targeting works", {

  p <- ggplot(diamonds, aes(cut)) +
    geom_bar(aes(fill = cut)) +
    stat_count(geom = "label", aes(label = after_stat(count)))

  expect_in(
    c("x", "PANEL", "group"),
    colnames(layer_before_stat(p))
  )

  expect_in(
    c("count", "x", "PANEL", "group"),
    colnames(layer_after_stat(p))
  )

  expect_in(
    c("y", "x", "PANEL", "group"),
    colnames(layer_before_geom(p))
  )

  expect_in(
    c("alpha", "PANEL", "group"),
    colnames(layer_after_scale(p))
  )

})

test_that("sublayer data can target layer by index", {

  p <- ggplot(diamonds) +
    geom_bar(aes(cut, fill = cut)) +
    geom_blank() +
    stat_count(geom = "label", aes(cut, label = after_stat(count)))

  all_stages <- list(
    layer_before_stat(p, 2),
    layer_after_stat(p, 2),
    layer_before_geom(p, 2),
    layer_after_scale(p, 2)
  )

  expect_identical(
    Reduce(function(x, y) if (identical(x, y)) x else FALSE, all_stages),
    all_stages[[1]]
  )

  expect_no_error({
    layer_before_stat(p, 1)
    layer_before_stat(p, 2)
    layer_before_stat(p, 3)
  })

})

test_that("checks throwing errors", {

  expect_error({
    layer_before_stat(p, 0)
    layer_before_stat(p, 4)
  })

  expect_error({
    layer_before_stat(mean)
    layer_before_stat(1)
  })

  expect_error({
    layer_before_stat(arg_to_dots = 1)
  })

  expect_error({
    layer_before_stat(stop("stop"))
  })

})
