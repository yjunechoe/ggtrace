library(ggplot2)

p <- ggplot(diamonds, aes(cut)) +
  geom_bar(aes(fill = cut)) +
  stat_count(geom = "label", aes(label = after_stat(count)))

test_that("sublayer data targeting works", {

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
