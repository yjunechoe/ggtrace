library(ggplot2)
library(ggforce)

StatDensityCommon <- ggproto("StatDensityCommon", Stat,
                             required_aes = "x",

                             setup_params = function(data, params) {
                               if (!is.null(params$bandwidth))
                                 return(params)

                               xs <- split(data$x, data$group)
                               bws <- vapply(xs, bw.nrd0, numeric(1))
                               bw <- mean(bws)
                               message("Picking bandwidth of ", signif(bw, 3))

                               params$bandwidth <- bw
                               params
                             },

                             compute_group = function(data, scales, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth)
                               data.frame(x = d$x, y = d$y)
                             }
)

test_that("ggbody gets body as list", {
  expect_equal(
    ggbody(StatBezier$compute_panel),
    as.list(body(get("compute_panel", StatBezier)))
  )
  expect_equal(
    ggbody(StatDensityCommon$compute_group),
    as.list(body(get("compute_group", StatDensityCommon))),
    ignore_attr = TRUE
  )
  expect_equal(
    ggbody(StatDensityCommon$setup_params),
    as.list(body(get("setup_params", StatDensityCommon))),
    ignore_attr = TRUE
  )
})

test_that("Works with :: and :::", {
  expect_equal(
    ggbody(ggplot2:::Layer$compute_position),
    as.list(body(get("compute_position", ggplot2:::Layer)))
  )
  expect_equal(
    ggbody(ggforce::StatBezier$compute_panel),
    as.list(body(get("compute_panel", StatBezier)))
  )
  expect_equal(
    ggbody(ggrepel::GeomTextRepel$draw_panel),
    as.list(body(get("draw_panel", ggrepel::GeomTextRepel)))
  )
})

test_that("errors if method missing or not defined for ggproto object", {
  expect_error(
    ggbody(StatBoxplot$compute_panel),
    "Method .* not defined for .*"
  )
  expect_error(
    ggbody(StatBoxplot$not_a_method),
    "Method .* not defined for .*"
  )
  expect_error(
    ggbody(ggrepel::GeomTextRepel$compute_panel),
    "Method .* not defined for .*"
  )
  expect_error(
    ggbody(StatDensityCommon$compute_panel),
    "Method .* not defined for .*"
  )
})

test_that("errors if method not found in parent(s)", {
  expect_error(
    ggbody(StatBoxplot$not_a_method, inherit = TRUE),
    "Method .* not inherited for .*"
  )
  expect_error(
    ggbody(StatDensityCommon$draw_layer, inherit = TRUE),
    "Method .* not inherited for .*"
  )
})

test_that("errors if object not defined", {
  expect_error(
    ggbody(NotAnObj$method),
    "not found"
  )
})

test_that("returns method from closest parent in a message", {
  expect_message(
    ggbody(GeomArcBar$default_aes, inherit = TRUE),
    "not inherited"
  )
  expect_message(
    ggbody(GeomArcBar$draw_panel, inherit = TRUE),
    "GeomShape\\$draw_panel"
  )
  expect_message(
    ggbody(GeomArcBar$draw_key, inherit = TRUE),
    "GeomPolygon\\$draw_key"
  )
  expect_message(
    ggbody(GeomArcBar$draw_group, inherit = TRUE),
    "Geom\\$draw_group"
  )
  expect_message(
    ggbody(StatDensityCommon$compute_panel, inherit = TRUE),
    "Stat\\$compute_panel"
  )
})

test_that("returns same with or without :: and :::", {
  expect_equal(
    ggbody(StatBin$compute_group),
    ggbody(ggplot2::StatBin$compute_group)
  )
  expect_equal(
    ggbody(StatBin$compute_group),
    ggbody(ggplot2:::StatBin$compute_group)
  )
  expect_equal(
    ggbody(FacetCol$draw_panels),
    ggbody(ggforce::FacetCol$draw_panels)
  )
})

test_that("handles non-functions", {
  expect_equal(
    ggbody(PositionJitter$required_aes),
    c("x", "y")
  )
  expect_equal(
    ggbody(GeomBar$extra_params),
    c("na.rm", "orientation")
  )
  expect_equal(
    ggbody(StatDensityCommon$required_aes),
    "x"
  )
})
