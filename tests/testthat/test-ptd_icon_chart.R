library(testthat)

make_spc <- function(target = NULL, facet = FALSE) {
  withr::local_options(ptd_spc.warning_threshold = 0)
  set.seed(1)
  if (facet) {
    d <- data.frame(
      x = rep(as.Date("2020-01-01") + 1:24, 3),
      y = rnorm(72),
      org = rep(c("A", "B", "C"), each = 24)
    )
    ptd_spc(d, "y", "x", facet_field = "org", target = target)
  } else {
    d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
    ptd_spc(d, "y", "x", target = target)
  }
}

test_that("ptd_icon_chart errors on non-ptd_spc_df input", {
  expect_error(ptd_icon_chart(data.frame()), "ptd_spc_df")
})

test_that("ptd_icon_chart validates ncol", {
  s <- make_spc()
  expect_error(ptd_icon_chart(s, ncol = 0), "ncol")
  expect_error(ptd_icon_chart(s, ncol = c(1, 2)), "ncol")
})

test_that("ptd_icon_chart returns a ptd_icon_chart carrying icon data", {
  s <- make_spc()
  result <- ptd_icon_chart(s)

  expect_s3_class(result, "ptd_icon_chart")
  expect_named(result, c("icon_data", "ncol"))
  expect_setequal(c("f", "type", "icon"), names(result$icon_data))
})

test_that("layout is a single unlabelled cell when unfaceted", {
  lay <- ptd_icon_chart_layout(ptd_icon_chart(make_spc())$icon_data, NULL)

  expect_false(lay$faceted)
  expect_equal(lay$ncol, 1L)
  expect_equal(lay$nrow, 1L)
})

test_that("layout wraps facets into a roughly square grid by default", {
  lay <- ptd_icon_chart_layout(ptd_icon_chart(make_spc(facet = TRUE))$icon_data, NULL)

  expect_true(lay$faceted)
  expect_equal(lay$n, 3L)
  # 3 facets -> ceiling(sqrt(3)) = 2 columns, 2 rows
  expect_equal(lay$ncol, 2L)
  expect_equal(lay$nrow, 2L)
})

test_that("ncol overrides the default column count", {
  lay <- ptd_icon_chart_layout(ptd_icon_chart(make_spc(facet = TRUE))$icon_data, ncol = 3)

  expect_equal(lay$ncol, 3L)
  expect_equal(lay$nrow, 1L)
})

test_that("ptd_icon_chart_grob draws one cell per facet", {
  grob <- ptd_icon_chart_grob(ptd_icon_chart(make_spc(facet = TRUE))$icon_data, NULL)

  expect_s3_class(grob, "gTree")
  expect_length(grob$children, 3L)
})

test_that("a target adds a second icon to each facet cell", {
  grob <- ptd_icon_chart_grob(
    ptd_icon_chart(make_spc(facet = TRUE, target = 0.5))$icon_data, NULL
  )

  # each cell holds two icon grobs plus a label = 3 children
  cell <- grob$children[[1]]
  expect_length(cell$children, 3L)
})

test_that("print.ptd_icon_chart draws invisibly to the device", {
  s <- make_spc()

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(print(ptd_icon_chart(s)))
})

test_that("inline SVG embeds one image per icon, no labels when unfaceted", {
  svg <- ptd_icon_chart_svg(ptd_icon_chart(make_spc())$icon_data, NULL)

  expect_true(startsWith(svg, "<svg"))
  expect_equal(lengths(regmatches(svg, gregexpr("<image ", svg))), 1L)
  expect_equal(lengths(regmatches(svg, gregexpr("<text ", svg))), 0L)
})

test_that("inline SVG labels each facet and embeds its icon", {
  svg <- ptd_icon_chart_svg(ptd_icon_chart(make_spc(facet = TRUE))$icon_data, NULL)

  expect_equal(lengths(regmatches(svg, gregexpr("<image ", svg))), 3L)
  expect_equal(lengths(regmatches(svg, gregexpr("<text ", svg))), 3L)
  expect_match(svg, ">A</text>")
  expect_match(svg, ">B</text>")
  expect_match(svg, ">C</text>")
})

test_that("inline SVG places two icons side by side when target set", {
  svg <- ptd_icon_chart_svg(ptd_icon_chart(make_spc(target = 0.5))$icon_data, NULL)

  # two icons (assurance left at x=0, variation right at x=378)
  expect_equal(lengths(regmatches(svg, gregexpr("<image ", svg))), 2L)
  expect_match(svg, '<image href[^>]+ x="0"')
  expect_match(svg, '<image href[^>]+ x="378"')
})

test_that("knit_print emits an inline SVG for HTML output", {
  skip_if_not_installed("knitr")

  mockery::stub(knit_print.ptd_icon_chart, "knitr::is_html_output", TRUE)
  result <- knit_print.ptd_icon_chart(ptd_icon_chart(make_spc()))

  expect_s3_class(result, "knit_asis")
  expect_match(as.character(result), "<svg")
})

test_that("knit_print draws to the device for non-HTML output", {
  skip_if_not_installed("knitr")

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  mockery::stub(knit_print.ptd_icon_chart, "knitr::is_html_output", FALSE)
  result <- knit_print.ptd_icon_chart(ptd_icon_chart(make_spc()))

  expect_null(result)
})
