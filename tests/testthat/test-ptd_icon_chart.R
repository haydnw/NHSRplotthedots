library(testthat)

test_that("ptd_icon_chart errors on non-ptd_spc_df input", {
  expect_error(ptd_icon_chart(data.frame()), "ptd_spc_df")
})

test_that("ptd_icon_chart returns ptd_svg character for single icon", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  result <- ptd_icon_chart(s)

  expect_s3_class(result, "ptd_svg")
  expect_type(result, "character")
  expect_true(startsWith(result, "<svg"))
})

test_that("ptd_icon_chart returns combined SVG for two icons when target set", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x", target = 0.5)

  result <- ptd_icon_chart(s)

  expect_s3_class(result, "ptd_svg")
  # combined SVG wraps two icons side by side (756 wide)
  expect_match(result, 'width="756"')
  expect_match(result, 'height="378"')
  # both icon data URIs embedded
  expect_equal(lengths(regmatches(result, gregexpr("<image ", result))), 2L)
})

test_that("ptd_icon_chart puts assurance left and variation right", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x", target = 0.5)

  result <- ptd_icon_chart(s)

  # first <image> should be at x=0 (assurance), second at x=378 (variation)
  expect_match(result, '<image href[^>]+x="0"')
  expect_match(result, '<image href[^>]+x="378"')
})

test_that("knit_print.ptd_svg returns the raw svg as asis output", {
  skip_if_not_installed("knitr")

  svg <- structure("<svg>x</svg>", class = c("ptd_svg", "character"))

  result <- knit_print.ptd_svg(svg)

  expect_s3_class(result, "knit_asis")
  # the svg markup is emitted verbatim, without the ptd_svg class attribute
  expect_equal(as.character(result), "<svg>x</svg>")
})
