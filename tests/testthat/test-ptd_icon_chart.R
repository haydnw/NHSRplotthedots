library(testthat)

test_that("ptd_icon_chart errors on non-ptd_spc_df input", {
  expect_error(ptd_icon_chart(data.frame()), "ptd_spc_df")
})

test_that("ptd_icon_chart returns a ptd_icon_chart object", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  result <- ptd_icon_chart(s)

  expect_s3_class(result, "ptd_icon_chart")
  expect_length(result$paths, 1L)
})

test_that("ptd_icon_chart carries two icon paths when target set", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x", target = 0.5)

  result <- ptd_icon_chart(s)

  expect_length(result$paths, 2L)
  # assurance left, variation right
  expect_match(result$paths[[1]], "assurance")
  expect_match(result$paths[[2]], "variation")
})

test_that("inline SVG is a single inlined svg for one icon", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  svg <- ptd_icon_chart_svg(ptd_icon_chart(s)$paths)

  expect_true(startsWith(svg, "<svg"))
})

test_that("inline SVG combines two icons side by side when target set", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x", target = 0.5)

  svg <- ptd_icon_chart_svg(ptd_icon_chart(s)$paths)

  # combined SVG wraps two icons side by side (756 wide)
  expect_match(svg, 'width="756"')
  expect_match(svg, 'height="378"')
  # both icon data URIs embedded, assurance at x=0, variation at x=378
  expect_equal(lengths(regmatches(svg, gregexpr("<image ", svg))), 2L)
  expect_match(svg, '<image href[^>]+x="0"')
  expect_match(svg, '<image href[^>]+x="378"')
})

test_that("knit_print emits inline SVG for HTML output", {
  skip_if_not_installed("knitr")
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  # is_html_output() reads knitr's output format
  mockery::stub(knit_print.ptd_icon_chart, "knitr::is_html_output", TRUE)
  result <- knit_print.ptd_icon_chart(ptd_icon_chart(s))

  expect_s3_class(result, "knit_asis")
  expect_true(startsWith(as.character(result), "<svg"))
})

test_that("knit_print draws to the device for non-HTML output", {
  skip_if_not_installed("knitr")
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  mockery::stub(knit_print.ptd_icon_chart, "knitr::is_html_output", FALSE)
  result <- knit_print.ptd_icon_chart(ptd_icon_chart(s))

  expect_null(result)
})

test_that("print.ptd_icon_chart draws invisibly to the device", {
  withr::local_options(ptd_spc.warning_threshold = 0)

  set.seed(1)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24, y = rnorm(24))
  s <- ptd_spc(d, "y", "x")

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(print(ptd_icon_chart(s)))
})
