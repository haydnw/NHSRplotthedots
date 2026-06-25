#' PTD Icon
#'
#' Returns the Making Data Count variation and assurance icon(s) for a
#' `ptd_spc_df` object as an SVG string. Renders inline automatically in
#' RMarkdown and Quarto HTML documents.
#'
#' When a target is set, two icons are returned side by side (assurance on the
#' left, variation on the right), matching the layout of [ptd_create_ggplot()].
#'
#' @param .data a `ptd_spc_df` object created with [ptd_spc()]
#'
#' @return a character string of class `"ptd_svg"` containing SVG markup
#'
#' @export
ptd_icon_chart <- function(.data) {
  stopifnot(
    "`.data` must be a `ptd_spc_df` object created with `ptd_spc()`" =
      inherits(.data, "ptd_spc_df")
  )

  icon_data <- ptd_get_icons(.data)

  # order: assurance left, variation right — matches geom_ptd_icon layout
  icon_data <- icon_data[order(icon_data$type, decreasing = TRUE), ]
  paths <- icon_data$icon

  if (length(paths) == 1L) {
    svg <- paste(readLines(paths, warn = FALSE), collapse = "")
  } else {
    uris <- vapply(paths, read_svg_as_b64, character(1L))
    n <- length(uris)
    imgs <- paste(vapply(seq_along(uris), function(i) {
      sprintf(
        '<image href="%s" x="%d" y="0" width="378" height="378"/>',
        uris[[i]], (i - 1L) * 378L
      )
    }, character(1L)), collapse = "")
    svg <- sprintf(
      '<svg width="%d" height="378" xmlns="http://www.w3.org/2000/svg">%s</svg>',
      n * 378L, imgs
    )
  }

  structure(svg, class = c("ptd_svg", "character"))
}

#' @exportS3Method knitr::knit_print
knit_print.ptd_svg <- function(x, ...) {
  knitr::asis_output(unclass(x))
}
