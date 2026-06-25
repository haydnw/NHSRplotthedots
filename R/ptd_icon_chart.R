#' PTD Icon Chart
#'
#' Renders the Making Data Count variation and assurance icon(s) for a
#' `ptd_spc_df` object (not embedded in an SPC chart).
#'
#' The same call works in any output format:
#' \itemize{
#'   \item In RMarkdown/Quarto **HTML** documents the icons are emitted inline
#'     as crisp vector SVG, no graphics device required.
#'   \item In **other** formats (PowerPoint, Word, PDF) the icons are drawn to
#'     the chunk's graphics device. Set a vector device such as
#'     `dev = "svglite"` to embed them as SVG.
#' }
#'
#' When a target is set, two icons are shown side by side (assurance on the
#' left, variation on the right), matching the layout of [ptd_create_ggplot()].
#'
#' @param .data a `ptd_spc_df` object created with [ptd_spc()]
#'
#' @return an object of class `ptd_icon_chart`. Printing it (the default for a
#'   value returned from a knitr chunk) renders the icons: inline SVG for HTML
#'   output, otherwise drawn to the current graphics device.
#'
#' @export
ptd_icon_chart <- function(.data) {
  stopifnot(
    "`.data` must be a `ptd_spc_df` object created with `ptd_spc()`" =
      inherits(.data, "ptd_spc_df")
  )

  icon_data <- ptd_get_icons(.data)

  # order: assurance left, variation right — matches geom_ptd_icon layout.
  # ascending sort puts "assurance" before "variation".
  icon_data <- icon_data[order(icon_data$type), ]
  paths <- icon_data$icon

  structure(
    list(paths = paths),
    class = "ptd_icon_chart"
  )
}

# build the inline SVG markup string used for HTML output
ptd_icon_chart_svg <- function(paths) {
  if (length(paths) == 1L) {
    return(paste(readLines(paths, warn = FALSE), collapse = ""))
  }

  uris <- vapply(paths, read_svg_as_b64, character(1L))
  n <- length(uris)
  imgs <- paste(vapply(seq_along(uris), function(i) {
    sprintf(
      '<image href="%s" x="%d" y="0" width="378" height="378"/>',
      uris[[i]], (i - 1L) * 378L
    )
  }, character(1L)), collapse = "")
  sprintf(
    '<svg width="%d" height="378" xmlns="http://www.w3.org/2000/svg">%s</svg>',
    n * 378L, imgs
  )
}

# build a grid grob that draws the icons in a single row of equal-width cells.
# height is left NULL so rasterGrob keeps each icon's native (square) aspect.
ptd_icon_chart_grob <- function(paths) {
  n <- length(paths)
  grobs <- lapply(seq_len(n), function(i) {
    grid::rasterGrob(
      rsvg::rsvg_nativeraster(paths[[i]], width = 378L),
      x = grid::unit((i - 0.5) / n, "npc"),
      width = grid::unit(1 / n, "npc"),
      interpolate = TRUE
    )
  })
  grid::gTree(children = do.call(grid::gList, grobs))
}

#' @export
print.ptd_icon_chart <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(ptd_icon_chart_grob(x$paths))
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.ptd_icon_chart <- function(x, ...) {
  if (knitr::is_html_output()) {
    return(knitr::asis_output(ptd_icon_chart_svg(x$paths)))
  }
  # non-HTML: draw to the chunk's graphics device so knitr captures a figure.
  # use a vector device (e.g. dev = "svglite") to embed as SVG.
  print(x)
  invisible(NULL)
}
