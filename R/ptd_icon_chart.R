#' PTD Icon Chart
#'
#' Renders the Making Data Count variation and assurance icon(s) for a
#' `ptd_spc_df` object (not embedded in an SPC chart).
#'
#' The same call works in any output format:
#' \itemize{
#'   \item In RMarkdown/Quarto **HTML** documents the icons are emitted inline
#'     as SVG, no graphics device required.
#'   \item In **other** formats (PowerPoint, Word, PDF) the icons are drawn to
#'     the chunk's graphics device. Set a vector device such as
#'     `dev = "svglite"` to embed them as SVG.
#' }
#'
#' When a target is set, two icons are shown side by side (assurance on the
#' left, variation on the right), matching the layout of [ptd_create_ggplot()].
#'
#' When the `ptd_spc_df` is faceted, one labelled cell is drawn per facet,
#' arranged in a grid. Use `ncol` to control the number of columns.
#'
#' @param .data a `ptd_spc_df` object created with [ptd_spc()]
#' @param ncol the number of columns to arrange faceted icons into. The default
#'   (`NULL`) uses a single column when unfaceted, otherwise a roughly square
#'   grid.
#'
#' @return an object of class `ptd_icon_chart`. Printing it (the default for a
#'   value returned from a knitr chunk) renders the icons: inline SVG for HTML
#'   output, otherwise drawn to the current graphics device.
#'
#' @export
ptd_icon_chart <- function(.data, ncol = NULL) {
  stopifnot(
    "`.data` must be a `ptd_spc_df` object created with `ptd_spc()`" =
      inherits(.data, "ptd_spc_df"),
    "`ncol` must be a single positive number, or NULL" =
      is.null(ncol) || (is.numeric(ncol) && length(ncol) == 1L && ncol >= 1L)
  )

  icon_data <- ptd_get_icons(.data)

  structure(
    list(icon_data = icon_data, ncol = ncol),
    class = "ptd_icon_chart"
  )
}

# work out the grid arrangement: a row of facet cells wrapped into `ncol`
# columns. unfaceted data carries the single facet "no facet", which is drawn
# as a single unlabelled cell.
ptd_icon_chart_layout <- function(icon_data, ncol) {
  facets <- unique(icon_data$f)
  n <- length(facets)
  faceted <- !(n == 1L && facets[[1L]] == "no facet")

  if (is.null(ncol)) {
    ncol <- if (faceted) ceiling(sqrt(n)) else 1L
  }
  ncol <- min(as.integer(ncol), n)
  nrow <- ceiling(n / ncol)

  list(facets = facets, n = n, faceted = faceted, ncol = ncol, nrow = nrow)
}

# build a grid grob laying the icons out in a labelled grid. each facet cell
# holds its icon(s) (assurance left, variation right) above an optional label.
ptd_icon_chart_grob <- function(icon_data, ncol) {
  lay <- ptd_icon_chart_layout(icon_data, ncol)

  # height of the label strip as a fraction of each cell (0 when unfaceted)
  label_frac <- if (lay$faceted) 0.18 else 0

  cells <- lapply(seq_len(lay$n), function(i) {
    fct <- lay$facets[[i]]
    rws <- icon_data[icon_data$f == fct, , drop = FALSE]
    # assurance left, variation right — matches geom_ptd_icon layout
    rws <- rws[order(rws$type), ]
    k <- nrow(rws)

    pos_row <- ((i - 1L) %/% lay$ncol) + 1L
    pos_col <- ((i - 1L) %% lay$ncol) + 1L

    # icon area sits above the label strip
    icon_vp <- grid::viewport(
      y = grid::unit(label_frac, "npc"),
      height = grid::unit(1 - label_frac, "npc"),
      just = "bottom"
    )
    icons <- lapply(seq_len(k), function(j) {
      grid::rasterGrob(
        rsvg::rsvg_nativeraster(rws$icon[[j]], width = 378L),
        x = grid::unit((j - 0.5) / k, "npc"),
        width = grid::unit(0.92, "snpc"),
        height = grid::unit(0.92, "snpc"),
        vp = icon_vp
      )
    })

    label <- if (lay$faceted) {
      list(grid::textGrob(
        fct,
        y = grid::unit(label_frac / 2, "npc"),
        gp = grid::gpar(fontsize = 10, fontfamily = "sans")
      ))
    } else {
      list()
    }

    grid::gTree(
      children = do.call(grid::gList, c(icons, label)),
      vp = grid::viewport(layout.pos.row = pos_row, layout.pos.col = pos_col)
    )
  })

  grid::gTree(
    children = do.call(grid::gList, cells),
    vp = grid::viewport(layout = grid::grid.layout(lay$nrow, lay$ncol))
  )
}

#' @export
print.ptd_icon_chart <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(ptd_icon_chart_grob(x$icon_data, x$ncol))
  invisible(x)
}

# build a standalone inline SVG string laying the icons out in the same
# labelled grid as ptd_icon_chart_grob(), for HTML output. icons are embedded
# as base64 data URIs of their (vector) SVG files, so they stay crisp.
ptd_icon_chart_svg <- function(icon_data, ncol) {
  lay <- ptd_icon_chart_layout(icon_data, ncol)

  icon_px <- 378L
  gap <- if (lay$faceted) 40L else 0L
  label_h <- if (lay$faceted) 90L else 0L

  # all facets share the same icon count (1, or 2 when a target is set)
  k <- max(vapply(lay$facets, function(f) sum(icon_data$f == f), integer(1L)))
  cell_w <- k * icon_px
  cell_h <- icon_px + label_h

  cells <- vapply(seq_len(lay$n), function(i) {
    fct <- lay$facets[[i]]
    rws <- icon_data[icon_data$f == fct, , drop = FALSE]
    # assurance left, variation right — matches geom_ptd_icon layout
    rws <- rws[order(rws$type), ]

    pos_row <- (i - 1L) %/% lay$ncol
    pos_col <- (i - 1L) %% lay$ncol
    x0 <- pos_col * (cell_w + gap)
    y0 <- pos_row * (cell_h + gap)

    # centre this facet's icon(s) within the cell width
    inset <- (cell_w - nrow(rws) * icon_px) / 2
    imgs <- paste(vapply(seq_len(nrow(rws)), function(j) {
      sprintf(
        '<image href="%s" x="%d" y="%d" width="%d" height="%d"/>',
        read_svg_as_b64(rws$icon[[j]]),
        as.integer(x0 + inset + (j - 1L) * icon_px), y0, icon_px, icon_px
      )
    }, character(1L)), collapse = "")

    if (lay$faceted) {
      imgs <- paste0(imgs, sprintf(
        paste0(
          '<text x="%d" y="%d" text-anchor="middle" ',
          'font-family="sans-serif" font-size="44">%s</text>'
        ),
        as.integer(x0 + cell_w / 2), as.integer(y0 + icon_px + 60), fct
      ))
    }
    imgs
  }, character(1L))

  total_w <- lay$ncol * cell_w + (lay$ncol - 1L) * gap
  total_h <- lay$nrow * cell_h + (lay$nrow - 1L) * gap
  sprintf(
    '<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">%s</svg>',
    total_w, total_h, paste(cells, collapse = "")
  )
}

#' @exportS3Method knitr::knit_print
knit_print.ptd_icon_chart <- function(x, ...) {
  if (knitr::is_html_output()) {
    return(knitr::asis_output(ptd_icon_chart_svg(x$icon_data, x$ncol)))
  }
  # non-HTML: draw to the chunk's graphics device so knitr captures a figure.
  # use a vector device (e.g. dev = "svglite") to embed as SVG.
  print(x)
  invisible(NULL)
}
