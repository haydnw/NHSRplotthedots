# NHSRplotthedots 0.2.2.9000

## New features
* Added `ptd_icon_chart()`, which renders the Making Data Count variation and assurance icon(s) for a `ptd_spc_df` object (not embedded in an SPC chart). In RMarkdown and Quarto HTML documents the icons are emitted inline as vector SVG; in other formats (PowerPoint, Word, PDF) they are drawn to the chunk's graphics device, so setting a vector device such as `dev = "svglite"` embeds them as SVG.
* `ptd_create_ggplot()` (and the `plot()` method for `ptd_spc_df` objects) gains a `show_icons` argument (default `TRUE`) to control whether the variation and assurance icons are drawn on the plot.

## Bugfixes
* `ptd_icon_chart()` now places the assurance icon on the left and the variation icon on the right when a target is set, matching its documentation and the `ptd_create_ggplot()` layout (they were previously reversed).

# NHSRplotthedots 0.2.2

This second version of 'Plot the Dots' adds new features and bugfixes to the original version.  They are summarised below:

## New features
* Improved icons added for both variation and assurance types.
* Ploty implementation added with `ptd_create_plotly()`
* Process limits and mean can now be annotated on the right of the plot with `ptd_create_ggplot(... label_limits = TRUE)`.

## Bugfixes
* Fixed an issue where points on opposite sides of the mean could incorrectly trigger "2-in-3" special-cause flags (issue #143).
* Fixed an issue where trends of 7 points were coloured differently depending on whether they are increasing or decreasing (issue #167).
* Fixed an issue where trends of 7 points crossing the mean are coloured incorrectly (issue #171)
* Standardised icon and point colours with the MDC team PowerBI team (issue #196).

# NHSRplotthedots 0.1.0

This is first release for the NHS-R Community's 'Plot the Dots' package.  This supports NHS England and NHS Improvement's 'Making Data Count' campaign which encourages the use of run charts and statistical process control charts for more informed decision making.  This package creates methods for XmR charts, with consideration for multiple indicators with facets, 'rebasing', and applying rules sets with summary indicators.  This package is for use across the NHS, augmenting existing tools in SQL and MS Excel.
