#' Plot bar and line plot in same figure
#'
#' @param dataset
#' @param x_var
#' @param scale_interval
#' @param main_axis_name
#' @param sec_axis_name
#' @param x_axis_name
#' @param bar_fill_palette
#' @param line_color
#' @param point_color
#' @param legend_pos
#' @param bar_var
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang !! ensym
bar_line_ggplot <- function(
  dataset, x_var, bar_var = "sample_size", scale_blank_div = 5, scale_interval = NA,
  main_axis_name, sec_axis_name, x_axis_name,
  bar_fill_palette = "grey", line_color = "black", point_color = "white",
  legend_pos = c("none", "left", "right", "bottom", "top"),
  label_size
) {
  # use first column as x axis variable if `x_var` is not giving
  x_var <- ifelse(!missing(x_var), rlang::quo_name(x_var), colnames(dataset)[1])
  bar_var <- "sample_size"
  line_var <- base::setdiff(colnames(dataset), c(x_var, bar_var))[1]
  # cat(x_var, bar_var, line_var, collapse = "\n")

  # get sacle params and funcs for secondary axis
  if (is.na(scale_interval) || !is.numeric(scale_interval) || length(scale_interval) != 2) {
    scale_blank <- max(dataset[[bar_var]]) / scale_blank_div
    scale_interval <- c(scale_blank, max(dataset[[bar_var]]) - scale_blank)
  }
  base_interval <- c(min(dataset[[line_var]]), max(dataset[[line_var]]))
  scale_factor <- diff(scale_interval) / diff(base_interval)
  scale_func <- function(x) {
    return(scale_interval[1] + scale_factor * (x - base_interval[1]))
  }
  inv_scale_func <- function(x) {
    return((x - scale_interval[1]) / scale_factor + base_interval[1])
  }

  # auto-detect column names
  colnames_map <- getOption("report.options")[["colnameMap"]]
  if (is.null(colnames_map)) {
    message("You donnot set the `report.options$colnameMap` in `options()` before using name auto-detection.")
    message("We will use given params or column names of dataset to auto-detect axis name in figure.")
    colnames_map <- list()
    colnames_map[[bar_var]] = bar_var
    colnames_map[[line_var]] = line_var
    colnames_map[[x_var]] = x_var
  }
  if (missing(main_axis_name)) {
    main_axis_name <- ifelse(
      !is.null(colnames_map[[bar_var]]), colnames_map[[bar_var]], bar_var
    )
  }
  if (missing(sec_axis_name)) {
    sec_axis_name <- ifelse(
      !is.null(colnames_map[[line_var]]), colnames_map[[line_var]], line_var
    )
  }
  if (missing(x_axis_name)) {
    x_axis_name <- ifelse(
      !is.null(colnames_map[[x_var]]), colnames_map[[x_var]], x_var
    )
  }

  # generate bar palette
  if (length(bar_fill_palette) < nlevels(dataset[[x_var]])) {
    bar_fill_palette <- rep(bar_fill_palette[1], nlevels(dataset[[x_var]]))
  }

  p <- ggplot(data = dataset) +
    geom_col(
      mapping = aes(x = !!ensym(x_var), y = !!ensym(bar_var), fill = !!ensym(x_var)),
      width = 0.6
    ) +
    scale_fill_manual(values = bar_fill_palette) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~inv_scale_func(.), name = sec_axis_name)) +
    geom_line(
      mapping = aes(x = !!ensym(x_var), y = scale_func(!!ensym(line_var)), group = 1),
      color = line_color, size = 1.5
    ) +
    geom_point(
      mapping = aes(x = !!ensym(x_var), y = scale_func(!!ensym(line_var))),
      color = point_color, size = 2
    ) +
    geom_label(
      mapping = aes(
        x = !!ensym(x_var), y = scale_func(!!ensym(line_var)),
        label = paste(round(!!rlang::ensym(line_var), 2), '%'),
        label.paddng = 0
      ),
      vjust = -0.5
    ) +
    geom_text(
      mapping = aes(
        x = !!ensym(x_var), y = !!ensym(bar_var),
        label = !!ensym(bar_var)
      ),
      vjust = -0.5,
      show.legend = FALSE
    ) +
    labs(x = x_axis_name, y = main_axis_name) +
    theme(
      axis.title = element_text(size = 13),
      legend.position = legend_pos[1]
    ) +
    geom_blank()
  return(p)
}
