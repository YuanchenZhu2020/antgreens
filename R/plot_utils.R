#' Plot bar and line figure in the same figure
#'
#' @description
#' This function use the given \code{x_var} and \code{bar_var} to draw bar plot but
#' use the given \code{x_var} and the first column in \code{dataset} except \code{x_var} and
#' \code{bar_var} to draw line plot. The principle of drawing bar and line plot in one figure is
#' to map the graduation of line plot to the main Y axis and write the actual graduation of line
#' plot to the secondary Y axis.
#'
#' You can control the top and bottom blank of line plot using
#' parameters \code{scale_blank_div} and specify the range of line plot in the main Y axis using
#' parameters \code{scale_interval}. These two parameters can easily control the scope of line plot
#' to make it not conflict with bar plot.
#'
#' This function has the auto-detect function to change variable name in English to axis name in
#' Chinese. The information in \code{option} you have set before run r plot code is used as a
#' convertor.
#'
#' @param dataset data frame containing at least 3 columns.
#' @param x_var character. The column name that will be the X axis in the plot.
#' @param bar_var character, default \code{"sample_size"}. The column name that will be used to
#'   draw bar plot.
#' @param scale_blank_div numeric, default 5. It directly affect the proportion of the figure
#'   height that line plot occupied. The line plot in the figure will occupies
#'   \eqn{\frac{scale_blank_div-2}{scale_blank_div}} of the figure height and leave blanks in the
#'   top and bottom \eqn{\frac{1}{scale_blank_div}}.
#' @param scale_interval numeric vector of length 2. The interval of line plot in the main Y axis.
#' @param main_axis_name character. The name of main Y axis. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{bar_var}.
#' @param sec_axis_name character. The name of secondary Y axis. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{line_var}.
#' @param x_axis_name character. The name of X axis. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{x_var}.
#' @param bar_fill_palette character of character vector. It is the color palette of bar plot.
#' @param line_color character of color. It is the color of line in line plot.
#' @param point_color character of color. It is the color of point in line plot.
#' @param legend_pos character of character vector. It controls the position of legend.
#' @param label_size numeric, default 4. It controls the size of lebel of line plot. DO NOT make
#'   it less than 4 of larger than 6 or it will look ugly.
#'
#' @return an object of class \code{ggplot}.
#' @export
#'
#' @examples
#' df <- data.frame(year = as.factor(2016:2020), sample_size = 10:14, qualification_rate = 81:85)
#' bar_line_ggplot(df, x_var = "year", bar_var = "sample_size")
#' @importFrom rlang !! ensym
bar_line_ggplot <- function(
  dataset, x_var, bar_var = "sample_size", scale_blank_div = 5, scale_interval = NA,
  main_axis_name, sec_axis_name, x_axis_name,
  bar_fill_palette = "grey", line_color = "black", point_color = "white",
  legend_pos = c("none", "left", "right", "bottom", "top"),
  label_size = 4
) {
  # use first column as x axis variable if `x_var` is not giving
  x_var <- ifelse(!missing(x_var), rlang::quo_name(x_var), colnames(dataset)[1])
  bar_var <- "sample_size"
  line_var <- base::setdiff(colnames(dataset), c(x_var, bar_var))[1]

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

  p <- ggplot2::ggplot(data = dataset) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = !!ensym(x_var), y = !!ensym(bar_var), fill = !!ensym(x_var)),
      width = 0.6
    ) +
    ggplot2::scale_fill_manual(values = bar_fill_palette) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~inv_scale_func(.), name = sec_axis_name)) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = !!ensym(x_var), y = scale_func(!!ensym(line_var)), group = 1),
      color = line_color, size = 1.5
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = !!ensym(x_var), y = scale_func(!!ensym(line_var))),
      color = point_color, size = 2
    ) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = !!ensym(x_var), y = scale_func(!!ensym(line_var)),
        label = paste(round(!!rlang::ensym(line_var), 2), '%')
      ),
      size = label_size,
      vjust = -0.5
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = !!ensym(x_var), y = !!ensym(bar_var),
        label = !!ensym(bar_var)
      ),
      vjust = -0.5,
      show.legend = FALSE
    ) +
    ggplot2::labs(x = x_axis_name, y = main_axis_name) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 13),
      legend.position = legend_pos[1]
    ) +
    ggplot2::geom_blank()
  return(p)
}

#' Grouped Bar Chart (with Timeline)
#'
#' @description Plot grouped bar charts using \code{\link[echarts4r]{e_bar}}. This function uses
#' \code{datazoom} toolbox with type "inside". And the type of trigger is set to "axis" with axis
#' pointer type "shadow". For data label, it uses "bold" font weight to make the data label more
#' clear.
#'
#' @param data a data frame
#' @param timeline_var character. The variable indicating timeline.
#' @param x_var character. The variable that will be the X axis in the chart.
#' @param bar_var character vector. The variables that will be used to draw grouped bar chart.
#' @param bar_name character vector. The name of these variables in \code{bar_var}.
#' @param sec_y_axis logical, default FALSE. Whether to use secondary Y axis.
#' @param main_lower_bound numeric. The lower bound of main Y axis.
#' @param main_upper_bound numeric. The upper bound of main Y axis.
#' @param emphasis_type character, default to use "series" as the emphasis type in echarts.
#'   see parameter \code{emphasis} in \code{\link[echarts4r]{e_bar}}.
#' @param show_labels logical, default FALSE. Whether to show data label in each bar.
#' @param label_fontsize numeric, default 12. The font size of data label.
#' @param toolbox_freatures character vectors, default \code{c("saveAsImage", "restore")}. It controls
#'   the toolbox functions in echarts.
#'
#' @return an object of class \code{echarts4r}.
#' @export
bar_group_echart <- function(
  data, timeline_var = NULL, x_var, bar_var, bar_name, sec_y_axis = FALSE,
  main_lower_bound, main_upper_bound,
  emphasis_type = c("series", "none", "self"),
  show_labels = FALSE, label_fontsize = 12,
  toolbox_freatures = c("saveAsImage", "restore")
) {
  # auto-detect column names
  colnames_map <- getOption("report.options")[["colnameMap"]]
  if (is.null(colnames_map)) {
    message("You donnot set the `report.options$colnameMap` in `options()` before using name auto-detection.")
    message("We will use given params or column names of dataset to auto-detect axis name in figure.")
    colnames_map <- as.list(sapply(bar_var, function(x) {x}))
  }
  if (missing(bar_name)) {
    bar_name <- unname(sapply(
      bar_var, function(x) {ifelse(!is.null(colnames_map[[x]]), colnames_map[[x]], x)}
    ))
  }

  # find the bound of secondary Y axis.
  last_index <- length(bar_var)
  main_lower_bound <- ifelse(
    !missing(main_lower_bound), main_lower_bound, get_odds_bounds(data, bar_var[1])
  )
  main_upper_bound <- ifelse(
    !missing(main_upper_bound), main_upper_bound, get_odds_bounds(data, bar_var[1], lower = FALSE)
  )

  if (!is.null(timeline_var)) e <- data |> dplyr::group_by(.data[[timeline_var]])
  else e <- data

  e <- e |>
    echarts4r::e_charts_(x_var, timeline = !is.null(timeline_var))
  e <- e |>
    echarts4r::e_bar_(bar_var[1], name = bar_name[1], emphasis = list(focus = emphasis_type[1]))
  for (i in 2:last_index) {
    e <- e |>
      echarts4r::e_bar_(
        bar_var[i],
        name = bar_name[i],
        emphasis = list(focus = emphasis_type[1]),
        y_index = ifelse(sec_y_axis, 1, 0)
      )
  }
  if (sec_y_axis) {
    e <- e |> echarts4r::e_y_axis(index = 0, min = main_lower_bound, max = main_upper_bound)
  }
  e <- e |>
    echarts4r::e_labels(show = show_labels, fontWeight = "bold", fontSize = label_fontsize) |>
    echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) |>
    echarts4r::e_toolbox_feature(feature = toolbox_freatures) |>
    echarts4r::e_datazoom(x_index = 0, type = "inside")
  return(e)
}

#' Heatmap Chart (with Timeline)
#'
#' @description Plot heat map using \code{\link[echarts4r]{e_heatmap}}. This function uses
#' \code{datazoom} toolbox with type "inside" and "continuous" visual map. For Y axis labels,
#' the width of label text is set to 60 and when it will use break to wrap when the actual
#' width is overflow. For timeline options, the timeline widget is on the top and 5 units from
#' top. The time point widgets padding is set to 0 and the play intercal is 1500 ms.
#'
#' @param data a data frame
#' @param timeline_var character. The variable indicating timeline.
#' @param x_var character. The variable that will be the X axis in the chart.
#' @param y_var character. The variable that will be the Y axis in the chart.
#' @param value_var character. The variable that will be displayed in cells in the chart.
#' @param x_axis_name character. The name of X axis. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{x_var}.
#' @param y_axis_name character. The name of Y axis. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{y_var}.
#' @param value_axis_name character. The name of value variable. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{value_var}.
#' @param width,height character, \code{width} default is "100%" and \code{height} default is
#'   "700%". Must be a valid CSS unit (like \code{'100\%'}, \code{'400px'}, \code{'auto'})
#'   or a number, which will be coerced to a string and have \code{'px'} appended.
#' @param shrink_height character, default NULL. It is the same as \code{width} and \code{height}
#'   but to used in \code{\link[echarts4r]{e_grid}} to control the height of figure grid so the
#'   long label in X axis can be seen fully.
#' @param x_label_width numeric, default 1. The width of text of x label. If the text width overflow,
#'   then will use break to wrap.
#' @param x_label_fontsize numeric, default 12. The font size of text of x label.
#' @param toolbox_freatures character vectors, default \code{c("saveAsImage", "restore")}. It controls
#'   the toolbox functions in echarts.
#'
#' @return an object of class \code{echarts4r}.
#' @export
heatmap_echart <- function(
  data, timeline_var = NULL, x_var, y_var, value_var,
  x_axis_name, y_axis_name, value_axis_name,
  width = "100%", height = "700%", shrink_height = NULL,
  x_label_width = 1, x_label_fontsize = 12,
  toolbox_freatures = c("saveAsImage", "restore")
) {
  # auto-detect column names
  colnames_map <- getOption("report.options")[["colnameMap"]]
  if (is.null(colnames_map)) {
    message("You donnot set the `report.options$colnameMap` in `options()` before using name auto-detection.")
    message("We will use given params or column names of dataset to auto-detect axis name in figure.")
    colnames_map <- list()
    colnames_map[[x_var]] = x_var
    colnames_map[[y_var]] = y_var
    colnames_map[[value_var]] = value_var
  }
  if (missing(x_axis_name)) {
    x_axis_name <- ifelse(
      !is.null(colnames_map[[x_var]]), colnames_map[[x_var]], x_var
    )
  }
  if (missing(y_axis_name)) {
    y_axis_name <- ifelse(
      !is.null(colnames_map[[y_var]]), colnames_map[[y_var]], y_var
    )
  }
  if (missing(value_axis_name)) {
    value_axis_name <- ifelse(
      !is.null(colnames_map[[value_var]]), colnames_map[[value_var]], value_var
    )
  }

  # formatter str
  formatter_str <- sprintf(
    "function(params){
      return(
        '<strong>%s: ' + params.value[2] + '</strong><br/>%s: ' + params.value[0]
        + '<br/>%s: ' + params.value[1]
      )}",
    value_axis_name, x_axis_name, y_axis_name
  )

  # scale function
  value_upper_bound <- get_odds_bounds(data, value_var, lower = FALSE)
  if (max(data[[value_var]]) < value_upper_bound / 2) value_upper_bound <- max(data[[value_var]])
  visual_scale <- function(x) {
    if (!inherits(x, "numeric")) {
      stop("x must be numeric")
    }
    scales::rescale(x, to = c(0, value_upper_bound))
  }

  # translate to numeric
  if (!inherits(data[[value_var]], "numeric")) {
    data <- data |> int_to_numeric(value_var)
  }

  if (!is.null(timeline_var)) e <- data |> dplyr::group_by(.data[[timeline_var]])
  else e <- data

  e <- e |>
    echarts4r::e_charts_(
      x_var, timeline = !is.null(timeline_var),
      width = width, height = height
    ) |>
    echarts4r::e_heatmap_(y_var, value_var) |>
    echarts4r::e_visual_map_(value_var, type = "continuous", scale = visual_scale, show = FALSE) |>
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(formatter_str)) |>
    echarts4r::e_x_axis(
      axisLabel = list(
        interval = 0,
        width = x_label_width, overflow = "break",
        fontSize = x_label_fontsize
      )
    ) |>
    echarts4r::e_y_axis(axisLabel = list(width = 60, overflow = "break")) |>
    echarts4r::e_toolbox_feature(feature = toolbox_freatures) |>
    echarts4r::e_datazoom(x_index = 0, type = "inside") |>
    echarts4r::e_datazoom(y_index = 0, type = "inside")
  if (!is.null(shrink_height)) {
    e <- e |> echarts4r::e_grid(height = shrink_height)
  }

  if (!is.null(timeline_var)) {
    e <- e |>
      echarts4r::e_timeline_opts(playInterval = 1500, padding = 0, top = 5)
  }
  return(e)
}

#' Map Chart (with Timeline)
#'
#' @description Plot map using \code{\link[echarts4r]{e_map}}. It will auto-detect the \code{value_var}
#' name using information in \code{option} when the \code{value_axis_name} is not given. The chart's
#' width is set "100%" and the height is set "800%" to make the map larger enougth to see. For visual
#' map, the type is "continuous" and the upper bound is calculated automatically. The position of
#' visual map is set to right and vertically centered.
#'
#' @param data a data frame
#' @param map_json A geo list containing map data
#' @param timeline_var character. The variable indicating timeline.
#' @param value_var character. The variable that will be displayed in cells in the chart.
#' @param value_axis_name character. The name of value variable. If missing, the function will use
#'   information in \code{option} to auto-detect the name according to \code{value_var}.
#' @param use_digits logical, default TRUE. Whether to use digits in formatter.
#' @param interval numeric, default 0. The interval of label of timeline points.
#' @param timeline_label_width numeric, default 50. The label text width of timeline points.
#' @param toolbox_features character vectors, default \code{c("dataView", "saveAsImage", "restore")}.
#'   It controls the toolbox functions in echarts.
#'
#' @return an object of class \code{echarts4r}.
#' @export
map_echart <- function(
  data, map_json, timeline_var = NULL,
  value_var = "defective_rate_percent", value_axis_name,
  use_digits = TRUE,
  interval = 0, timeline_label_width = 50,
  toolbox_features = c("dataView", "saveAsImage", "restore")
) {
  # auto-detect column names
  colnames_map <- getOption("report.options")[["colnameMap"]]
  if (is.null(colnames_map)) {
    message("You donnot set the `report.options$colnameMap` in `options()` before using name auto-detection.")
    message("We will use given params or column names of dataset to auto-detect axis name in figure.")
    colnames_map <- list()
    colnames_map[[value_var]] = value_var
  }
  if (missing(value_axis_name)) {
    value_axis_name <- ifelse(
      !is.null(colnames_map[[value_var]]), colnames_map[[value_var]], value_var
    )
  }

  # get upper bound of visual map
  rate_upper <- get_odds_bounds(data, value_var, lower = FALSE)
  visual_scale_func <- function(x){
    if (!inherits(x, "numeric")) {
      stop("x must be numeric")
    }
    scales::rescale(x, to = c(0, rate_upper))
  }

  # formatter str
  formatter_str <- sprintf(
    "function(params){
      return('<strong>' + params.name + '</strong><br/>%s: ' + params.value%s)
    }",
    value_axis_name, ifelse(use_digits, ".toFixed(2)", "")
  )

  # translate to numeric
  if (!inherits(data[[value_var]], "numeric")) {
    data <- data %>% int_to_numeric(value_var)
  }

  if (!is.null(timeline_var)) e <- data |> dplyr::group_by(.data[[timeline_var]])
  else e <- data

  e <- e |>
    echarts4r::e_charts(
      province, timeline = !is.null(timeline_var),
      width = '100%', height = '800%'
    ) |>
    echarts4r::e_map_register("CustomMap", map_json) |>
    echarts4r::e_map_(value_var, map = "CustomMap") |>
    echarts4r::e_visual_map_(
      value_var, type = "continuous",
      scale = visual_scale_func,
      right = 0, top = 'center'
    ) |>
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(formatter_str)) |>
    echarts4r::e_toolbox_feature(feature = toolbox_features)

  if (!is.null(timeline_var)) {
    e <- e |>
      echarts4r::e_timeline_opts(
        playInterval = 1500, top = 5,
        label = list(
          position = "bottom", interval = interval,
          width = timeline_label_width, overflow = "break"
        ),
        padding = 0
      )
  }
  return(e)
}
