% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_utils.R
\name{bar_group_echart}
\alias{bar_group_echart}
\title{Grouped Bar Chart (with Timeline)}
\usage{
bar_group_echart(
  data,
  timeline_var = NULL,
  x_var,
  bar_var,
  bar_name,
  sec_y_axis = FALSE,
  main_lower_bound,
  main_upper_bound,
  emphasis_type = c("series", "none", "self"),
  show_labels = FALSE,
  label_fontsize = 12,
  toolbox_freatures = c("saveAsImage", "restore")
)
}
\arguments{
\item{data}{a data frame}

\item{timeline_var}{character. The variable indicating timeline.}

\item{x_var}{character. The variable that will be the X axis in the chart.}

\item{bar_var}{character vector. The variables that will be used to draw grouped bar chart.}

\item{bar_name}{character vector. The name of these variables in \code{bar_var}.}

\item{sec_y_axis}{logical, default FALSE. Whether to use secondary Y axis.}

\item{main_lower_bound}{numeric. The lower bound of main Y axis.}

\item{main_upper_bound}{numeric. The upper bound of main Y axis.}

\item{emphasis_type}{character, default to use "series" as the emphasis type in echarts.
see parameter \code{emphasis} in \code{\link[echarts4r]{e_bar}}.}

\item{show_labels}{logical, default FALSE. Whether to show data label in each bar.}

\item{label_fontsize}{numeric, default 12. The font size of data label.}

\item{toolbox_freatures}{character vectors, default \code{c("saveAsImage", "restore")}. It controls
the toolbox functions in echarts.}
}
\value{
an object of class \code{echarts4r}.
}
\description{
Plot grouped bar charts using \code{\link[echarts4r]{e_bar}}. This function uses
\code{datazoom} toolbox with type "inside". And the type of trigger is set to "axis" with axis
pointer type "shadow". For data label, it uses "bold" font weight to make the data label more
clear.
}
