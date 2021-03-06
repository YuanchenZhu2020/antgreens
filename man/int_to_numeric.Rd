% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_utils.R
\name{int_to_numeric}
\alias{int_to_numeric}
\title{Change Integer Column to Numeric in Data Frame}
\usage{
int_to_numeric(data, column_name)
}
\arguments{
\item{data}{data frame with integer column}

\item{column_name}{character. The column name of integer column.}
}
\value{
data frame with numerci column
}
\description{
Change Integer Column to Numeric in Data Frame
}
\examples{
df <- data.frame(a = as.integer(1:3))
int_to_numeric(df, 'a')
}
