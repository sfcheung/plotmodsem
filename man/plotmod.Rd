% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmod.R
\name{plotmod}
\alias{plotmod}
\title{Plot the moderation effect in a path model}
\usage{
plotmod(
  fit,
  y,
  x,
  w,
  xw,
  x_label,
  w_label,
  y_label,
  title,
  a_shift = 0,
  expansion = 0.1,
  standardized = FALSE,
  digits = 3,
  x_from_mean_in_sd = 1,
  w_from_mean_in_sd = 1,
  w_method = "sd",
  w_percentiles = c(0.16, 0.84),
  x_method = "sd",
  x_percentiles = c(0.16, 0.84),
  w_sd_to_percentiles,
  x_sd_to_percentiles,
  plot_x_vlines,
  x_vlines_unit = "sd"
)
}
\arguments{
\item{fit}{The output from \link[lavaan:lavaan]{lavaan::lavaan} or its wrapper, such as
\link[lavaan:sem]{lavaan::sem}.}

\item{y}{The name of the outcome variable as in the data set in \code{fit}. It
can be the name of the variable, with or without quotes.}

\item{x}{The name of the focal variable as in the data set in
It
can be the name of the variable, with or without quotes.\code{fit}.}

\item{w}{The name of the moderator as in the data set in \code{fit}.
It
can be the name of the variable, with or without quotes.}

\item{xw}{The name of the product term, \code{x * w}. If not supplied,
The function will try to find it in the data set.
It
can be the name of the variable, with or without quotes.}

\item{x_label}{The label for the X-axis. Default is the vlaues of \code{x}.}

\item{w_label}{The label for the legend for the lines. Default is the value of\code{w}.}

\item{y_label}{The label for the Y-axis. Default is the value of \code{y}.}

\item{title}{The title of the graph. If not supplied, will be generated from the variable
names.}

\item{a_shift}{Default is 0. Can be ignored for now.}

\item{expansion}{How much tha lower and upper limits of the axis will be adjusted.}

\item{standardized}{Logical. Plot the moderation effect in standardized metric. All three
variables, \code{x}, \code{w}, and \code{y} will be standardized. Default
is \code{FALSE}}

\item{digits}{Number of decimal digits to print. Default is 3.}

\item{x_from_mean_in_sd}{How many SD from mean is used to define "low" and
"high" for the focal variable. Default is 1.}

\item{w_from_mean_in_sd}{How many SD from mean is used to define "low" and
"high" for the moderator. Default is 1.}

\item{w_method}{How to define "high" and "low" for the moderator levels.
Default is in terms of the
standard deviation of the moderator, "sd". If equal to
"percentile", then percentiles of the moderator in the
dataset is used.}

\item{w_percentiles}{If \code{w_method} is "percentile", then this argument
specifies the two percentiles to be used, divided by 100.
It must be a
vector of two numbers. The default is \code{c(.16, .84)},
the 16th and 84th percentiles,
which corresponds approximately
to one SD below and above mean for a
normal distributoin, respectively.}

\item{x_method}{How to define "high" and "low" for the focal variable levels.
Default is in terms of the
standard deviation of the focal variable, "sd". If equal to
"percentile", then percentiles of the focal variable in the
dataset is used.}

\item{x_percentiles}{If \code{x_method} is "percentile", then this argument
specifies the two percentiles to be used, divided by 100.
It must be a
vector of two numbers. The default is \code{c(.16, .84)},
the 16th and 84th percentiles,
which corresponds approximately
to one SD below and above mean for a
normal distributoin, respectively.}

\item{w_sd_to_percentiles}{If \code{w_method} is "percentile" and this argument is
set to a number, this number will be used to
to determine the percentiles to be used. The
lower percentile is the percentile in a normal
distribution
that is \code{w_sd_to_percentiles} SD below the mean.
The upper percentile is the percentil in a normal
distribution that is \code{w_sd_to_percentiles} SD
above the mean. Therefore, if
\code{w_sd_to_percentiles} is set to 1, then the lower
and upper percentiles are 16th and 84th,
respectively.}

\item{x_sd_to_percentiles}{If \code{x_method} is "percentile" and this argument is
set to a number, this number will be used to
to determine the percentiles to be used. The
lower percentile is the percentile in a normal
distribution
that is \code{x_sd_to_percentiles} SD below the mean.
The upper percentile is the percentil in a normal
distribution that is \code{x_sd_to_percentiles} SD
above the mean. Therefore, if
\code{x_sd_to_percentiles} is set to 1, then the lower
and upper percentiles are 16th and 84th,
respectively.}

\item{plot_x_vlines}{If supplied, vertical lines to indicate the levels of
the focal variable will be plotted. This should be a
vector of numbers, indicating the levels to be plotted.
How these numbers are interpreted depends on
\code{x_vlines_unit}.}

\item{x_vlines_unit}{If equal to "sd", then the values of \code{plot_x_vlines}
will be interpreted as the deviation from the mean.
For example, 1 is 1 SD above mean, and -1 is 1 SD
below mean. If equal to "percentile", then the numbers,
mulipltied by 100, are the precentiles. For example,
.25 is the 25th percentile, and .75 is the 75th
percentile.}
}
\value{
A \link{ggplot2} graph.
}
\description{
Plot the moderation effect in a path model
}
\details{
This function extracts the information stored
in the \code{lavaan} fit object to plot a two-line
graph, one for the relation between the focal variale (\code{x}) and
the outcome variable (\code{y}) when the moderator (\code{w}) is one stanard
deviation below mean, and one when the moderator is one standard
deviation above mean.
}
\examples{
\dontrun{
# To be prepared
}
}
