% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/storage_object_functions.R
\name{user_peak_params_input}
\alias{user_peak_params_input}
\title{user_peak_params_input}
\usage{
user_peak_params_input(
  storage_object,
  smooth_type,
  smooth_window,
  slope_thresh,
  amp_thresh,
  density_window_rt_distance,
  density_count_in_window_threshold
)
}
\arguments{
\item{storage_object}{A metScribeR object}

\item{smooth_type}{passed to chromatographR::find_peaks()}

\item{smooth_window}{passed to chromatographR::find_peaks()}

\item{slope_thresh}{passed to chromatographR::find_peaks()}

\item{amp_thresh}{passed to chromatographR::find_peaks()}

\item{density_window_rt_distance}{the distance, measured in RT, within which
the number of other points is counted for subsequent filtering.}

\item{density_count_in_window_threshold}{any point with fewer points in the given RT window
than this threshold will be removed.}
}
\value{
an updated storage_object
}
\description{
Collect information for feature finding in eic data.
}
\details{
This function collects parameters to pass to the feature finding function.
Feature finding uses chromatographR::find_peaks(), so see that package's documentation for details.
The density window and density threshold parameters are designed to eliminate hanging points on peaks
that are noise far in RT from any other point. The algorithm will count how many other observations belonging
to a given peak are within the RT window and eliminate any points with fewer found than the threshold parameter.

These parameters are associated with the active library id.
}
