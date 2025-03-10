% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_slopes_for_periods.R
\name{calculate_slopes_for_periods}
\alias{calculate_slopes_for_periods}
\title{calculate_slopes_for_periods}
\usage{
calculate_slopes_for_periods(raw.data.clean, metadata, time_windows)
}
\arguments{
\item{raw.data.clean, metadata, time_windows}{Define overlapping time windows}
}
\value{
final_results
}
\description{
Find the lowest point in each slope period - we can use this to work out the point at which 5\% O2 is reached
}
