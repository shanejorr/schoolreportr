% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-census.R
\name{sr_calculate_percentages}
\alias{sr_calculate_percentages}
\title{Calculate percentages in district given counts of each tract in a district}
\usage{
sr_calculate_percentages(.data)
}
\arguments{
\item{.data}{Data frame containing census demographic data. Data.frame should
be created with \code{sr_demographic_data}.}
}
\description{
Calculates overall percentage demographics in a district and state given counts
in all census tracts in a district and overall state counts. For example, calculates
racial breakdown by percentage in the district by creating aggregate
percentage for all census tracts in the district. Input data is created with \code{\link[=sr_demographic_data]{sr_demographic_data()}}.
}
\details{
This is a helper function used to create the Rmarkdown report.
}
