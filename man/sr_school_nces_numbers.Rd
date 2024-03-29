% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-educationdata-district.R
\name{sr_school_nces_numbers}
\alias{sr_school_nces_numbers}
\title{Get a data frame with all schools from a state along with their key information}
\usage{
sr_school_nces_numbers(state_abb)
}
\arguments{
\item{state_abb}{The two letter state abbbreviation on the state that
you want to show all schools for.}
}
\value{
A data frame showing all schools in a state, along with key identifying information.
}
\description{
Returns a data frame of all schools in a state, along with their nces number, leaid,
district, and address. This information is useful to find a school's nces number for
use in \code{\link[=sr_school_report]{sr_school_report()}}.
}
