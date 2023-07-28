#' Create flexdashboard for a given school or list of schools
#'
#' Use the automated template to create a report for a school that provides background
#' information about the school or list of schools. If more than one school is specified,
#' all the data from the data from the schools is aggregated. But, all school must
#' be in the same district.
#'
#' @param output_filename The filename and path to output the HTML file of the school report.
#'      The path is relative to your R root folder in your current session. It should end in '.html'.
#' @param nces_num The NCES number(s) of the school(s) that we want to generate
#'      reports for. The numbers should be as strings. Use a vetor of numbers if
#'      to aggregate the data of multiple schools within a single district.
#'      Use [`sr_school_nces_numbers()`] to get a data frame of NCES numbers for each school in a state.
#' @param years The years, as a vector of integers, we want to include in the data.
#' @param state_abb The two letter state abbreviation where the school(s) are located.
#' @param grades A vector of integers representing the grades we want included in the report.
#'      If the school does not contain the grades requested, these grades will be ignored.
#'      Use [`sr_school_nces_numbers()`] to see which grades each school in a state serves.
#'      Use -1 for Pre-K and 0 for K.
#'
#' @export
sr_school_report <- function(output_filename, nces_num, years, state_abb, grades) {

  # ensure the state_abb character is in fact a state
  if (!state_abb %in% datasets::state.abb) {
    stop("`state_abb` is not a state. Make sure `state_abb` is the state's two letter abbreviation.", call. = FALSE)
  }

  # ensure grades are vectors of integers
  if(!all(grades == floor(grades))) {
    stop("`grades` should be integers")
  }

  # Check if the string ends with '.html'
  if (!stringr::str_ends(output_filename, ".html")) {
    stop("`output_filename` should end with '.html'")
  }

  # ensure directory exists
  # Extract directory from the full path
  dir_path <- fs::path_dir(output_filename)

  if (!fs::dir_exists(dir_path)) {
    stop(glue::glue("The directory '{dir_path}' does not exits."))
  }

  org_level <- 'schools'

  rmarkdown::render(
    input = system.file("rmd", "template-school.Rmd", package = "schoolreportr"),
    # input = paste0(system.file(package = "schoolreportr"), "/rmd/template-school.Rmd"),
    output_format = 'flexdashboard::flex_dashboard',
    output_file = here::here('report.html'),
    # quiet = TRUE,
    params = list(
      org_level = org_level,
      nces_num = nces_num,
      years = years,
      state_abb = state_abb,
      grades = grades
    )
  )

  # Copy the Rmarkdown file to the location specified by the user
  fs::file_move(here::here('report.html'), output_filename)

  return(message(glue::glue("School report downloaded to {output_filename}")))

}

#' Download the template Rmarkdown file
#'
#' This function allows users to download and save the template Rmarkdown file
#' that is used to build the default report. Users can then adjust the file
#' to suit their needs.
#'
#' @param destination_path The path where the Rmarkdown file should be saved.
#'
#' @export
sr_download_template_rmd <- function(destination_path) {

  # Check if the string ends with '.html'
  if (!stringr::str_detect(destination_path, "[.][R|r]md$")) {
    stop("`destination_path` must end in `.rmd`", call. = FALSE)
  }

  # ensure directory exists
  # Extract directory from the full path
  dir_path <- fs::path_dir(destination_path)

  if (!fs::dir_exists(dir_path)) {
    stop(glue::glue("The directory '{dir_path}' does not exits."))
  }

  # Get the path to the example Rmarkdown file within the installed package
  rmd_path <- system.file("rmd", "template-school.Rmd", package = "schoolreportr")

  # Copy the Rmarkdown file to the location specified by the user
  fs::file_copy(rmd_path, destination_path)

  return(message(glue::glue("----- School report template downloaded to {destination_path} -----")))

}

