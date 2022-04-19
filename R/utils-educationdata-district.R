#' Get district directory (staff, address, etc.)
#'
#' @param leaid The school's LEAID number, as a string.
#' @param year The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#'
#' @returns A data set with the district directory.
#'
#' @export
get_district_directory <- function(leaid, year) {

  ccd_years <- intersect(data_years_available('ccd'), year)

  educationdata::get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(year = ccd_years, leaid = leaid)
  )

}

#' Percentage of 5-17 year olds in poverty in district
#'
#' @param leaid The school's LEAID number, as a string.
#' @param year The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#'
#' @returns A data set with the percentage of 5-17 year old sin poverty by year.
#'
#' @export
get_district_in_poverty <- function(leaid, year) {

  saipe_years <- intersect(data_years_available('saipe'), year)

  educationdata::get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(year = saipe_years, leaid = leaid)
  )

}

#' School directory for schools in district and grade
#'
#' @param leaid The school's LEAID number, as a string.
#' @param year The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grades Grades to import. Defaults to all grades (99)
#'
#' @returns A data set with directory information for all schools in the district and within grades.
#'
#' @export
get_schools_in_district <- function(leaid, year, grades = 99) {

  all_grades <- seq(min(grades), max(grades), 1)

  recode_grades <- c('-1' = 'Pre-K', '0' = 'K')

  ccd_years <- intersect(data_years_available('ccd'), year)

  educationdata::get_education_data(
    level = "schools",
    source = "ccd",
    topic = "directory",
    filters = list(leaid = leaid, year = ccd_years, school_status = c(1,3,4,5))
  ) %>%
    # make TRUE if school is in grade range, FALSE otherwise
    dplyr::mutate(in_grade = dplyr::case_when(
      lowest_grade_offered %in% all_grades ~ TRUE,
      highest_grade_offered %in% all_grades ~ TRUE,
      dplyr::between(lowest_grade_offered, min(grades), max(grades)) ~ TRUE,
      dplyr::between(highest_grade_offered, min(grades), max(grades)) ~ TRUE,
      (lowest_grade_offered <= min(grades)) & (highest_grade_offered >= min(grades)) ~ TRUE,
      (lowest_grade_offered <= max(grades)) & (highest_grade_offered >= max(grades)) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(
      across(contains('grade_offered'), ~as.character(.x)),
      across(contains('grade_offered'), ~dplyr::recode(.x, !!!recode_grades, .default = .x))
    ) %>%
    dplyr::mutate(grade_range = glue::glue("{lowest_grade_offered} to {highest_grade_offered}"))

}

#' Get district enrollment information by year, grade, race
#'
#' @param leaid The school's LEAID number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grades Grades to import. Defaults to all grades (99)
#'
#' @returns A data set with the district enrollment by year, grade, and race.
#'
#' @export
get_district_enrollment <- function(leaid, years, grades = 99) {

  ccd_years <- intersect(data_years_available('ccd'), years)

  educationdata::get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters = list(year = ccd_years, leaid = leaid, grade = grades),
    subtopic = list("race"),
    add_labels = TRUE
  ) %>%
    dplyr::group_by(leaid, year, race) %>%
    dplyr::summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = 'drop')

}

#' Get state assessment data for a district
#'
#' Imports district data from Ed Facts using the Urban Institute's API. The data contains state assessment information.
#'
#' @param fips_code State FIPS code, as integer.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grade Grades to import. Defaults to all grades (99)
#'
#' @returns A list with each element containing a data frame with school data. The list is named,
#'      which identifies the data contained in the list element.
#'
#' @export
get_state_assessments_by_district <- function(fips_code, years, grade = 99) {

  # get each individual race and all races (99)
  race_to_use <- c(seq(1, 9), 99)

  edfacts <- intersect(data_years_available('edfacts'), years)

  educationdata::get_education_data(level = "school-districts",
                     source = "edfacts",
                     topic = "assessments",
                     filters = list(
                       fips = fips_code,
                       year = edfacts,
                       grade_edfacts = grade
                      ),
                     subtopic = list("race"),
                     add_labels = TRUE
                    ) %>%
    clean_numeric_assessments()
}

#' Aggregate state assessment data
#'
#' Summarize aggregate percentages of state assessment data by using the total number of takers
#' and total numebr of passers
#'
#' @export
aggregate_assessment <- function(.data, grouping_vars) {

  .data %>%
    dplyr::group_by_at(grouping_vars) %>%
    dplyr::summarize(dplyr::across(dplyr::contains('_num_'), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr::mutate(
      read_pct_pass = (read_test_num_pass / read_test_num_valid) * 100,
      math_pct_pass = (math_test_num_pass / math_test_num_valid) * 100
    )

}

#' Create table with state-level assessment scores for the whole state and a single district
#'
#' @param state_abb Two letter state abbreviation.
#' @param district_leaid The district's LEAID number, as an integer.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grade Grades to import. Defaults to all grades (99)
#'
#' @returns A single data set that contains assessment scores broken down by race. It includes rows
#'      for the whole state and a single district that is specified.
#'
#' @export
assessment_scores_by_race <- function(state_assessment_data, state_abb, district_leaid) {

  # calculate district scores
  district <- state_assessment_data %>%
    dplyr::filter(leaid_num == !!as.numeric(district_leaid)) %>%
    aggregate_assessment(c('lea_name', 'year', 'race')) %>%
    dplyr::mutate(geography = lea_name) %>%
    dplyr::select(-lea_name)

  # calcualte state scores
  state_average_assessment <- state_assessment_data %>%
    aggregate_assessment(c('year', 'race')) %>%
    dplyr::mutate(geography = glue::glue("{state_abb} Total"))

  district %>%
    dplyr::bind_rows(state_average_assessment) %>%
    # rename racial categories and relevel
    dplyr::mutate(race = rename_reorder_race_education(race))

}

#' Clean enrollment by race data
#'
#' Calculate percentage race breakdowns, change race names, reorder race
#'
#' @param district_race_enrollment Aataframe with enrollment data by race, created with get_district_enrollment()
#'
#' @returns A data frame with percentage race breakdowns by year.
#'
#' @export
clean_enrollment_by_race <- function(district_race_enrollment) {

  district_race_enrollment %>%
    dplyr::group_by(leaid, year) %>%
    dplyr::mutate(
      total_students = max(enrollment),
      percent_race = enrollment / total_students,
      percent_race_clean = scales::percent(percent_race, accuracy = 1),
      percent_race = percent_race * 100,
      race = rename_reorder_race_education(race)
    ) %>%
    dplyr::filter(race != 'Total') %>%
    dplyr::ungroup()
}

#' Rename and reorder race categories
#'
#' Rename Black to 'Black / African-American' and Hispanic to 'Hispanic / Latinx'. Also reorder so that
#' 'Black / African-American' and 'Hispanic / Latinx' appear first and second.
#'
#' @param race_col A vector containing the race data
#'
#' @returns A factor vector with race names changes and order changes.
#'
#' @export
rename_reorder_race_education <- function(race_col) {

  race_order_education <- c('Black' = 'Black / African-American', 'Hispanic' = 'Hispanic / Latinx', 'White' = 'White')

  race <- dplyr::recode(race_col, !!!race_order_education)

  race <- forcats::fct_relevel(race, race_order_education)

  race <- forcats::fct_relevel(race, 'Two or more races', after = Inf)

  return(race)

}

#' Create dataset with all district names and their LEAID numbers
#'
#' Used so we can locate leaid numbers from district names
#'
#' @export
get_state_leaid_numbers <- function(state_abb, year) {

  educationdata::get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(fips = state_fips_code(state_abb), year = year)
  ) %>%
    dplyr::select(leaid, lea_name, city_location, number_of_schools, enrollment)
}
