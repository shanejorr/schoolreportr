#' Get state assessment data for a district
#'
#' Imports district data from Ed Facts using the Urban Institute's API. The data contains state assessment information.
#'
#' @param lea_number The school's LEAID number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grade Grades to import. Defaults to all grades (99)
#'
#' @returns A list with each element containing a data frame with school data. The list is named,
#'      which identifies the data contained in the list element.
#'
#' @keywords internal
get_state_assessments_by_district <- function(fips_code, years, grade = 99) {

  # get each individual race and all races (99)
  race_to_use <- c(seq(1, 9), 99)

  educationdata::get_education_data(level = "school-districts",
                     source = "edfacts",
                     topic = "assessments",
                     filters = list(
                       fips = fips_code,
                       #leaid_num = lea_number,
                       year = years,
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
#' @keywords internal
aggregate_assessment <- function(.data, grouping_vars) {

  .data %>%
    dplyr::group_by_at(grouping_vars) %>%
    dplyr::summarize(dplyr::across(dplyr::contains('_num_'), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr::mutate(
      read_pct_pass = (read_test_num_pass / read_test_num_valid) * 100,
      math_pct_pass = (math_test_num_pass / math_test_num_valid) * 100
    )

}

#' Create table with state-level assessment scores for the whol state and a single district
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
assessment_scores_by_race <- function(state_assessment_data, state_abb, district_leaid, years, grade = 99) {

  # calculate district scores
  district <- state_assessment_data %>%
    dplyr::filter(leaid_num == !!district_leaid) %>%
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

#' Rename and reorder race categories
#'
#' Rename Black to 'Black / African-American' and Hispanic to 'Hispanic / Latinx'. Also reorder so that
#' 'Black / African-American' and 'Hispanic / Latinx' appear first and second.
#'
#' @param race_col A vector containing the race data
#'
#' @returns A factor vector with race names changes and order changes.
rename_reorder_race_education <- function(race_col) {

  race_order_education <- c('Black' = 'Black / African-American', 'Hispanic' = 'Hispanic / Latinx', 'White' = 'White')

  race <- dplyr::recode(race_col, !!!race_order_education)

  race <- forcats::fct_relevel(race, race_order_education)

  race <- forcats::fct_relevel(race, 'Two or more races', after = Inf)

  return(race)

}
