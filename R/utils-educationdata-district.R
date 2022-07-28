#' Vector of state FIPS codes
#'
#' @export
fips_states <- function() {
  c(2, 4:6, 8:13, 15:42, 44:51, 53:56)
}

#' Years data is available for each Urban Inst. API data source
#'
#' @param data_source The data source for the API. One of 'ccd', 'crdc', 'edfacts', or 'saipe
#'
#' @return Vector with the years (as integers) in which the data source is available.
#'
#' @export
data_years_available <- function(data_source) {

  sources <- c('ccd', 'crdc', 'edfacts', 'saipe')

  sources_string <- paste0(sources, collapse = "', '")

  if (!data_source %in% sources) stop(paste0("Data sources must be one of either: '", sources_string, "'"))

  years_available <- list(
    ccd = 1986:2020,
    crdc = c(2011, 2013, 2015, 2017),
    edfacts = 2009:2018,
    saipe= 1999:2018
  )

  years_available[[data_source]]

}

#' Check 'level' parameter in functions
#'
#' For the educationdata API, the level must be either 'schools' or 'school-districts' for many calls.
#' This function checks to ensure that the level parameter is one of these two values and throws an error if not.
#'
#' @keywords internal
check_level <- function(level) {

  levels <- c('schools', 'school-districts')

  if (!level %in% levels) stop(paste0("`level` must be either: '", paste0(levels, collapse = "' or '")))

}

#' Convert year as a single number to a school year
#'
#' For example, convert 2014 to 2014-2015. For all education data API years, the year listed is the
#' fall semester. Function also orders the school year column by the year column, so plots with the
#' school year column on the x axis are in the proper order
#'
#'
#' @returns A vector of school years.
#'
#' @keywords internal
convert_to_sy <- function(years_col) {

  school_years_col <- glue::glue("{years_col}-{years_col+1}")

  return(forcats::fct_reorder(school_years_col, years_col))

}

#' Get directory (staff, address, etc.)
#'
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param nces_num The school's LEAID number or NCES number, as a string.
#' @param year The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#'
#' @returns A data set with the district directory.
#'
#' @export
get_ccd_directory <- function(org_level, nces_num, year) {

  check_level(org_level)

  map_df(nces_num, function(x) {

    if (org_level == 'schools') {
      filters <- list(ncessch = x, year = year)
    }
    else if (org_level == 'school-districts') {
      filters <- list(leaid = x, year = year)
    }

    educationdata::get_education_data(
      level = org_level,
      source = "ccd",
      topic = "directory",
      filters = filters
    )
  }) %>%
    dplyr::mutate(perc_free_lunch = free_lunch / enrollment)

}

#' Get enrollment (staff, address, etc.)
#'
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param nces_num The school's LEAID number or NCES number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grades The grades to import with API. Enrollment numbers will combine all grades into one number.
#'
#' @returns A data set with the enrollment data.
#'
#' @export
get_ccd_enrollment <- function(org_level, nces_num, years, grades) {

  check_level(org_level)

  enrollment <- map_df(nces_num, function(x) {

    if (org_level == 'schools') {
      filters <- list(ncessch = x, year = years, grade = grades)
    }
    else if (org_level == 'school-districts') {
      filters <- list(leaid = x, year = years, grade = grades)
    }

    educationdata::get_education_data(
      level = org_level,
      source = "ccd",
      topic = "enrollment",
      filters = filters,
      subtopic = list("race"),
      add_labels = TRUE
    )

  }) %>%
    dplyr::group_by(year, race) %>%
    dplyr::summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = 'drop')

  enrollment$school_year_both <- convert_to_sy(enrollment$year)

  enrollment %>%
    arrange(school_year_both, race)

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
    identify_school_grades(all_grades) %>%
    dplyr::mutate(
      across(contains('grade_offered'), ~as.character(.x)),
      across(contains('grade_offered'), ~dplyr::recode(.x, !!!recode_grades, .default = .x))
    ) %>%
    dplyr::mutate(grade_range = glue::glue("{lowest_grade_offered} to {highest_grade_offered}"))

}

#' Identify if school contains a grade
#'
#' NCES data reports grades for schools, but reports it as the lowest and highest grades.
#' This function takes a vector of grades as input and then returns a boolean of whether the school
#' contains any of the grades based on the lowest and highest grades.
#'
#' @export
identify_school_grades <- function(.data, vector_of_grades) {

  min_grade <- min(vector_of_grades)
  max_grade <- max(vector_of_grades)

  .data %>%
    # make TRUE if school is in grade range, FALSE otherwise
    dplyr::mutate(in_grade = dplyr::case_when(
      lowest_grade_offered %in% vector_of_grades ~ TRUE,
      highest_grade_offered %in% vector_of_grades ~ TRUE,
      dplyr::between(lowest_grade_offered, min_grade, max_grade) ~ TRUE,
      dplyr::between(highest_grade_offered, min_grade, max_grade) ~ TRUE,
      (lowest_grade_offered <= min_grade) & (highest_grade_offered >= min_grade) ~ TRUE,
      (lowest_grade_offered <= max_grade) & (highest_grade_offered >= max_grade) ~ TRUE,
      TRUE ~ FALSE
    ))

}

#' Get state assessment data for a district
#'
#' Imports district data from Ed Facts using the Urban Institute's API. The data contains state assessment information.
#'
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param fips_code State FIPS code, as integer.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grade Grades to import. Defaults to all grades (99)
#'
#' @returns A list with each element containing a data frame with school data. The list is named,
#'      which identifies the data contained in the list element.
#'
#' @export
get_edfacts_state_assessments <- function(org_level, fips_code, years, grade = 99) {

  # get each individual race and all races (99)
  race_to_use <- c(seq(1, 9), 99)

  edfacts <- intersect(data_years_available('edfacts'), years)

  educationdata::get_education_data(level = org_level,
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


#' Clean up numeric values of state assessments and calculate number who passed
#'
#' @export
clean_numeric_assessments <- function(.data) {

  .data %>%
    # clean up numeric columns
    dplyr::mutate(dplyr::across(dplyr::contains("_test_"), ~as.numeric(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("_test_"), ~ifelse(.x < 0, NA_real_, .x))) %>%
    # convert integers to percentages
    dplyr::mutate(dplyr::across(dplyr::contains("_pct_prof_"), ~(.x / 100))) %>%
    # calculate number of students who passed
    dplyr::mutate(
      read_test_num_pass = round(read_test_num_valid * read_test_pct_prof_midpt, 0),
      math_test_num_pass = round(math_test_num_valid * math_test_pct_prof_midpt, 0)
    )

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
assessment_scores_by_race <- function(state_assessment_data, org_level, nces_num) {

  check_level(org_level)

  # we will use a different column to identify the school or district
  # identify filtering column
  filter_col <- if (org_level == 'schools') 'ncessch' else 'leaid'
  geography_id <- if (org_level == 'schools') 'school_name' else 'lea_name'

  # calculate district scores
  district <- state_assessment_data %>%
    dplyr::filter(.data[[filter_col]] %in% !!nces_num) %>%
    aggregate_assessment(c('year', 'race')) %>%
    dplyr::mutate(geography = 'schools / districts')

  # calcualte state scores
  state_average_assessment <- state_assessment_data %>%
    aggregate_assessment(c('year', 'race')) %>%
    dplyr::mutate(geography = 'state total')

  district %>%
    dplyr::bind_rows(state_average_assessment) %>%
    # rename racial categories and relevel
    dplyr::mutate(race = rename_reorder_race_education(race)) %>%
    dplyr::mutate(school_year_both = convert_to_sy(year)) %>%
    arrange(school_year_both, race)

}

#' Clean enrollment by race data
#'
#' Calculate percentage race breakdowns, change race names, reorder race
#'
#' @param race_enrollment Data frame with enrollment data by race (school or district), created with get_ccd_enrollment()
#'
#' @returns A data frame with percentage race breakdowns by year.
#'
#' @export
clean_enrollment_by_race <- function(race_enrollment) {

  race_enrollment %>%
    dplyr::group_by(year) %>%
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

#' Create dataset with all district / school names and their LEAID / NCES numbers
#'
#' Used so we can locate leaid numbers from district names
#'
#' @export
get_state_school_numbers <- function(org_level, state_abb, year) {

  org_details <- educationdata::get_education_data(
    level = org_level,
    source = "ccd",
    topic = "directory",
    filters = list(fips = state_fips_code(state_abb), year = year)
  )

  if (org_level == 'school-districts') {
    org_details <- org_details %>%
        dplyr::select(leaid, lea_name, city_location, number_of_schools, enrollment)
  } else if (org_level == 'schools') {
    org_details <- org_details %>%
      dplyr::select(ncessch, leaid, school_name, city_location, lowest_grade_offered, highest_grade_offered, enrollment)
  } else {
    stop()
  }

  return(org_details)

}
