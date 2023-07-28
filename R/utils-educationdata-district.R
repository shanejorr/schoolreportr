#' Helper function to add leading zeroes to school and district numbers
#'
#' School and district numbers started receiveing an additional leading zero
#' in 2020. This function places all numbers in the same format by ensuring all
#' numbers are of the same length. Leading zeroes are added when numbers are not of the proper length
#'
#' @param .data Data frame containing columns titled `ncessch` and `leaid`
#'      that we want to convert to character
#'
#' @keywords internal
sr_add_leading_zeroes <- function(.data) {

  district_length <- 7
  school_length <- 12

  .data |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of('ncessch'), ~stringr::str_pad(.x, school_length, "0")),
      dplyr::across(dplyr::any_of('leaid'), ~stringr::str_pad(.x, district_length, "0"))
    )

}

#' Years data is available for each Urban Inst. API data source
#'
#' Find the years each data source is available for data from the Urban Institute's
#' Education data explorer API.
#'
#' @param data_source The data source for the API. One of 'ccd', 'crdc', 'edfacts', or 'saipe
#'
#' @return Vector with the years (as integers) in which the data source is available.
#'
#' @export
sr_years_data_available <- function(data_source) {

  sources <- c('ccd', 'crdc', 'edfacts', 'saipe', 'census')

  sources_string <- paste0(sources, collapse = "', '")

  if (!data_source %in% sources) stop(paste0("Data sources must be one of either: '", sources_string, "'"))

  years_available <- list(
    ccd = 1986:2021,
    crdc = c(2011, 2013, 2015, 2017),
    edfacts = c(2009:2018, 2020),
    saipe= 1999:2021,
    census = c(2000:2019, 2021) # no 2020 1 year due to COVID
  )

  years_available[[data_source]]

}

#' Check 'level' parameter in functions
#'
#' For the educationdata API, the level must be either 'schools' or 'school-districts' for many calls.
#' This function checks to ensure that the level parameter is one of these two values and throws an error if not.
#'
#' @keywords internal
sr_check_level <- function(level) {

  levels <- c('schools', 'school-districts')

  if (!level %in% levels) stop(paste0("`level` must be either: '", paste0(levels, collapse = "' or '")))

}

#' Convert year as a single number to a school year
#'
#' For example, convert 2014 to 2014-2015. For all education data API years, the year listed is the
#' fall semester. Function also orders the school year column by the year column, so plots with the
#' school year column on the x axis are in the proper order
#'
#' @param years_col Vector containing years. This will typically be the column in the
#'      data set containing years.
#'
#' @returns A vector of school years, as a factor.
#'
#' @keywords internal
sr_convert_to_sy <- function(years_col) {

  school_years_col <- glue::glue("{years_col}-{years_col+1}")

  return(forcats::fct_reorder(school_years_col, years_col))

}

#' Get a data frame with all schools from a state along with their key information
#'
#' Returns a data frame of all schools in a state, along with their nces number, leaid,
#' district, and address. This information is useful to find a school's nces number for
#' use in [`sr_school_report()`].
#'
#' @param state_abb The two letter state abbbreviation on the state that
#'      you want to show all schools for.
#'
#' @returns A data frame showing all schools in a state, along with key identifying information.
#'
#' @export
sr_school_nces_numbers <- function(state_abb) {

  max_year <- max(schoolreportr::sr_years_data_available('ccd'))

  state_abb <- stringr::str_to_upper(state_abb)

  a <- educationdata::get_education_data(
    level = 'schools',
    source = "ccd",
    topic = "directory",
    filters = list(year = max_year, state_location = state_abb)
  )  |>
    dplyr::select(dplyr::all_of(c('ncessch', 'leaid', 'school_name', 'lea_name', 'city_location', 'state_location')), dplyr::ends_with('_offered')) |>
    dplyr::mutate(dplyr::across(c(dplyr::ends_with('name'), 'city_location'), ~stringr::str_to_title(.x)))

}

#' Get CCD directory of a school or district.
#'
#' Returns the directory of a school or district for a single year, from the Common
#' Core of Data data set. In the Education Data API, this returns the "directory" topic from the "ccd" source.
#' Can return the information from multiple schools or districts by supplying a vector
#' of numbers to `nces_num`.
#'
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param nces_num The school's LEAID number or NCES number, as a string. Can use multiple
#'     schools or districts by combining all numbers in a vector.
#' @param year The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#'
#' @returns A data set with the district directory.
#'
#' @export
sr_ccd_directory <- function(org_level, nces_num, year) {

  sr_check_level(org_level)

  purrr::map(nces_num, function(x) {

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
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(perc_free_lunch = .data$free_lunch / .data$enrollment)

}

#' Get enrollment data of the school or district.
#'
#' Gets enrollment data of the school or district, by race. Data comes from the CCD
#' and is imported via the Education Data Portal API.
#'
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param nces_num The district's LEAID number or school's NCES number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grades The grades to import. Enrollment numbers will combine all grades into one number.
#'
#' @returns A data frame with the enrollment data.
#'
#' @export
sr_ccd_enrollment <- function(org_level, nces_num, years, grades) {

  sr_check_level(org_level)

  enrollment <- purrr::map(nces_num, function(x) {

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

  }) |>
    purrr::list_rbind() |>
    dplyr::group_by_at(c('year', 'race'))  |>
    dplyr::summarize(enrollment = sum(.data$enrollment, na.rm = TRUE), .groups = 'drop')

  enrollment$school_year_both <- sr_convert_to_sy(enrollment$year)

  enrollment |>
    dplyr::arrange(.data$school_year_both, .data$race)

}

#' Percentage of 5-17 year olds in poverty in district
#'
#' @param leaid The district's LEAID number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year. Use a vector of years to return data from
#'      multiple years.
#'
#' @returns A data set with the percentage of 5-17 year olds in poverty by year.
#'
#' @export
sr_district_in_poverty <- function(leaid, years) {

  saipe_years <- intersect(sr_years_data_available('saipe'), years)

  educationdata::get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(year = saipe_years, leaid = leaid)
  )

}

#' Identify if school contains a grade
#'
#' NCES data reports grades for schools, but reports it as the lowest and highest grades.
#' This function takes a vector of grades as input and then returns a boolean of whether the school
#' contains any of the grades based on the lowest and highest grades.
#'
#' @param .data Dataset of CCD data, created with [`sr_ccd_directory()`].
#' @param vector_of_grades Vector of integers representing the grades of which
#'       we want to see if the school has any of the grades in the vector.
#'
#' @export
sr_identify_school_grades <- function(.data, vector_of_grades) {

  min_grade <- min(vector_of_grades)
  max_grade <- max(vector_of_grades)

  .data |>
    # make TRUE if school is in grade range, FALSE otherwise
    dplyr::mutate(in_grade = dplyr::case_when(
      .data$lowest_grade_offered %in% vector_of_grades ~ TRUE,
      .data$highest_grade_offered %in% vector_of_grades ~ TRUE,
      dplyr::between(.data$lowest_grade_offered, min_grade, max_grade) ~ TRUE,
      dplyr::between(.data$highest_grade_offered, min_grade, max_grade) ~ TRUE,
      (.data$lowest_grade_offered <= min_grade) & (.data$highest_grade_offered >= min_grade) ~ TRUE,
      (.data$lowest_grade_offered <= max_grade) & (.data$highest_grade_offered >= max_grade) ~ TRUE,
      .default = FALSE
    )) |>
    dplyr::pull(.data$in_grade)

}

#' Get state assessment data for a district or school from EdFacts
#'
#' Imports state assessment data from Ed Facts using the Urban Institute's API.
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
sr_state_assessments <- function(org_level, fips_code, years, grade = 99) {

  # TODO: allow one to filter by school / district

  # get each individual race and all races (99)
  race_to_use <- c(seq(1, 9), 99)

  edfacts <- intersect(sr_years_data_available('edfacts'), years)

  educationdata::get_education_data(
    level = org_level,
    source = "edfacts",
    topic = "assessments",
    filters = list(
      fips = fips_code,
      year = edfacts,
      grade_edfacts = grade
    ),
    subtopic = list("race"),
    add_labels = TRUE
  ) |>
    sr_clean_numeric_assessments()

}


#' Clean up numeric values of state assessments and calculate number who passed
#'
#' @param .data The state assessment data to clean.
#'
#' @keywords internal
sr_clean_numeric_assessments <- function(.data) {

  .data |>
    # clean up numeric columns
    dplyr::mutate(dplyr::across(dplyr::contains("_test_"), ~as.numeric(.x))) |>
    dplyr::mutate(dplyr::across(dplyr::contains("_test_"), ~ifelse(.x < 0, NA_real_, .x))) |>
    # convert integers to percentages
    dplyr::mutate(dplyr::across(dplyr::contains("_pct_prof_"), ~(.x / 100))) |>
    # calculate number of students who passed
    dplyr::mutate(
      read_test_num_pass = round(.data$read_test_num_valid * .data$read_test_pct_prof_midpt, 0),
      math_test_num_pass = round(.data$math_test_num_valid * .data$math_test_pct_prof_midpt, 0)
    )

}

#' Aggregate state assessment data
#'
#' Summarize aggregate percentages of state assessment data by using the total number of takers
#' and total numebr of passers
#'
#' @keywords internal
sr_aggregate_assessment <- function(.data, grouping_vars) {

  .data |>
    dplyr::group_by_at(grouping_vars) |>
    dplyr::summarize(dplyr::across(dplyr::contains('_num_'), ~sum(.x, na.rm = TRUE)), .groups = 'drop') |>
    dplyr::mutate(
      read_pct_pass = (.data$read_test_num_pass / .data$read_test_num_valid) * 100,
      math_pct_pass = (.data$math_test_num_pass / .data$math_test_num_valid) * 100
    )

}

#' Create a table with state-level assessment scores for the whole state and a single district
#'
#' Returns a single data set with school / district and state assessment scores, which allows
#' for comparisons between the school / district and state.
#'
#' @param state_assessment_data State assessment data set.
#' @param org_level The level to return the results. This is the 'level' parameter in the API.
#'      One of either 'schools' or 'school-district'
#' @param nces_num The district's LEAID number or school's NCES number, as a string.
#'
#' @returns A single data set that contains assessment scores broken down by race. It includes rows
#'      for the whole state and a single district that is specified.
#'
#' @export
sr_assessment_scores_by_race <- function(state_assessment_data, org_level, nces_num) {

  sr_check_level(org_level)

  # we will use a different column to identify the school or district
  # identify filtering column
  filter_col <- if (org_level == 'schools') 'ncessch' else 'leaid'
  geography_id <- if (org_level == 'schools') 'school_name' else 'lea_name'

  # when filtering the dataset, there can be problems with leading zeros.
  # therefore, remove leading zeros from the nces number and from the numbers in teh data set
  nces_num_remove_leading_zeros <- stringr::str_remove(nces_num, "^0+")

  # calculate district scores
  district <- state_assessment_data |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c('ncessch', 'leaid')), ~stringr::str_remove(.x, "^0+"))) |>
    dplyr::filter(.data[[filter_col]] %in% !!nces_num_remove_leading_zeros) |>
    sr_aggregate_assessment(c('year', 'race')) |>
    dplyr::mutate(geography = 'schools / districts')

  # calcualte state scores
  state_average_assessment <- state_assessment_data |>
    sr_aggregate_assessment(c('year', 'race')) |>
    dplyr::mutate(geography = 'state total')

  district |>
    dplyr::bind_rows(state_average_assessment) |>
    # rename racial categories and relevel
    dplyr::mutate(race = sr_rename_reorder_race_education(.data$race)) |>
    dplyr::mutate(school_year_both = sr_convert_to_sy(.data$year)) |>
    dplyr::arrange(.data$school_year_both, .data$race)

}

#' Clean enrollment by race data
#'
#' Calculate percentage breakdowns by race and clean racial category names.
#'
#' @param .data Data frame with enrollment data by race (school or district), created with `get_ccd_enrollment()`.
#'
#' @returns A data frame with percentage race breakdowns by year.
#'
#' @export
sr_clean_enrollment_by_race <- function(.data) {

  sr_check_required_cols(.data, c('year', 'enrollment', 'race'))

  .data |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      total_students = max(.data$enrollment),
      percent_race = .data$enrollment / .data$total_students,
      percent_race_clean = scales::percent(.data$percent_race, accuracy = 1),
      percent_race = .data$percent_race * 100,
      race = sr_rename_reorder_race_education(.data$race)
    ) |>
    dplyr::filter(.data$race != 'Total') |>
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
#' @keywords internal
sr_rename_reorder_race_education <- function(race_col) {

  race_order_education <- c('Black' = 'Black / African-American', 'Hispanic' = 'Hispanic / Latinx', 'White' = 'White')

  race <- dplyr::recode(race_col, !!!race_order_education)

  race <- forcats::fct_relevel(race, race_order_education)

  race <- forcats::fct_relevel(race, 'Two or more races', after = Inf)

  return(race)

}
