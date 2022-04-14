#' Vector of state FIPS codes
#'
#' @keywords internal
fips_states <- function() {
  c(2, 4:6, 8:13, 15:42, 44:51, 53:56)
}

#' Years data is available for each Urban Inst. API data source
#'
#' @keywords internal
data_years_available <- function(data_source) {

  years_available <- list(
    ccd = 1986:2020,
    crdc = c(2011, 2013, 2015, 2017),
    edfacts = 2009:2018
  )

  years_available[[data_source]]

}

#' Base function to import data from the educationdata api
#'
#'
#' @keywords internal
get_eddata_topic_subtopic <- function(nces_number, years, source, topic, subtopic= NULL, grades = FALSE, fips_state = fips_states()) {

  source_values <- c('ccd', 'crdc', 'edfacts')

  if (!source %in% source_values) stop(glue::glue("`source` must be either {paste0(source_values, collapse = ", ", )}"))

  years <- switch(
    source,
    'ccd' = data_years_available('ccd'),
    'crdc' = intersect(data_years_available('crdc'), years),
    'edfacts' = intersect(data_years_available('edfacts'), years)
  )

  # create filter list based on whether grades are present
  if (!grades) {
    filter_list = list(
      year = years,
      ncessch = nces_number,
      fips = fips_state
    )
  } else if (grades & (source != 'edfacts')) {
    filter_list = list(
      year = years,
      ncessch = nces_number,
      grade = 99,
      fips = fips_state
    )
  } else if (grades & (source == 'edfacts')) {
    filter_list = list(
      year = years,
      ncessch = nces_number,
      grade_edfacts  = 99,
      fips = fips_state
    )
  } else {
    stop("`grades` must be TRUE or FALSE")
  }

  educationdata::get_education_data(
    level = 'schools',
    source = source,
    topic = topic,
    subtopic = subtopic,
    filters = filter_list,
    add_labels = TRUE
  )

}

#' School directory
#'
#' General information on  the school.
#'
#' @keywords internal
get_ccd_directory <- function(nces_number, years, fips_state = fips_states()) {

  get_eddata_topic_subtopic(nces_number, years, 'ccd', 'directory', fips_state = fips_state) %>%
    dplyr::select(
      year, ncessch, school_name, leaid, lea_name, street_location:zip_location, latitude, longitude,
      dplyr::contains('grade_offered'), teachers_fte, dplyr::contains('lunch'), enrollment
    ) %>%
    dplyr::mutate(
      free_or_reduced_lunch = free_lunch + reduced_price_lunch,
      perc_free_reduced_lunch = free_or_reduced_lunch / enrollment
    )
}

#' CCD enrollment by race
#'
#'
#' @keywords internal
get_ccd_enrollment_race <- function(nces_number, years, fips_state = fips_states()) {

  get_eddata_topic_subtopic(nces_number, years, source = 'ccd', topic = 'enrollment', subtopic = list('race'), grades = TRUE, fips_state = fips_state) %>%
    dplyr::select(year, ncessch, race,enrollment) %>%
    dplyr::group_by(year, ncessch) %>%
    dplyr::mutate(
      total_enrollment = max(enrollment),
      perc_enrollment = enrollment / total_enrollment
    ) %>%
    dplyr::ungroup()

}

#' Enrollment by limit English proficiency (LEP)
#'
#' Import data from the educationdata api that is school enrollment by LEP status
#' Uses the "school crdc enrollment lep sex" endpoint
#'
#' @keywords internal
get_crdc_school_enrollment_lep <- function(nces_number, years, fips_state = fips_states()) {

  get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "enrollment", subtopic = list("lep", "sex"), fips_state = fips_state) %>%
    dplyr::group_by(ncessch, year, lep) %>%
    dplyr::summarize(enrollment = sum(enrollment_crdc, na.rm = T), .groups = 'drop') %>%
    dplyr::mutate(
      lep = stringr::str_replace_all(lep, "^Students.*limited.*", "lep_limited"),
      lep = stringr::str_replace_all(lep, "^All.*", "total_students")
    ) %>%
    tidyr::pivot_wider(id_cols = c('ncessch', 'year'), names_from = 'lep', values_from = 'enrollment') %>%
    dplyr::mutate(perc_lep = lep_limited / total_students)

}

#' Percentage of ACT / SAT participation by race
#'
#' Returns the percentage of students taking the ACT/ SAT by race
#'
#' @param crdc_enrollment_by_race A data set of  CRDC enrollment by race. Needed so we can calculate percentages
#'    Created with get_crdc_topic_subtopics(nces_number, years, "enrollment", c("race", "sex")) %>% clean_crdc('enrollment_crdc')
#'
#' @keywords internal
get_crdc_school_test_participation_percentages <- function(nces_number, years, crdc_enrollment_by_race, fips_state = fips_states()) {

  # number of ACT / SAT takers by race
  get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "sat-act-participation", subtopic = c("race", "sex"), fips_state = fips_state) %>%
    clean_crdc('students_SAT_ACT') %>%
    # combine both data sets and calculate percentage
    dplyr::left_join(crdc_enrollment_by_race, by = c('ncessch','year','leaid','race')) %>%
    dplyr::filter(enrollment_crdc != 0) %>%
    dplyr::mutate(perc_sat_act = students_SAT_ACT / enrollment_crdc) %>%
    dplyr::rename(num_sat_act_takers = students_SAT_ACT)

}

#' Percentage of IB / AP takers by race
#'
#' @param crdc_enrollment_by_race A data set of  CRDC enrollment by race. Needed so we can calculate percentages
#'    Created with get_crdc_topic_subtopics(nces_number, years, "enrollment", c("race", "sex")) %>% clean_crdc('enrollment_crdc')
#'
#' @keywords internal
get_crdc_school_ib_ap <- function(nces_number, years, crdc_enrollment_by_race, fips_state = fips_states()) {

  # number of ACT / SAT takers by race
  get_eddata_topic_subtopic(nces_number, years, 'crdc', "ap-ib-enrollment", c("race", "sex"), fips_state = fips_state) %>%
    clean_crdc(c('enrl_IB', 'enrl_AP')) %>%
    # combine both data sets and calculate percentage
    dplyr::left_join(crdc_enrollment_by_race, by = c('ncessch','year','leaid','race')) %>%
    dplyr::filter(enrollment_crdc != 0) %>%
    dplyr::mutate(perc_ib = enrl_IB / enrollment_crdc) %>%
    dplyr::mutate(perc_ap = enrl_AP / enrollment_crdc)

}

#' Graduation Rates
#'
#' @keywords internal
get_edfacts_gradrates <- function(nces_number, years, fips_state = fips_states()) {

  message("Getting graduation rate data...")

  get_eddata_topic_subtopic(nces_number, years, source = 'edfacts', topic = "grad-rates", fips_state = fips_state) %>%
    dplyr::filter(disability == 'Total', econ_disadvantaged == 'Total', foster_care =='Total', lep == 'All students') %>%
    dplyr::select(year, ncessch, dplyr::starts_with('grad_rate')) %>%
    convert_to_na(c('grad_rate_high', 'grad_rate_low','grad_rate_midpt'))

}

#' State Assessments
#'
#' @keywords internal
get_edfacts_state_assessments <- function(nces_number, years, fips_state = fips_states()) {

  message("Getting assessment data...")

  get_eddata_topic_subtopic(nces_number, 2018, source = 'edfacts', topic = "assessments", grades = 99, subtopic = list('race'), fips_state = fips_state) %>%
    dplyr::select(ncessch, year, race, dplyr::contains('_test_')) %>%
    convert_to_na(dplyr::contains('_test_')) %>%
    dplyr::mutate(dplyr::across(dplyr::contains('_pct_'), ~(.x / 100)))

}

#' Convert educationdata api missing values to NA
#'
#' @keywords internal
convert_to_na <- function(.data, cols) {

  .data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ifelse(.x < 0, NA_real_, .x)))

}

#' Common clean up for CRDC data
#'
#' @keywords internal
clean_crdc <- function(.data, metric_colname) {

  initial_race_order <- c('Black, African-American', 'Hispanic / Latinx', 'White')

  .data <- .data %>%
    dplyr::filter(
      sex == 'Total',
      disability == 'Total'
    ) %>%
    dplyr::mutate(race = rename_race(race)) %>%
    dplyr::select(ncessch, year, leaid, race, dplyr::all_of(metric_colname))

  .data$race <- relevel_race(.data$race)

  return(.data)

}

#' Get education data
#'
#' Imports multiple pieces of data from the Urban Institute's educationdata API at once. Users can specify
#' the needed data sets and all data sets will be returned in a single list.
#'
#' @param nces_number The school's NCES number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param data_sources The data sources to retrieve. Can be one of the following:
#'      enrollment race', 'enrollment lep', 'test participation', 'IB AP enrollment', 'state assessments'
#'
#' @returns A list with each element containing a data frame with school data. The list is named,
#'      which identifies the data contained in the list element.
#'
#' @export
get_all_educationdata <- function(nces_number, years, data_sources) {

  data_source_options <- c(
    'enrollment race', 'enrollment lep', 'test participation', 'IB AP enrollment', 'state assessments'
  )

  # make sure that all input data_source items are valid and throw error if one is not
  wrong_input <- setdiff(data_sources, data_source_options)

  if (length(wrong_input) > 0) stop(
    glue::glue("At least one of your `data_source` items are incorrect. `data_source` must be one of {paste0(data_source_options, collapse = ', ')}")
  )

  source_suffix <- "/nvia Education Data Portal v. 0.14.0, Urban Institute."

  school_data <- list()

  # always supply directory information
  message("Getting school directory information ...")

  school_data[['directory']] <- list(
    data = get_ccd_directory(nces_number, max(years)),
    source = glue::glue("Common Core of Data{source_suffix}")
  )

  # need crdc enrollment by race to calculate percentage
  # we are not going to write it out, so we don't need it in the list containing all information
  if ('test participation' %in% data_sources | 'IB AP enrollment' %in% data_sources) {
    crdc_enrollment_by_race <- get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "enrollment", subtopic = c("race", "sex")) %>%
      clean_crdc('enrollment_crdc')
  }


  if ('enrollment race' %in% data_sources) {
    message("Getting CCD enrollment by race data...")
    school_data[['enrollment_race']] <- list(
      data = get_ccd_enrollment_race(nces_number, years, fips_state = fips_states()),
      source = glue::glue("Common Core of Data{source_suffix}")
    )
  }

  if ('enrollment lep' %in% data_sources) {
    message("Getting CRDC LEP data...")
    school_data[['enrollment_lep']] <- list(
      data = get_crdc_school_enrollment_lep(nces_number, years),
      source = glue::glue("The Civil Rights Data Collection{source_suffix}")
    )

  }

  if ('test participation' %in% data_sources) {
    message("Getting CRDC ACT / SAT participation data...")
    school_data[['test_participation']] <- list(
      data = get_crdc_school_test_participation_percentages(nces_number, years, crdc_enrollment_by_race),
      source = glue::glue("The Civil Rights Data Collection{source_suffix}")
    )
  }

  if ('IB AP enrollment' %in% data_sources) {
    message("Getting CRDC IB / AP data participation data...")
    school_data[['ib_ap_enrollment']] <- list(
      data = get_crdc_school_test_participation_percentages(nces_number, years, crdc_enrollment_by_race),
      source = glue::glue("The Civil Rights Data Collection{source_suffix}")
    )
  }

  if ('state assessments' %in% data_sources) {
    message("Getting EDFacts state assessment data...")
    school_data[['state_assessments']] <- list(
      data = get_edfacts_state_assessments(nces_number, years),
      source = glue::glue("EDFacts{source_suffix}")
    )
  }

  if ('grad rates' %in% data_sources) {
    message("Getting EDFacts graduation rate data...")
    school_data[['grad_rates']] <- list(
      data = get_edfacts_gradrates(nces_number, years),
      source = glue::glue("EDFacts{source_suffix}")
    )
  }

  return(school_data)

}
