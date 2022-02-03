get_eddata_topic_subtopic <- function(nces_number, years, source = c('ccd', 'crdc', 'edfacts'), topic, subtopic= NULL, grades = FALSE) {

  years <- switch(
    source,
    'ccd' = years,
    'crdc' = intersect(c(2011, 2013, 2015, 2017), years),
    'edfacts' = intersect(2009:2018, years)
  )

  # create filter list based on whether grades are present
  if (!grades) {
    filter_list = list(
      year = years,
      ncessch = nces_number
    )
  } else if (grades & (source != 'edfacts')) {
    filter_list = list(
      year = years,
      ncessch = nces_number,
      grade = 99
    )
  } else if (grades & (source == 'edfacts')) {
    filter_list = list(
      year = years,
      ncessch = nces_number,
      grade_edfacts  = 99
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
get_ccd_directory <- function(nces_number, years) {

  get_eddata_topic_subtopic(nces_number, years, 'ccd', 'directory') %>%
    dplyr::select(
      year, ncessch, school_name, leaid, lea_name, street_location:zip_location, latitude, longitude,
      contains('grades_offered'), teachers_fte, contains('lunch'), enrollment
    ) %>%
    dplyr::mutate(
      free_or_reduced_lunch = free_lunch + reduced_price_lunch,
      perc_free_reduced_lunch = free_or_reduced_lunch / enrollment
    )
}

#' Enrollment by limit English proficiency (LEP)
#'
#' Import data from the educationdata api that is school enrollment by LEP status
#' Uses the "school crdc enrollment lep sex" endpoint
#'
#' @keywords internal
get_crdc_school_enrollment_lep <- function(nces_number, years) {

  message("Getting CRDC LEP data...")

  get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "enrollment", subtopic = list("lep", "sex")) %>%
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
#'    Created with \code{get_crdc_topic_subtopics(nces_number, years, "enrollment", c("race", "sex")) %>% clean_crdc('enrollment_crdc')}
#'
#' @keywords internal
get_crdc_school_test_participation_percentages <- function(nces_number, years, crdc_enrollment_by_race) {

  message("Getting CRDC ACT / SAT participation data...")

  # number of ACT / SAT takers by race
  get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "sat-act-participation", subtopic = c("race", "sex")) %>%
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
#'    Created with \code{get_crdc_topic_subtopics(nces_number, years, "enrollment", c("race", "sex")) %>% clean_crdc('enrollment_crdc')}
#'
#' @keywords internal
get_crdc_school_ib_ap <- function(nces_number, years, crdc_enrollment_by_race) {

  message("Getting CRDC IB / AP data participation data...")

  # number of ACT / SAT takers by race
  get_eddata_topic_subtopic(nces_number, years, 'crdc', "ap-ib-enrollment", c("race", "sex")) %>%
    clean_crdc(c('enrl_IB', 'enrl_AP')) %>%
    # combine both data sets and calculate percentage
    dplyr::left_join(crdc_enrollment_by_race, by = c('ncessch','year','leaid','race')) %>%
    dplyr::filter(enrollment_crdc != 0) %>%
    dplyr::mutate(perc_ib = enrl_IB / enrollment_crdc) %>%
    dplyr::mutate(perc_ap = enrl_AP / enrollment_crdc)

}

#' Percentage of IB / AP takers by race
#'
#' @param crdc_enrollment_by_race A data set of  CRDC enrollment by race. Needed so we can calculate percentages
#'    Created with \code{get_crdc_topic_subtopics(nces_number, years, "enrollment", c("race", "sex")) %>% clean_crdc('enrollment_crdc')}
#'
#' @keywords internal
get_edfacts_gradrates <- function(nces_number, years) {

  message("Getting graduation rate data...")

  get_eddata_topic_subtopic(nces_number, years, source = 'edfacts', topic = "grad-rates") %>%
    dplyr::filter(disability == 'Total', econ_disadvantaged == 'Total', foster_care =='Total', lep == 'All students') %>%
    dplyr::select(year, ncessch, dplyr::starts_with('grad_rate')) %>%
    convert_to_na(c('grad_rate_high', 'grad_rate_low','grad_rate_midpt'))

}


#' Convert educationdata api missing values to NA
#'
#' @keywords internal
convert_to_na <- function(.data, cols) {

  .data %>%
    dplyr::mutate(dplyr::across(all_of(cols), ~ifelse(.x < 0, NA_real_, .x)))

}

#' Common clean up for CRDC data
#'
#' @keywords internal
clean_crdc <- function(.data, metric_colname) {

  initial_race_order <- c('Black, African-American', 'Hispanic / Latinx', 'White')

  .data %>%
    dplyr::filter(
      sex == 'Total',
      disability == 'Total'
    ) %>%
    dplyr::filter(race != 'Total') %>%
    dplyr::mutate(
      race = rename_race(race),
      race = forcats::fct_relevel(race, initial_race_order) %>%
        forcats::fct_relevel('Two or more races', after = Inf)) %>%
    dplyr::select(ncessch, year, leaid, race, dplyr::all_of(metric_colname))

}

# directory
get_ccd_directory(nces_number, 2018)

# enrollment by race
get_eddata_topic_subtopic(nces_number, 2018, source = 'ccd', topic = 'enrollment', subtopic = list('race'), grade = TRUE) %>%
  select(year, ncessch, race,enrollment)

# enrollment by lep
get_crdc_school_enrollment_lep(nces_number, years)

# crdc enrollment by race
# used to calculate percentages in other metrics
crdc_enrollment_by_race <- get_eddata_topic_subtopic(nces_number, years, source = 'crdc', topic = "enrollment", subtopic = c("race", "sex")) %>%
    clean_crdc('enrollment_crdc')

# SAT / ACT test participation percentageg
get_crdc_school_test_participation_percentages(nces_number, years, crdc_enrollment_by_race)


# percentage enrollment in AP / IP
get_crdc_school_ib_ap(nces_number, years, crdc_enrollment_by_race)

# state assessments by grade and race
assessments <- get_eddata_topic_subtopic(nces_number, 2018, source = 'edfacts', topic = "assessments", subtopic = list("race"), grades = 99) %>%
  dplyr::select(ncessch, year, school_name, race, contains('_test_'))

# graduation rtes
get_edfacts_gradrates(nces_number, 2018)




#' Get all educationdata data
#'
#' Single function that calls all educationdata functions and returns all the data in a single list
#'
#' @keywords internal
get_all_educationdata <- function(nces_number, years) {

  source_suffix <- "/nvia Education Data Portal v. 0.14.0, Urban Institute."

  school_directory <- list(data = get_ccd_school_directory(nces_number, year),
                           source = glue::glue("Common Core of Data{source_suffix}"))

  school_enrollment_race <- list(data = get_ccd_school_enrollment_race(nces_number, years),
                                 source = glue::glue("Common Core of Data{source_suffix}"))

  school_enrollment_lep <- list(data = get_crdc_school_enrollment_lep(nces_number, years),
                                source = glue::glue("The Civil Rights Data Collection{source_suffix}"))

  school_test_takers <- list(data = get_crdc_school_test_participation_percentages(nces_number, years),
                             source = glue::glue("The Civil Rights Data Collection{source_suffix}"))

  school_staff <- list(data = get_crdc_school_staff(nces_number, years),
                       source = glue::glue("The Civil Rights Data Collection{source_suffix}"))

  state_assessment <- list(data = get_edfacts_state_assessments(nces_number, year),
                           source = glue::glue("EDFacts{source_suffix}"))
}
