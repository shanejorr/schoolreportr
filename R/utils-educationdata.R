#' Import school overview data
#'
#' Import data from the educationdata api that is an overview of the school.
#' Uses the "school ccd directory" endpoint.
#'
#' @keywords internal
get_ccd_school_directory <- function(nces_number, year) {

  message("Getting basic school information...")

  educationdata::get_education_data(
    level = 'schools',
    source = 'ccd',
    topic = 'directory',
    filters = list(
      year = year,
      ncessch = nces_number,
      school_type = 1
    ),
    add_labels = TRUE
  )
}


#' Enrollment by race
#'
#' Import data from the educationdata api that is school enrollment by race.
#' Uses the "school ccd enrollment" endpoint
#'
#' @keywords internal
get_ccd_school_enrollment_race <- function(nces_number, years) {

  message("Getting CCD enrollment data by race...")

  educationdata::get_education_data(
    level = 'schools',
    source = 'ccd',
    topic = 'enrollment',
    subtopic = list('race'),
    filters = list(
      year = years,
      ncessch = nces_number
    ),
    add_labels = TRUE
  )
}

#' Get years CRDC is available
#'
#' CRDC data is not available every year. This function takes a sequence of years and returns
#' a vector of years that CRDC data is available
#'
#' @keywords internal
crdc_years <- function(years) {

  crdc_years <- c(2011, 2013, 2015, 2017)

  intersect(crdc_years, years)

}

#' Common clean up for CRDC data
#'
#' @keywords internal
clean_crdc <- function(.data, metric_colname) {

  .data %>%
    dplyr::filter(
      sex == 'Total',
      disability == 'Total'
    ) %>%
    dplyr::filter(race != 'Total') %>%
    dplyr::select(ncessch, year, leaid, race, dplyr::all_of(metric_colname))

}

#' Enrollment by limit English proficiency (LEP)
#'
#' Import data from the educationdata api that is school enrollment by LEP status
#' Uses the "school crdc enrollment lep sex" endpoint
#'
#' @keywords internal
get_crdc_school_enrollment_lep <- function(nces_number, years) {

  message("Getting CRDC LEP data...")

  years_crdc <- crdc_years(years)

  educationdata::get_education_data(
    level = "schools",
    source = "crdc",
    topic = "enrollment",
    subtopic = c("lep", "sex"),
    filters = list(
      year = years_crdc,
      ncessch = nces_number
    ),
    add_labels = TRUE
  ) %>%
    dplyr::group_by(ncessch, year, lep) %>%
    dplyr::summarize(enrollment = sum(enrollment_crdc, na.rm = T), .groups = 'drop') %>%
    dplyr::mutate(
      lep = stringr::str_replace_all(lep, "^Students.*limited.*", "lep_limited"),
      lep = stringr::str_replace_all(lep, "^All.*", "total_students")
    ) %>%
    tidyr::pivot_wider(id_cols = c('ncessch', 'year'), names_from = 'lep', values_from = 'enrollment') %>%
    dplyr::mutate(perc_lep = lep_limited / total_students)

}

#' Number of ACT / SAT participation by race
#'
#' Returns the number of students taking the ACT/ SAT by race
#'
#' @keywords internal
get_crdc_school_test_participation_numbers <- function(nces_number, years) {

  years_crdc <- crdc_years(years)

  # get number of participants by race
  educationdata::get_education_data(
    level = "schools",
    source = "crdc",
    topic = "sat-act-participation",
    subtopic = list("race", "sex"),
    filters = list(
      year = years_crdc,
      ncessch = nces_number
    ),
    add_labels = TRUE
  ) %>%
    clean_crdc('students_SAT_ACT') %>%
    dplyr::rename(num_students_SAT_ACT = students_SAT_ACT)

}

#' CRDC enrollment by races
#'
#' Returns student enrollment by race. Needed so that we can calculate the percentage of each race
#' that takes the ACT / SAT.
#'
#' @keywords internal
get_crdc_school_enrollment_race <- function(nces_number, years) {

  years_crdc <- crdc_years(years)

  educationdata::get_education_data(
    level = "schools",
    source = "crdc",
    topic = "enrollment",
    subtopic = list("race", "sex"),
    filters = list(
      year = years_crdc,
      ncessch = nces_number
    ),
    add_labels = TRUE
  ) %>%
    clean_crdc('enrollment_crdc')

}

#' Percentage of ACT / SAT participation by race
#'
#' Returns the percentage of students taking the ACT/ SAT by race
#'
#' @keywords internal
get_crdc_school_test_participation_percentages <- function(nces_number, years) {

  message("Getting CRDC ACT / SAT participation data...")

  # number of ACT / SAT takers by race
  num_takers <- get_crdc_school_test_participation_numbers(nces_number, years)

  # enrollment by race (needed to calculate percentages)
  enrollment_race <- get_crdc_school_enrollment_race(nces_number, years)

  # combine both data sets and calculate percentage
  num_takers %>%
    dplyr::left_join(enrollment_race, by = c('ncessch','year','leaid','race')) %>%
    dplyr::filter(enrollment_crdc != 0) %>%
    dplyr::mutate(perc_sat_act = num_students_SAT_ACT / enrollment_crdc)

}

#' Number of teachers / staff at the school
#'
#' @keywords internal
get_crdc_school_test_participation_percentages <- function(nces_number, years) {

  years_crdc <- crdc_years(years)

  educationdata::get_education_data(
    level = "schools",
    source = "crdc",
    topic = "teachers-staff",
    filters = list(
      year = years_crdc,
      ncessch = nces_number
    ),
    add_labels = TRUE
  ) %>%
    dplyr::select(year, ncessch, teachers_fte_crdc, counselors_fte) %>%
    dplyr::mutate(counselors_fte = ifelse(counselors_fte < 0, NA_real_, counselors_fte))
}
