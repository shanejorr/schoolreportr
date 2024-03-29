#' Import ACS census data for a state and tracts
#'
#' Imports census tract and state-level demographic data from the ACS.
#' Then combines the census tract and state-level information into one data set.
#' This is a helper function for creating the Rmarkdown file and has little application
#' outside this file.
#'
#' @param acs_variables The variable names from the ACS that we want to pull.
#' @param state_abb The two letter state abbreviation on the state that
#'      you want to show all schools for.
#' @param tract_fips The fips code of the census tracts that we want to get data for.
#'
#' @export
sr_demographic_data <- function(acs_variables, state_abb, tract_fips) {

  acs_geographies <- c('state', 'tract')

  purrr::map(acs_geographies, function(.x) {
    df <- tidycensus::get_acs(
      geography = .x,
      variables = acs_variables,
      survey = 'acs5',
      state = state_abb,
      moe_level = 95
    )

    if (.x == 'tract') {
      df <- df |>
        dplyr::filter(.data$GEOID %in% !!tract_fips)
    }

    return(df)
  })  |>
    purrr::list_rbind() |>
    dplyr::mutate(geography = ifelse(stringr::str_detect(.data$NAME, 'Tract'), 'District Average', 'State Average'))
}

#' Calculate percentages in district given counts of each tract in a district
#'
#' Calculates overall percentage demographics in a district and state given counts
#' in all census tracts in a district and overall state counts. For example, calculates
#' racial breakdown by percentage in the district by creating aggregate
#' percentage for all census tracts in the district. Input data is created with [`sr_demographic_data()`].
#'
#' This is a helper function used to create the Rmarkdown report.
#'
#' @param .data Data frame containing census demographic data. Data.frame should
#'      be created with `sr_demographic_data`.
#'
#' @export
sr_calculate_percentages <- function(.data) {

  sr_check_required_cols(.data, c('geography', 'variable', 'estimate'))

  .data |>
    dplyr::group_by_at(c('geography', 'variable')) |>
    dplyr::summarize(n_demo = sum(.data$estimate), .groups = 'drop_last') |>
    dplyr::mutate(
      n_total = max(.data$n_demo),
      perc_demo = (.data$n_demo / .data$n_total) * 100
    ) |>
    dplyr::ungroup()
}

#' Calculate educational attainment
#'
#' Pull in educational attainment data, combine educational levels, and calculate the percentage
#' of the population that has attained certain educational levels. Uses `tidycensus`
#' to import education attainment data from the census ACS.
#'
#' Helper function to create Rmarkdown report.
#'
#' @param state_abb The two letter state abbreviation on the state that
#'      you want to show all schools for.
#' @param tract_fips The fips code of the census tracts that we want to get data for.
#'
#' @export
sr_rmd_get_educational_attainment <- function(state_abb, tract_fips) {

  # list of acs variables for educational attainment
  education_variables <- stringr::str_pad(1:25, width = 3, side = 'left', pad = '0')
  education_variables <- stringr::str_c('B15003_', education_variables)

  # get county and state educational attainment numbers
  education <- sr_demographic_data(education_variables, state_abb, tract_fips)

  # re-bin educational attainment levels
  education |>
    # use variable numbers to bin groups
    dplyr::mutate(variable_number = stringr::str_extract(.data$variable, "[0-9]{3}$") |> as.numeric()) |>
    dplyr::mutate(education_level = dplyr::case_when(
      .data$variable_number == 1 ~ 'Population 25 and over',
      dplyr::between(.data$variable_number, 2, 16) ~ 'No HS diploma or GED',
      dplyr::between(.data$variable_number, 17, 18) ~ 'HS diploma or GED',
      dplyr::between(.data$variable_number, 19, 21) ~ "Some college or associates degree",
      dplyr::between(.data$variable_number, 22, 22) ~ "Bachelor's degree",
      dplyr::between(.data$variable_number, 23, 25) ~ "Beyond bachelor's",
      TRUE ~ 'Failed to match'
    )) |>
    # create ordered factor based on variable number
    dplyr::group_by(.data$education_level) |>
    dplyr::mutate(max_variable_number = max(.data$variable_number)) |>
    dplyr::ungroup() |>
    dplyr::mutate(education_level = forcats::fct_reorder(.data$education_level, .data$max_variable_number)) |>
    # don't need these variables because we have descriptive labels for them
    dplyr::select(-dplyr::all_of(c("variable_number", "variable", "label"))) |>
    #rename tpo match function that calculates percentages
    dplyr::rename(variable = .data$education_level) |>
    # calculate percentages for each newly created bin
    sr_calculate_percentages() |>
    dplyr::filter(.data$variable != 'Population 25 and over')

}

#' Get state FIPS codes
#'
#' @keywords internal
get_state_fips <- function(state_abbreviation) {

  # fips_code in internal data saved in sysdata.rda
  fips_codes |>
    dplyr::filter(.data$state == !!state_abbreviation) |>
    dplyr::pull(.data$state_code) |>
    unique() |>
    as.numeric()
}

#' Create data set with racial population and percentage breakdowns by census tracts
#'
#' Cleans ACS census data with race variables by putting it into a format where it
#' can be plotted with a choropleth map in highcharts.
#'
#' Helper function that is used to create Rmarkdown file
#'
#' @param .data Data set containing imported census demographic data created with [`sr_demographic_data()`]
#' @param races_to_use Races that we want to include in the data, as a vector of strings.
#'
#' @export
sr_rmd_census_tracts_race <- function(.data, races_to_use) {

  sr_check_required_cols(.data, c('NAME', 'variable', 'estimate'))

  .data |>
    dplyr::filter(stringr::str_detect(.data$NAME, 'Tract')) |>
    dplyr::group_by(.data$NAME) |>
    dplyr::mutate(total_estimate = max(.data$estimate), total_moe = max(.data$moe)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$variable %in% !!races_to_use) |>
    dplyr::mutate(
      perc_estimate = .data$estimate / .data$total_estimate,
      color_pal = leaflet::colorNumeric("Blues", .data$estimate)(.data$estimate),
      perc_estimate = scales::percent(.data$perc_estimate, accuracy = 1),
      perc_moe = tidycensus::moe_prop(.data$estimate, .data$total_estimate, .data$moe, .data$total_moe) * 100,
      dplyr::across(c('estimate', 'moe', 'perc_moe'), ~round(., 0))
    )

}
