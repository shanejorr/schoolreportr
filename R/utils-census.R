#' ACS Census demographic data
#'
#' Import demographic data from the ACS. Import multiple tables at once and return in single dataframe.
#'
#' @keywords internal
acs_demographic_data <- function(acs_variables, state, tract_fips) {

  acs_geographies <- c('state', 'tract')

  purrr::map(acs_geographies, function(.x) {
    df <- tidycensus::get_acs(
      geography = .x,
      variables = acs_variables,
      survey = 'acs5',
      state = state,
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

#' Covert raw counts to percentages
#'
#' Take a data set of raw count numbers and calcualte percentages for groups.
#'
#' @keywords internal
calculate_percentages <- function(.data, total_variable_string) {

  .data |>
    dplyr::group_by_at(c('geography', 'variable')) |>
    dplyr::summarize(n_demo = sum(.data$estimate), .groups = 'drop_last') |>
    dplyr::mutate(
      n_total = max(.data$n_demo),
      perc_demo = (.data$n_demo / .data$n_total) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$variable != !!total_variable_string)
}

#' Calculate educational attainment
#'
#' Pull in educational attainment data, combine educational levels, and calcualte the percentage
#' of the populatin that has attained certain educational levels.
#'
#' @keywords internal
educational_attainment <- function(state, tract_fips, year) {

  # list of acs variables for educational attainment
  education_variables <- stringr::str_pad(1:25, width = 3, side = 'left', pad = '0')
  education_variables <- stringr::str_c('B15003_', education_variables)

  acs_vars <- tidycensus::load_variables(year, "acs5", cache = TRUE)

  # get county and state educational attainment numbers
  education <- acs_demographic_data(education_variables, state, tract_fips)  |>
    dplyr::left_join(acs_vars[c('name', 'label')], by = c('variable' = 'name'))

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
    calculate_percentages('Population 25 and over')

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
#' @param .data Data set containing imported census demographic data created with `acs_demographic_data()`
#' @param races_to_use Races that we want to include in the data, as a vector of strings.
#'
#' @keywords internal
acs_tracts_race <- function(.data, races_to_use) {

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


#' Create choropath of census tracts and racial brakdowns
#'
#' @keywords internal
choropath_tracts_race <- function(.data, race, tracts_in_district, district_shapefile, specific_race_population, school_directory) {

  specific_race_population <- .data |> dplyr::filter(.data$variable == !!race)

  labels <- glue::glue(
    "<strong>{race} Population</strong><br/>
   <strong>{specific_race_population$estimate}</strong> {race} residents (+/- {specific_race_population$moe})<br/>
   <strong>{specific_race_population$perc_estimate}</strong> {race} population (+/- {specific_race_population$perc_moe})"
  ) |>
    lapply(htmltools::HTML)

  leaflet_census_tracts(
    tracts_in_district,
    specific_race_population,
    district_shapefile, school_directory, labels
  )

}
