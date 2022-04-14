#' ACS Census demographic data
#'
#' Import demographic data from the ACS. Import multiple tables at once and return in single dataframe.
#'
#' @keywords internal
acs_demographic_data <- function(acs_variables, state, tract_fips) {

  acs_geographies <- c('state', 'tract')

  purrr::map_df(acs_geographies, function(.x) {
    df <- tidycensus::get_acs(
      geography = .x,
      variables = acs_variables,
      survey = 'acs5',
      state = state,
      moe_level = 95
    )

    if (.x == 'tract') {
      df <- df %>%
        dplyr::filter(GEOID %in% !!tract_fips)
    }

    return(df)
  }) %>%
    dplyr::mutate(geography = ifelse(stringr::str_detect(NAME, 'Tract'), 'District Average', 'State Average'))
}

#' Covert raw counts to percentages
#'
#' Take a data set of raw count numbers and calcualte percentages for groups.
#'
#' @keywords internal
calculate_percentages <- function(.data, total_variable_string) {

  .data %>%
    dplyr::group_by(geography, variable) %>%
    dplyr::summarize(n_demo = sum(estimate), .groups = 'drop_last') %>%
    dplyr::mutate(
      n_total = max(n_demo),
      perc_demo = (n_demo / n_total) * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(variable != total_variable_string)
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
  education <- acs_demographic_data(education_variables, state, tract_fips)  %>%
    dplyr::left_join(acs_vars[c('name', 'label')], by = c('variable' = 'name'))

  # re-bin educational attainment levels
  education %>%
    # use variable numbers to bin groups
    dplyr::mutate(variable_number = stringr::str_extract(variable, "[0-9]{3}$") %>% as.numeric()) %>%
    dplyr::mutate(education_level = dplyr::case_when(
      variable_number == 1 ~ 'Population 25 and over',
      dplyr::between(variable_number, 2, 16) ~ 'No HS diploma or GED',
      dplyr::between(variable_number, 17, 18) ~ 'HS diploma or GED',
      dplyr::between(variable_number, 19, 21) ~ "Some college or associates degree",
      dplyr::between(variable_number, 22, 22) ~ "Bachelor's degree",
      dplyr::between(variable_number, 23, 25) ~ "Beyond bachelor's",
      TRUE ~ 'Failed to match'
    )) %>%
    # create ordered factor based on variable number
    dplyr::group_by(education_level) %>%
    dplyr::mutate(max_variable_number = max(variable_number)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(education_level = forcats::fct_reorder(education_level, max_variable_number)) %>%
    # don't need these variables because we have descriptive labels for them
    dplyr::select(-variable_number, -variable, -label) %>%
    #rename tpo match function that calculates percentages
    dplyr::rename(variable = education_level) %>%
    # calculate percentages for each newly created bin
    calculate_percentages('Population 25 and over')

}

#' Get state FIPS codes
#'
#' @keywords internal
get_state_fips <- function(state_abbreviation) {

  data(fips_codes, package = 'tidycensus')

  fips_codes %>%
    dplyr::filter(state == state_abbreviation) %>%
    pull(state_code) %>%
    unique() %>%
    as.numeric()
}
