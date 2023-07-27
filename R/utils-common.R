#' Rename racial groups
#'
#' Rename Hispanic / latinx and Black, African-American
#'
#' @keywords internal
rename_race <- function(race_col) {

  race_col <- as.character(race_col)

  dplyr::case_when(
    stringr::str_detect(.data$race_col, ".*[H|h]isp.*") ~ "Hispanic / Latinx",
    stringr::str_detect(.data$race_col, ".*Black.*|.*African.*") ~ "Black, African-American",
    .default ~ .data$race_col
  )

}

#' Relevel race factors
#'
#' Releve race factors in order of Black, Hispanic / Latinx, White, and Two or more at the end
#'
#' @keywords internal
relevel_race <- function(race_col) {

  # rename race to ensure it matches standard names
  race_col <- rename_race(race_col)

  race_col <- forcats::fct_relevel(.data$race_col, 'Black, African-American')
  race_col <- forcats::fct_relevel(.data$race_col, 'Hispanic / Latinx', after = 1)
  race_col <- forcats::fct_relevel(.data$race_col, 'White', after = 2)
  race_col <- forcats::fct_relevel(.data$race_col, 'Two or more races', after = Inf)

  return(race_col)
}

#' Get state FIPS codes from state abbreviations
#'
#' Pulling state data from APIs often requires knowing teh state's fips code, which
#' is an integer. This function allows you to retreive a single state's fips code
#' by entering the state abbreviation.
#'
#' @param state_abb Two letter state abbreviation of the state that you want to
#'      retrieve the fips code.
#'
#' @returns A single integer representing the fips code of the specified state.
sr_state_fips_code <- function(state_abb) {

  state_abb <- stringr::str_to_upper(state_abb)

  # ensure the state_abb character is in fact a state
  if (!state_abb %in% datasets::state.abb) {
    stop("`state_abb` is not a state. Make sure `state_abb` is the state's two letter abbreviation.", call. = FALSE)
  }

  tidycensus::fips_codes |>
    dplyr::filter(.data$state == !!state_abb) |>
    dplyr::distinct(.data$state_code) |>
    dplyr::pull(.data$state_code) |>
    as.numeric()

}

#' Create percentage columns for plotting
#'
#' @keywords internal
pretty_percent_cols <- function(.data, perc_col) {

  .data |>
    dplyr::mutate(
      .perc_plot = .data[[perc_col]] * 100,
      .perc_cleaned = scales::percent(.data[[perc_col]], accuracy = 1)
    )

}

#' Check Required Columns
#'
#' This function checks if all the required columns are present in a data frame.
#'
#' @param .data The data frame to check.
#' @param required_cols A vector of required column names.
#'
#' @return A logical value indicating if all the required columns are present.
#'
#' @keywords internal
sr_check_required_cols <- function(.data, required_cols) {

  # Get the column names of the data
  data_cols <- colnames(.data)

  # Check if each required column is in the data
  cols_present <- required_cols %in% data_cols

  # If not all columns are present, produce error
  if (!all(cols_present)) {
    missing_cols <- required_cols[!cols_present]
    stop(paste("The following required columns are missing: ",
                  paste(missing_cols, collapse = ", "), ".",
                  call. = FALSE))
  }

  # Return TRUE if all columns are present, FALSE otherwise
  all(cols_present)
}
