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
