#' Rename racial groups
#'
#' Rename Hispanic / latinx and Black, African-American
#'
#' @keywords internal
rename_race <- function(race_col) {

  race_col <- as.character(race_col)

  dplyr::case_when(
    stringr::str_detect(race_col, ".*[H|h]isp.*") ~ "Hispanic / Latinx",
    stringr::str_detect(race_col, ".*Black.*|.*African.*") ~ "Black, African-American",
    TRUE ~ race_col
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

  race_col <- forcats::fct_relevel(race_col, 'Black, African-American')
  race_col <- forcats::fct_relevel(race_col, 'Hispanic / Latinx', after = 1)
  race_col <- forcats::fct_relevel(race_col, 'White', after = 2)
  race_col <- forcats::fct_relevel(race_col, 'Two or more races', after = Inf)

  return(race_col)
}

#' Get state FIPS codes from state abbreviations
#'
#' @keywords internal
state_fips_code <- function(state_abb) {

  tidycensus::fips_codes %>%
    dplyr::filter(state == !!state_abb) %>%
    dplyr::distinct(state_code) %>%
    dplyr::pull(state_code) %>%
    as.numeric()

}

#' Create percentage columns for plotting
#'
#' @keywords internal
pretty_percent_cols <- function(.data, perc_col) {

  .data %>%
    dplyr::mutate(
      .perc_plot = .data[[perc_col]] * 100,
      .perc_cleaned = scales::percent(.data[[perc_col]], accuracy = 1)
    )

}
