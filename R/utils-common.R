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

  race_col <- forcats::fct_relevel(race_col, 'Black, African-American')
  race_col <- forcats::fct_relevel(race_col, 'Hispanic / Latinx', after = 1)
  race_col <- forcats::fct_relevel(race_col, 'White', after = 2)
  race_col <- forcats::fct_relevel(race_col, 'Two or more races', after = Inf)

  return(race_col)
}
