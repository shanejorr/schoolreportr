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
