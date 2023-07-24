# get state fips codes

fips_codes <- tidycensus:::fips_codes
  dplyr::filter(.data$state == !!state_abbreviation) |>
  dplyr::pull(.data$state_code) |>
  unique() |>
  as.numeric()

usethis::use_data(internal_this, internal_that, internal = TRUE)
