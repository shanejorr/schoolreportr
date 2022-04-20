#' School directory
#'
#' General information on  the school.
#'
#' @keywords internal
get_school_directory <- function(nces_number, year) {

  educationdata::get_education_data(level = "schools",
         source = "ccd",
         topic = "directory",
         filters = list(year = year, ncessch = nces_number)
      ) %>%
    dplyr::select(
      year, ncessch, school_name, leaid, lea_name, street_location:zip_location, latitude, longitude,
      dplyr::contains('grade_offered'), teachers_fte, dplyr::contains('lunch'), enrollment
    ) %>%
    dplyr::mutate(
      free_or_reduced_lunch = free_lunch + reduced_price_lunch,
      perc_free_reduced_lunch = free_or_reduced_lunch / enrollment
    )
}
