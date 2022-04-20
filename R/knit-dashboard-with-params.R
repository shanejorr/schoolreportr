#' Create district flexdashboard for a given school district
#'
#' @export
knit_dashboard <- function(output_filename, district_leaid_number, years, state_abb, grades) {

  rmarkdown::render(
    input = here::here("docs_to_render/school-dashboard-district.Rmd"),
    output_format = 'flexdashboard::flex_dashboard',
    output_file = here::here(output_filename),
    params = list(
      district_leaid_number = district_leaid_number,
      years = years,
      state_abb = state_abb,
      grades = grades
    )
  )

}
