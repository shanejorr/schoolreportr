#' Create district flexdashboard for a given school district
#'
#' @export
knit_dashboard <- function(output_filename, org_level, nces_num, years, state_abb, grades) {

  rmarkdown::render(
    input = here::here("docs_to_render/school-dashboard-school.Rmd"),
    output_format = 'flexdashboard::flex_dashboard',
    output_file = here::here(output_filename),
    params = list(
      org_level = org_level,
      nces_num = nces_num,
      years = years,
      state_abb = state_abb,
      grades = grades
    )
  )
}
