# Knit the flexdashboard with the district LEA ID as a parameter.
# Iterate through each district and knit.
# Output is a different flexdashboard for each LEA ID

library(tidyverse)

source('global_functions.R')

district_ids <- school_district_numbers()$district_lea

district_id <- district_ids[[1]]

output_filename <- here::here(glue::glue("create-dashboard/school-leaid-{district_id}.html"))

rmarkdown::render(
  here::here("create-dashboard/school-dashboard.Rmd"),
  output_format = 'flexdashboard::flex_dashboard',
  output_file = output_filename,
  params = list(
    year = 2019,
    district_lea_id = district_id
    )
)

knit_dashboard <- function(output_filename, district_leaid_number, years, state_abb, grades) {

  rmarkdown::render(
    input = here::here("create-dashboard/school-dashboard-district.Rmd"),
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

knit_dashboard('test.html', '0614550', 'CA', 2000:max(data_years_available('ccd')))

