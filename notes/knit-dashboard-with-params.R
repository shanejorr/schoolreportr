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
