# create dashboards
library(schoolreportr)

output_dir <- here::here('notes', 'school-dashboards')

# Fresno ---------------------
nces_num <- c(heaton = '061455001735', williams = '061455011895') # Heaton and Williams
state_abb <- 'CA'
years <- 2010:2020
grades <- 2:5
org_level <- 'schools'

output_filename <- here::here(output_dir, 'Fresno.html')

# get all NY schools, so we can find out nces number
# ca_schools <- get_state_school_numbers(org_level, state_abb, max(years))

knit_dashboard(output_filename, org_level, nces_num, years, state_abb, grades)

for (i in seq.int(nces_num)) {

  output_file <- here::here(output_dir, paste0("Fresno-", names(nces_num)[i], ".html"))
  knit_dashboard(output_file, org_level, nces_num[i], years, state_abb, grades)

}
