# create dashboards
library(tidyverse)
library(schoolreportr)

output_dir <- here::here('notes', 'school-dashboards')

years <- 2010:2020
org_level <- 'schools'

# Fresno ---------------------
nces_num <- c(heaton = '061455001735', williams = '061455011895') # Heaton and Williams
state_abb <- 'CA'
grades <- 2:5

output_filename <- here::here(output_dir, 'Fresno.html')

# get all NY schools, so we can find out nces number
# ca_schools <- get_state_school_numbers(org_level, state_abb, max(years))

knit_dashboard(output_filename, org_level, nces_num[1], years, state_abb, grades)

for (i in seq.int(nces_num)) {

  output_file <- here::here(output_dir, paste0("Fresno-", names(nces_num)[i], ".html"))
  knit_dashboard(output_file, org_level, nces_num[i], years, state_abb, grades)

}

# Rochester and Cleveland ----------------------

grades <- 6:8

# get district leaids so we can find al lschools
# ny_districts <- get_state_school_numbers('school-districts', 'OH', 2020)

roch_leaid <- '3624750'
cleveland_leaid <- '3904378'

# Rochester
roch_schools <- get_state_school_numbers('schools', 'NY', 2020) %>%
  filter(leaid == !!roch_leaid, enrollment > 100) %>%
  schoolreportr:::identify_school_grades(grades) %>%
  filter(in_grade) %>%
  pull(ncessch)

knit_dashboard(here::here(output_dir, 'Rochester-6_8.html'), org_level, roch_schools, years, 'NY', grades)

# Cleveland
clev_schools <- get_state_school_numbers('schools', 'OH', 2020) %>%
  filter(leaid == !!cleveland_leaid, enrollment > 100) %>%
  schoolreportr:::identify_school_grades(grades) %>%
  filter(in_grade) %>%
  pull(ncessch)

nces_num <- clev_schools
state_abb <- 'OH'

knit_dashboard(here::here(output_dir, 'Cleveland-6_8.html'), org_level, clev_schools, years, 'OH', grades)
