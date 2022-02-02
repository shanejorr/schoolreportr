################################################################################
#
# Title: Pull education data for landscape analysis
# Author: Shane Orr
# Description: This file uses the Urban Inst education data API and TEA data to pull in
#   the education data needed for the landscape analysis and does some light cleaning.
#   It outputs rds files with the data.
#
# Date created: October 1, 2021
#
##################################################################################

library(tidyverse)
library(glue)
library(educationdata)
library(here)

source(here("create-data", "school-char-functions.R"))
source(here('global_functions.R'))

# parameters -----------------------------------

state_fips <- state_fips_number()

isd_names <- district_names_re()

  raw_data_folder <- path_to_raw_data()

grades_to_get <- 9:12

ccd_years_to_get <- 2000:2019

crdc_years_to_get <- c(2011, 2013, 2015, 2017)

edfacts_years_to_get <- 2010:2017

# this object is created in this file, so it will not exists if data has never been pulled
school_district_ids <- read_rds(glue("{raw_data_folder}/school_district_ids.rds"))

# get general information --------------------------

district_information <- get_education_data(
  level = "school-districts",
  source = "ccd",
  topic = "directory",
  filters = list(year = max(ccd_years_to_get),
                 fips = state_fips)
) %>%
  filter(str_detect(str_to_lower(lea_name), !!isd_names())) %>%
  mutate(across(everything(), ~change_case(.x))) %>%
  select(year, lea_name, city_location, county_name, leaid, number_of_schools:instructional_aides_fte,
         guidance_counselors_total_fte:school_counselors_fte)

project_lea_numbers <- school_district_ids$school_nces#unique(district_information$leaid)

school_information <- map_df(
  project_lea_numbers,
  ~get_education_data(
    level = 'schools',
    source = 'ccd',
    topic = 'directory',
    filters = list(
      year = max(ccd_years_to_get),
      leaid = .x,
      school_type = 1
      ),
    add_labels = TRUE
  )
) %>%
  # we only want high schools
  filter(highest_grade_offered == 12) %>%
  # we want the high school with the highest enrollment in the district
  group_by(leaid) %>%
  filter(enrollment == max(enrollment)) %>%
  ungroup() %>%
  mutate(across(everything(), ~change_case(.x))) %>%
  select(year:lea_name, street_location:zip_location, latitude, longitude, urban_centric_locale,
         school_type:highest_grade_offered, title_i_eligible, charter, magnet,
         teachers_fte:free_or_reduced_price_lunch, enrollment)

write_rds(district_information, glue("{raw_data_folder}/district_information.rds"))
write_rds(school_information, glue("{raw_data_folder}/school_information.rds"))

# school-level specific information --------------------------------

project_lea_numbers <- unique(district_information$leaid)

project_nces_num <- unique(school_information$ncessch)

school_district_ids <- list(
  district_lea = project_lea_numbers,
  school_nces = project_nces_num
)

# write out numbers, so we can use between files
write_rds(school_district_ids, glue("{raw_data_folder}/school_district_ids.rds"))

# some data sets only show ncessch number, not school names
# make a data frame with ncessch number and name for joining
school_name_nces_num <- school_information %>%
  select(ncessch, school_name)

# enrollment by race ---------------------------
by_race <- map_df(project_nces_num, function(nces_num) {

  get_education_data(
    level = 'schools',
    source = 'ccd',
    topic = 'enrollment',
    subtopic = list('race'),
    filters = list(
      year = ccd_years_to_get,
      grade = grades_to_get,
      ncessch = nces_num
    ),
    add_labels = TRUE
  )
})

by_race <- by_race %>%
  left_join(school_name_nces_num, by = 'ncessch') %>%
  select(year, ncessch, school_name, grade, race, enrollment)

write_rds(by_race, glue("{raw_data_folder}/enrollment_race.rds"))

#  enrollment by limited English proficiency ---------------------------
by_lep <- map_df(project_nces_num, function (nces_num) {

  get_education_data(
    level = "schools",
    source = "crdc",
    topic = "enrollment",
    subtopic = c("lep", "sex"),
    filters = list(
      year = crdc_years_to_get,
      ncessch = nces_num
    ),
    add_labels = TRUE
  )

}) %>%
  group_by(ncessch, year, lep) %>%
  summarize(enrollment = sum(enrollment_crdc, na.rm = T), .groups = 'drop') %>%
  mutate(
    lep = str_replace_all(lep, "^Students.*limited.*", "lep_limited"),
    lep = str_replace_all(lep, "^All.*", "total_students")
  ) %>%
  pivot_wider(id_cols = c('ncessch', 'year'), names_from = 'lep', values_from = 'enrollment') %>%
  mutate(perc_lep = lep_limited / total_students) %>%
  left_join(school_name_nces_num, by = 'ncessch')

write_rds(by_lep, glue("{raw_data_folder}/enrollment_lep.rds"))

# ACT / SAT participation -------------------
act_participation <-  map_df(project_nces_num, function (nces_num) {

  get_education_data(
    level = "schools",
    source = "crdc",
    topic = "sat-act-participation",
    subtopic = list("race", "sex"),
    filters = list(
      year = crdc_years_to_get,
      ncessch = nces_num
    ),
    add_labels = TRUE
  )

}) %>%
  clean_crdc('students_SAT_ACT') %>%
  rename(num_students_SAT_ACT = students_SAT_ACT)

# we want to also get crdc enrollment totals by race, so we can calculate percentage of takers by race.
race_enroll_crdc <-  map_df(school_district_ids$school_nces, function (nces_num) {

  get_education_data(
    level = "schools",
    source = "crdc",
    topic = "enrollment",
    subtopic = list("race", "sex"),
    filters = list(
      year = crdc_years_to_get,
      ncessch = nces_num
    ),
    add_labels = TRUE
  )

}) %>%
  clean_crdc('enrollment_crdc')

# combine total enrollment to SAT numbers
act_participation <- act_participation %>%
  left_join(race_enroll_crdc, by = c('ncessch','year','leaid','race')) %>%
  filter(enrollment_crdc != 0) %>%
  mutate(perc_act = num_students_SAT_ACT / enrollment_crdc) %>%
  left_join(school_name_nces_num, by = 'ncessch')

write_rds(act_participation, glue("{raw_data_folder}/act_participation.rds"))

# number of staff ------------------------
school_staff <- map_df(school_district_ids$school_nces, function (nces_num) {
  get_education_data(
    level = "schools",
    source = "crdc",
    topic = "teachers-staff",
    filters = list(
      year = crdc_years_to_get,
      ncessch = nces_num
    ),
    add_labels = TRUE
  )
}) %>%
  select(year, ncessch, teachers_fte_crdc, counselors_fte) %>%
  mutate(counselors_fte = ifelse(counselors_fte < 0, NA_real_, counselors_fte)) %>%
  left_join(school_name_nces_num, by = 'ncessch')

write_rds(school_staff, glue("{raw_data_folder}/school_staff.rds"))
