# Scratch pad for testing

devtools::load_all()

ak_district <- '0200001'

state_abb <- 'AK'

grades <- 5:7

years <- 2016:2017

enroll <- get_district_enrollment(ak_district, years, grades)

directory <- get_district_directory(ak_district, max(years))
colnames(directory)
district_poverty <- educationdata::get_education_data(
  level = "school-districts",
  source = "saipe",
  filters = list(year = 2013, leaid = '0200001')
)

schools <- get_schools_in_district(ak_district, max(years), grades)

a <- schools %>%
  dplyr::select(ends_with('offered')) %>%
  dplyr::mutate(in_grade = dplyr::case_when(
    lowest_grade_offered %in% grades ~ TRUE,
    highest_grade_offered %in% grades ~ TRUE,
    dplyr::between(lowest_grade_offered, min(grades), max(grades)) ~ TRUE,
    dplyr::between(highest_grade_offered, min(grades), max(grades)) ~ TRUE,
    TRUE ~ FALSE
  ))
a
