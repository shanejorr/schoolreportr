# Data set that contains basic school information such as nces number, school name, district, grades
# used to lookup and search for schools

current_year <- 2019

school_directory <- get_ccd_directory(NULL, current_year)

school_directory <- school_directory %>%
  dplyr::select(ncessch, leaid, school_name, lea_name, city_location, state_location, contains('grade_offered'))

usethis::use_data(school_directory, compress = 'gzip')
