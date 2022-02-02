# functions applicable to all files

path_to_raw_data <- function() {
  raw_data_folder <- 'create-data/raw-data'
}

school_district_numbers <- function() {

  read_rds(here::here(path_to_raw_data(), 'school_district_ids.rds'))

}


district_names_re <- function() {

  "culbers|cleveland|san eli|mathis|sheldon|sinton"

}

state_fips_number <- function() {

  48

}
