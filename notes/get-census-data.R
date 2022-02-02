# get census data from Tidycensus

library(tigris)
library(tidycensus)
library(here)
library(glue)
library(leaflet)
library(sf)
library(testthat)
library(tidyverse)

############ ALREADY IN FILE ###############
source(here('global_functions.R'))
source(here('create-dashboard', 'dashboard_functions.R'))
source(here('create-dashboard', 'dashboard-functions-census.R'))

district_leaid <- school_district_numbers()$district_lea[[1]]

year <- 2019

district_information <- read_rds(here(path_to_raw_data(), 'district_information.rds')) %>%
  filter(leaid == !!district_leaid)

district_shapefile <- tigris::school_districts(state = state_fips_number(), cb = FALSE, year = year) %>%
  filter(GEOID %in% !!district_leaid)

##################################

district_shapefile <- district_shapefile %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(4326, quiet = TRUE)

# sample points from the district shapefile
# we will use the points to find out which block_groups are in the district
n_samples <- 500
district_grid <- st_sample(district_shapefile$geometry, size = n_samples)

# import shapefiles for census block groups that are in the school district's county
tx_census_block_groups_shapefile <- counties(
  state = state_fips_number(),
  year = year
)  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(4326, quiet = TRUE)

# find which block groups are in the school district
# get integer number of block group from the state-wide block group shape file
block_groups_in_district_int <- st_intersects(tx_census_block_groups_shapefile, district_grid, sparse = FALSE) %>%
  {which(rowSums(.) > 0)}

# extract only the block groups in the district
block_groups_in_district <- tx_census_block_groups_shapefile[block_groups_in_district_int, ]

block_groups_geoid_in_district <- block_groups_in_district$GEOID

# centroid of district, used to center view of map
district_centroid <- st_centroid(district_shapefile$geometry)

district_grid_df <- map_dfr(district_grid, ~tibble(lng = .x[[1]], lat = .x[[2]]))

# census data -----------------------------------------------

acs_vars <- load_variables(2019, "acs5", cache = TRUE)

# race --------------------------

population_tables <- c(
  Total = 'B03002_001',
  White = 'B03002_003',
  Black = 'B03002_004',
  `Hispanic / Latinx` = 'B03002_012'
)

race_population <- get_acs(
  geography = "block group",
  variables = population_tables,
  survey = 'acs5',
  state = "TX",
)

group_cols <- c('GEOID', 'NAME')

race_percentages <- state_district_block_percentages(
  race_population, group_cols, 'Total', block_groups_geoid_in_district
  )

# education ------------------------------------------

education <- get_acs(
  geography = "block group",
  table = 'B15003',
  survey = 'acs5',
  state = "TX"
) %>%
  left_join(acs_vars[c('name', 'label')], by = c('variable' = 'name'))

education <- education %>%
  # use variable numbers to bin groups
  mutate(variable_number = str_extract(variable, "[0-9]{3}$") %>% as.numeric()) %>%
  mutate(education_level = case_when(
    variable_number == 1 ~ 'Population 25 and over',
    between(variable_number, 2, 18) ~ 'Below HS, HS diploma, or GED',
    between(variable_number, 19, 22) ~ "Some college or bachelor's degree",
    between(variable_number, 23, 25) ~ "Beyond bachelor's",
    TRUE ~ 'Failed to match'
  )) %>%
  # don't need these variables because we have descriptive labels for them
  select(-variable_number, -variable, -label) %>%
  #rename tpo match function that calculates percentages
  rename(variable = education_level)

education_percentages <- state_district_block_percentages(
  education, group_cols, 'Population 25 and over', block_groups_geoid_in_district
)

# poverty --------------------
# poverty rates for families with children under 18
poverty_rates <- c(
  total_pop = 'S0501_C01_103', # population
  below_100 = 'S0501_C01_104', # below 100% poverty line
  poverty_families_children = 'S0501_C01_108' # families with children under 18
)

poverty <- get_acs(
  geography = "block group",
  variables = poverty_rates,
  survey = 'acs5',
  state = "TX",
)

# median household income ----------------------------------

income_table <- 'S1903_C03_'

income_variable_stem <- c('001', '003', '009', '010')

income_variables <- str_c(income_table, income_variable_stem)

acs_vars_subject <- load_variables(2019, "acs5/subject", cache = TRUE)

median_income <- get_acs(
  geography = "tract",
  variables = income_variables,
  survey = 'acs5',
  state = "TX"
)

a <- median_income %>%
  filter(GEOID %in% tracts_geoid_in_district) %>%
  left_join(acs_vars_subject, by = c('variable'  = 'name'))
unique(a$label)
