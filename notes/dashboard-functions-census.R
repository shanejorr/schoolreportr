# Functions for the school dashboard that relate to Census data and
# shape files related to Census data

calculate_percentages <- function(.data, total_variable_string) {

  .data %>%
    group_by(geography, variable) %>%
    summarize(num_demo = sum(estimate), .groups = 'drop_last') %>%
    mutate(
      total = max(num_demo),
      # multiple by 100 for plotting
      perc = (num_demo / total) * 100
    ) %>%
    ungroup() %>%
    filter(variable != total_variable_string)

}

district_blockgroups <- function(district_shapefile, tx_census_block_groups_shapefile) {

  # sample points from the district shapefile
  # we will use the points to find out which block_groups are in the district
  n_samples <- 500
  district_grid <- st_sample(district_shapefile$geometry, size = n_samples)

  # find which block groups are in the school district
  # get integer number of block group from the state-wide block group shape file
  block_groups_in_district_int <- st_intersects(tx_census_block_groups_shapefile, district_grid, sparse = FALSE) %>%
    {which(rowSums(.) > 0)}

  # extract only the block groups in the district
  tx_census_block_groups_shapefile[block_groups_in_district_int, ]

}

acs_district_state <- function(acs_variables, state_fips, tract_fips) {

  acs_geographies <- c('state', 'tract')

  map_df(
    acs_geographies,
    ~get_acs(
      geography = .x,
      variables = acs_variables,
      survey = 'acs5',
      state = "TX",
      moe_level = 95
    )
  ) %>%
    filter(GEOID %in% c(state_fips, tract_fips)) %>%
    mutate(geography = ifelse(str_detect(NAME, 'Tract'), 'District Average', 'State Average'))

}
