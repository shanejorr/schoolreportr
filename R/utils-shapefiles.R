#' Identify cities in the district
#'
#' Get the cities in a school district and return a dataframe with the cities and their population.
#'
#' @keywords internal
table_cities_in_single_district <- function(district_shapefile, year, state) {

  # populations for all cities in the state
  state_city_pop_geo <- tidycensus::get_estimates(
    geography = 'place',
    product = 'population',
    year = year,
    state = state,
    geometry = TRUE,
    keep_geo_vars = FALSE
  ) %>%
    dplyr::filter(variable == 'POP')

  # centroid of city, if centroid is within district then city is in district
  city_centroid <- sf::st_centroid(state_city_pop_geo$geometry)

  # calculate whether centroid of city is within district shapefile
  city_rows_in_district <- sf::st_contains(district_shapefile$geometry[[1]], city_centroid, sparse = FALSE) %>%
    # get the row numbers of cities that have the centroid within the district
    which()

  # drop the geometry to make printing the table easier
  cities_in_district <- sf::st_drop_geometry(state_city_pop_geo)[city_rows_in_district, c("NAME", "value")]

  return(cities_in_district)

}

#' Census tracts in district
#'
#' Find the census tracts in the district and return a list containing the following elements:
#'   (1) vector of tract IDs that are in the district, (2) shapefile of tracts in the district.
#'
#' @keywords internal
census_tracts_in_district <- function(state, year, district_shapefile) {

  # texas block groups
  state_census_tracts_shapefile <- tigris::tracts(
    state = state,
    year = year
  )  %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(4326, quiet = TRUE)

  district_shapefile <- district_shapefile %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(4326, quiet = TRUE)

  # extract tracts that touch the district
  tracts_in_district <- calculate_tracts_in_district(district_shapefile, state_census_tracts_shapefile)

  tracts_geoid_in_district <- tracts_in_district$GEOID

  # only keep tracts within the district for county shappefile
  state_census_tracts_shapefile <- state_census_tracts_shapefile %>%
    dplyr::filter(GEOID %in% !!tracts_geoid_in_district)

  list(
    tracts_in_district = tracts_geoid_in_district,
    shapefiles_tracts_in_district = state_census_tracts_shapefile
  )
}

#' Helper function that finds tracts in district
#'
#' @keywords internal
calculate_tracts_in_district <- function(district_shapefile, state_census_tracts_shapefile) {

  # sample points from the district shapefile
  # we will use the points to find out which block_groups are in the district
  n_samples <- 500
  district_grid <- sf::st_sample(district_shapefile$geometry, size = n_samples)

  # find which block groups are in the school district
  # get integer number of block group from the state-wide block group shape file
  block_groups_in_district_int <- sf::st_intersects(state_census_tracts_shapefile, district_grid, sparse = FALSE) %>%
    {which(rowSums(.) > 0)}

  # extract only the block groups in the district
  state_census_tracts_shapefile[block_groups_in_district_int, ]

}
