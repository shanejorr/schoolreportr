table_cities_in_single_district <- function(district_shapefile, year, state) {

  # outputs a table with the populations of all cities within a district
  # parameters:
  #   district_shapefile: shapefile from tigris of the district; can only be a single district
  #   year: year to get populatiosn for

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
