#' Identify cities in the district
#'
#' Get the cities in a school district and return a dataframe with the cities and their populations.
#'
#' @param district_shapefile The shapefile of the district. Can use `tigris::school_districts()`
#'      to get the shapefile.
#' @param state_abb The two letter state abbbreviation on the state that
#'      you want to show all schools for.
#'
#' @examples
#' \dontrun{
#' district_shapefile <- tigris::school_districts(state = "AR", cb = FALSE) |>
#'    dplyr::filter(GEOID == "0503060")
#'
#' sr_table_cities_in_district(district_shapefile, "AR")
#' }
#'
#' @returns A data frame with all the cities within a district's boundaries
#'      and each city's population.
#'
#' @export
sr_cities_in_district <- function(district_shapefile, state_abb) {

  # populations for all cities in the state
  state_city_pop_geo <- tidycensus::get_estimates(
    geography = 'place',
    product = 'population',
    state = state_abb,
    geometry = TRUE,
    keep_geo_vars = FALSE
  ) |>
    dplyr::filter(.data$variable == 'POP')

  # centroid of city, if centroid is within district then city is in district
  city_centroid <- sf::st_centroid(state_city_pop_geo$geometry)

  # calculate whether centroid of city is within district shapefile
  city_rows_in_district <- sf::st_contains(district_shapefile$geometry[[1]], city_centroid, sparse = FALSE) |>
    # get the row numbers of cities that have the centroid within the district
    which()

  # drop the geometry to make printing the table easier
  cities_in_district <- sf::st_drop_geometry(state_city_pop_geo)[city_rows_in_district, c("NAME", "value")]

  # clean up the table
  cities_in_district <- cities_in_district |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    dplyr::mutate(NAME = stringr::str_remove(.data$NAME, " city, .*")) |>
    dplyr::rename(city = .data$NAME, population = .data$value)

  return(cities_in_district)

}

#' Identify census tracts in a school district
#'
#' Find the census tracts in the district and return a list containing the following elements:
#'   (1) vector of tract IDs that are in the district, (2) shapefile of tracts in the district.
#'
#' @param district_shapefile The shapefile of the district. Can use `tigris::school_districts()`
#'      to get the shapefile.
#' @param state_abb The two letter state abbbreviation of the state in which
#'      the district is located.
#'
#' @examples
#' \dontrun{
#' state_abb <- 'AR'
#'
#' district_shapefile <- tigris::school_districts(state = state_abb, cb = FALSE) |>
#'    dplyr::filter(GEOID == "0503060")
#'
#' sr_census_tracts_in_district(district_shapefile, state_abb)
#' }
#'
#' @export
sr_census_tracts_in_district <- function(district_shapefile, state_abb) {

  # download shapefiles for all state census tracts
  state_census_tracts_shapefile <- tigris::tracts(state = state_abb)  |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(4326, quiet = TRUE)

  # clean up district shapefile so that it matches with state census tract shape files
  district_shapefile <- district_shapefile |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(4326, quiet = TRUE)

  # extract tracts that touch the district
  tracts_in_district <- sr_calculate_tracts_in_district(district_shapefile, state_census_tracts_shapefile)

  tracts_geoid_in_district <- tracts_in_district$GEOID

  # only keep census tracts within the district
  state_census_tracts_shapefile <- state_census_tracts_shapefile |>
    dplyr::filter(.data$GEOID %in% !!tracts_geoid_in_district)

  return(state_census_tracts_shapefile)

  # list(
  #   tracts_in_district = tracts_geoid_in_district,
  #   shapefiles_tracts_in_district = state_census_tracts_shapefile
  # )
}

#' Helper function that finds tracts in district
#'
#' @keywords internal
sr_calculate_tracts_in_district <- function(district_shapefile, state_census_tracts_shapefile) {

  # sample points from the district shapefile
  # we will use the points to find out which block_groups are in the district
  n_samples <- 500
  district_grid <- sf::st_sample(district_shapefile$geometry, size = n_samples)

  # find which block groups are in the school district
  # get integer number of block group from the state-wide block group shape file
  block_groups_in_district_int <- sf::st_intersects(state_census_tracts_shapefile, district_grid, sparse = FALSE)
  block_groups_in_district_int <- which(rowSums(block_groups_in_district_int) > 0)

  # extract only the block groups in the district
  state_census_tracts_shapefile[block_groups_in_district_int, ]

}
