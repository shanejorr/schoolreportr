#' District boundary and school plot
#'
#' Leaflet plot showing district bounadries as a shapefile and a point for the school.
#'
#' @keywords internal
leaflet_district_schools <- function(district_shapefile, school_information) {

  numeric_cols <- c('latitude', 'longitude', 'free_or_reduced_price_lunch', 'enrollment')

  # school coordinates
  school_information <- school_information %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~as.numeric(.))) %>%
    dplyr::mutate(enrollment = scales::comma(enrollment)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(4326, quiet = TRUE) %>%
    # create tooltip labels
    dplyr::mutate(tool_tip = glue::glue(
      "<strong>{school_name}</strong><br>
      {lea_name}<br>
      {street_location}<br>
      {city_location}, {state_location} {zip_location}"
    ))

  tooltip_labels <- as.list(school_information$tool_tip)

  # centroid of district, used to center view of map
  district_centroid <- sf::st_centroid(district_shapefile$geometry)

  # map
  district_shapefile %>%
    leaflet::leaflet() %>%
    leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = .3,
      layerId = 1
    ) %>%
    leaflet::addCircleMarkers(
      data = school_information,
      layerId = 2,
      radius = 6, stroke = FALSE, fillOpacity = 0.5,
      label = lapply(tooltip_labels, htmltools::HTML)
    )

}
