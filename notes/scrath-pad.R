# items to add --------------------------

# median income: census
# education attainment: census

# Chronic Absenteeism (Consistent attendance): CRCD

# discipline: CRCD
# days suspended: CRCD

# dual enrollment: CRCD

# census stuff ------------------

# vizualization of census tracts

district_centroid <- sf::st_centroid(district_shapefile$geometry)

district_shapefile_for_centroid <- district_shapefile$geometry

district_tracts_shapefiles <- tracts_in_district[["shapefiles_tracts_in_district"]]

census_data_by_tract <- census_demographics %>% filter(variable == "Black / African-American")

school_location <- school_directory

leaflet_census_tracts <- function(district_tracts_shapefiles, census_data_by_tract, district_shapefile_for_centroid, school_location) {

  labels <- glue::glue("<strong>{census_data_by_tract$NAME}</strong><br/>{census_data_by_tract$estimate} people (+/- {round(census_data_by_tract$moe, 0)})") %>%
    lapply(htmltools::HTML)

  district_tracts_shapefiles %>%
    left_join(census_data_by_tract, by = c("GEOID")) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(4326, quiet = TRUE) %>%
    leaflet::leaflet() %>%
    leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      layerId = 2,
      weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = .3,
      fillColor = ~leaflet::colorQuantile("Blues", estimate)(estimate),
      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      color = "#444444",
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
    leaflet::addCircleMarkers(
      data = school_location,
      lat = ~latitude,
      lng = ~longitude,
      layerId = 1,
      radius = 6, stroke = FALSE, fillOpacity = 0.5
    )

}

tracts_in_district[["shapefiles_tracts_in_district"]] %>%
  left_join(
    census_demographics %>% filter(variable == "Black / African-American"),
    by = c("GEOID")
  ) %>%
  leaflet::leaflet() %>%
  leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(
    weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = .3,

    fillColor = ~colorQuantile("Blues", estimate)(estimate),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    color = "#444444"
  )
