year <- 2021

a <- educationdata::get_education_data(
  level = "school-districts",
  source = "saipe",
  filters = list(year = 2019:2020)
)


library(rvest)

url <- 'https://urbaninstitute.github.io/education-data-package-r/'

# Read the HTML code from the website
webpage <- read_html(url)

# Using CSS selectors to scrap the table
# 'table.table' is the CSS selector for the table with class 'table'
table_data_html <- html_nodes(webpage,'table.table')

# Converting the HTML table to data frame
table_data <- html_table(table_data_html[[1]], fill = TRUE) |>
  dplyr::filter(
    .data$Level %in% c('schools', 'school-districts'),
    .dataTopic == 'directory'
  ) |>
  dplyr::distinct(Level, Source, `Years Available`)

# items to add --------------------------

library(educationdata)

df <- get_education_data(level = 'schools',
                         source = 'ccd',
                         topic = 'enrollment',
                         subtopic = list('race'),
                         filters = list(year = c(2010, 2011,2030),
                                        grade = 9,
                                        ncessch = '340606000122'),
                         add_labels = TRUE)

max_year <- max(schoolreportr::data_years_available('ccd'))

a <- educationdata::get_education_data(
  level = 'schools',
  source = "ccd",
  topic = "directory",
  filters = list(year = max_year, state_location = 'AR')
) |>
  dplyr::select(dplyr::all_of(c('ncessch', 'leaid', 'school_name', 'lea_name', 'city_location', 'state_location')))

# median income: census
# education attainment: census

# Chronic Absenteeism (Consistent attendance): CRCD

# discipline: CRCD
# days suspended: CRCDß

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
