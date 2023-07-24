#' District boundary and school plot
#'
#' Leaflet plot showing district bounadries as a shapefile and a point for the school.
#'
#' @export
leaflet_district_schools <- function(district_shapefile, school_information) {

  numeric_cols <- c('latitude', 'longitude', 'free_or_reduced_price_lunch', 'enrollment')

  # school coordinates
  school_information <- school_information |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~as.numeric(.))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c('school_name', 'lea_name', 'street_location', 'city_location')),
                   ~stringr::str_to_title(.x))) |>
    dplyr::mutate(grade_range = glue::glue("{.data$lowest_grade_offered}-{.data$highest_grade_offered}")) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(4326, quiet = TRUE) |>
    # create tooltip labels
    dplyr::mutate(tool_tip = glue::glue(
      "<strong>{.data$school_name}</strong><br>
      {.data$lea_name}<br>
      Grades in school: {.data$grade_range}<br>
      School enrollment: {.data$enrollment}<br>
      {.data$street_location}<br>
      {.data$city_location}, {.data$state_location} {.data$zip_location}"
    ))

  tooltip_labels <- as.list(school_information$tool_tip)

  # centroid of district, used to center view of map
  district_centroid <- sf::st_centroid(district_shapefile$geometry)

  # map
  district_shapefile |>
    leaflet::leaflet() |>
    leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = .3,
      layerId = 1
    ) |>
    leaflet::addCircleMarkers(
      data = school_information,
      layerId = 2,
      radius = 6, stroke = FALSE, fillOpacity = 0.5,
      label = lapply(tooltip_labels, htmltools::HTML)
    )

}

#' Plot data by census tract using Leaflet
#'
#' Leaflet plot showing census tracts within a district a fill color for the tracts based on data included
#' the parameters.
#'
#' @param district_tracts_shapefiles A shape file  with the district tracts, must contain a 'GEOID' column.
#' @param census_data_by_tract Data frame with data by census tract taht we want to plot. Must contain
#'      a 'GEOID' column because this column is merged with the shapefile. There should only be one row per GEOID.
#' @param district_shapefile_for_centroid A shapefile for the school district, so we can compute the district's
#'      centriod. This centroid is used to center the map,.
#' @param school_location A data frame containing the latitude and longitude of the school/s. The columns
#'      for these two data points should be called 'latitude' and 'longitude'.
#' @param tool_tip_labels Tool tips to use, as HTML.
#' Example: `glue::glue("<strong>{census_data_by_tract$NAME}</strong> |> lapply(htmltools::HTML)`
#'
#' @export
leaflet_census_tracts <- function(district_tracts_shapefiles, census_data_by_tract, district_shapefile_for_centroid, school_location, tool_tip_labels) {

  # centroid of district, used to center view of map
  district_centroid <- sf::st_centroid(district_tracts_shapefiles$geometry)

  district_tracts_shapefiles |>
    dplyr::left_join(census_data_by_tract, by = c("GEOID")) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(4326, quiet = TRUE) |>
    leaflet::leaflet() |>
    leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      layerId = 2,
      weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = .3,
      fillColor = ~color_pal,
      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      color = "#444444",
      label = tool_tip_labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) |>
    leaflet::addCircleMarkers(
      data = school_location,
      lat = ~latitude,
      lng = ~longitude,
      layerId = 1,
      radius = 6, stroke = FALSE, fillOpacity = 0.5
    )

}

#' Table of cities
#'
#' Clean the table of cities in the district and convert it to a gt table
#'
#' @export
cities_in_district <- function(district_cities, pop_colname) {

  district_cities |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    dplyr::mutate(
      NAME = stringr::str_remove(.data$NAME, " city, .*"),
      value = scales::comma(.data$value)
    ) |>
    gt::gt() |>
    gt::cols_label(
      NAME = gt::md("**City Name**"),
      value = gt::md(.data$pop_colname)
    )

}

#' Formatting for Highcharts tooltip
#'
#' @export
create_html_tooltip <- function(group_color, y_text, total_number) {

  stringr::str_c(
    '<tr><td style="padding:0"><span style="color:{point.color};font-weight:bold">{point.',
    group_color,
    '}</span>: {point.',
    y_text,
    '} ({point.',
    total_number,
    '} students)</tr></td><br>'
  )
}

#' Create Highcharts tooltip
#'
#' @export
custom_hc_tooltip <- function(plt, tool_tip_html) {

  plt |>
    highcharter::hc_tooltip(
      pointFormat = tool_tip_html,
      crosshairs = TRUE,
      backgroundColor = "#F0F0F0",
      shared = TRUE,
      borderWidth = 3,
      useHTML = TRUE
    )
}

#' Axis labels for Highcharter line charts
#'
#' @export
add_title_axis_labels <- function(plt, plt_title, plt_x_label = 'School Year', plt_y_label) {

  plt |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = plt_x_label)) |>
    highcharter::hc_yAxis(title = list(text = plt_y_label))

}

#' Highcharts grouped line chart with percentages
#'
#' @export
hc_plot_grouped_line <- function(.data, x_col, y_col, group_col, plt_title,
                                 x_var_title, y_var_title, y_percentage = TRUE) {

  plt <- highcharter::hchart(.data, "line", highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]))  |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = x_var_title)) |>
    highcharter::hc_legend(enabled = TRUE) |>
    highcharter::hc_exporting(enabled = TRUE)

  if (y_percentage) {

    plt <- plt |>
      plt_hc_percentage(y_var_title)

  } else {

    plt <- plt |> highcharter::hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

#' Highcharts grouped bar chart
#'
#' @export
hc_plot_grouped_bar <- function(.data, x_col, y_col, group_col, y_title) {

  highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]),
    tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y:,.0f}%")
  ) |>
    highcharter::hc_xAxis(title = list(text = NULL)) |>
    plt_hc_percentage(y_title) |>
    highcharter::hc_exporting(enabled = TRUE)

}

#' Plot state assessments
#'
#' @export
hc_plot_assessments <- function(.data, x_var, y_var, subject) {

  tool_tip <- paste0("<b>", subject, " % Proficient:</b> {point.y:,.0f}%")

  y_var_title <- paste0("Percentage Passing State ", subject, " Assessments")

  highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_var]], y = .data[[y_var]]),
    tooltip = list(pointFormat = tool_tip)
  ) |>
    plt_hc_percentage(y_var_title) |>
    highcharter::hc_xAxis(title = NULL) |>
    highcharter::hc_exporting(enabled = TRUE)

}

#' Create percentages on axis labels
#'
#' @export
plt_hc_percentage <- function(plt, y_var_title) {

  # make the y-axis a percentage from 0 to 100%
  plt |>
    highcharter::hc_yAxis(
      title = list(text = y_var_title),
      labels = list(format = '{value}%'),
      min = 0, max = 100
    ) |>
    highcharter::hc_exporting(enabled = TRUE)

}
