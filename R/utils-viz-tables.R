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

#' Table of cities
#'
#' Clean the table of cities in the district and convert it to a gt table
#' @keywords internal
cities_in_district <- function(district_cities) {

  district_cities %>%
    dplyr::mutate(
      NAME = stringr::str_remove(NAME, " city, .*"),
      value = scales::comma(value)
    ) %>%
    gt::gt() %>%
    gt::cols_label(
      NAME = gt::md("**City Name**"),
      value = gt::md(pop_colname)
    )

}

#' Formatting for Highcharts tooltip
#'
#' @keywords internal
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
#' @keywords internal
custom_hc_tooltip <- function(plt, tool_tip_html) {

  plt %>%
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
#' @keywords internal
add_title_axis_labels <- function(plt, plt_title, plt_x_label = 'School Year', plt_y_label) {

  plt %>%
    highcharter::hc_title(text = plt_title) %>%
    highcharter::hc_xAxis(title = list(text = plt_x_label)) %>%
    highcharter::hc_yAxis(title = list(text = plt_y_label))

}

#' Highcharts grouped line chart with percentages
#'
#' @keywords internal
hc_plot_grouped_line <- function(.data, x_col, y_col, group_col, plt_title,
                                 x_var_title, y_var_title, y_percentage = TRUE) {

  plt <- highcharter::hchart(.data, "line", highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]))  %>%
    highcharter::hc_title(text = plt_title) %>%
    highcharter::hc_xAxis(title = list(text = x_var_title)) %>%
    highcharter::hc_legend(enabled = TRUE)

  if (y_percentage) {

    plt <- plt %>%
      plt_hc_percentage(y_var_title)

  } else {

    plt <- plt %>% hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

#' Highcharts grouped bar chart
#'
#' @keywords internal
hc_plot_grouped_bar <- function(.data, x_col, y_col, group_col, x_order, y_title) {

  highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]),
    tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y:,.0f}%")
  ) %>%
    highcharter::hc_xAxis(title = list(text = NULL), categories = x_order) %>%
    plt_hc_percentage(y_title)

}

#' Plot state assessments
#'
#' @keywords internal
hc_plot_assessments <- function(.data, x_var, y_var, subject) {

  tool_tip <- paste0("<b>", subject, " % Proficient:</b> {point.y:,.0f}%")

  y_var_title <- paste0("Percentage Passing State ", subject, " Assessments")

  highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_var]], y = .data[[y_var]]),
    tooltip = list(pointFormat = tool_tip)
  ) %>%
    plt_hc_percentage(y_var_title) %>%
    highcharter::hc_xAxis(title = NULL)

}

#' Create percentages on axis labels
#'
#' @keywords internal
plt_hc_percentage <- function(plt, y_var_title) {

  # make the y-axis a percentage from 0 to 100%
  plt %>%
    highcharter::hc_yAxis(
      title = list(text = y_var_title),
      labels = list(format = '{value}%'),
      min = 0, max = 100
    )

}
