#' District boundary and school plot
#'
#' Leaflet plot showing district boundaries as a shapefile and a point for schools.
#' The tooltip over teh school contains school infomration.
#'
#' @param district_shapefile The shapefile of the district. Can use `tigris::school_districts()`
#'      to get the shapefile.
#' @param school_information A data frame containing information about the school, including
#'    latitdue and longitude location data. Can get this information from `sr_ccd_directory()`
#'
#' @examples
#' \dontrun{
#' district_shapefile <- tigris::school_districts(state = "AR", cb = FALSE) |>
#'    dplyr::filter(GEOID == "0503060")
#'
#' school_directory <- sr_ccd_directory('schools', "050306000073", 2021)
#'
#' sr_viz_district_boundary(district_shapefile, school_directory)
#' }
#'
#'
#' @returns A leaflet plot with the district boundaries and the school locations.
#'
#' @export
sr_viz_district_boundary <- function(district_shapefile, school_information) {

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

#' Choropleth map of census tracts and racial brakdowns
#'
#' Helper function that plots a choropleth map of the percentage of people of a given
#' race living in each census tract within a district. Uses `sr_plot_choropleth_tracts`
#' to create the plot, but creates and adds the tool tip and filters the data
#' for the racial category that we want to plot.
#'
#' @param .data Data set of racial totals and percentages by census tract.
#'      Data set created with `sr_rmd_census_tracts_race`. This does not contain
#'      shapefile data.
#' @param race String representing racial group we want to plot. This will come from
#'      the `variable` column in `.data`.
#' @param ... Parameters for `sr_plot_choropleth_tracts`.
#'
#' @returns A leaflet choropleth map of census tracts, with the percentage of
#'      people in a given census tract belonging to a given racial identity.
#'
#' @export
sr_rmd_plot_choropleth_tracts_race <- function(.data, race, ...) {

  specific_race_population <- .data |> dplyr::filter(.data$variable == !!race)

  labels <- glue::glue(
    "<strong>{race} Population</strong><br/>
   <strong>{specific_race_population$estimate}</strong> {race} residents (+/- {specific_race_population$moe})<br/>
   <strong>{specific_race_population$perc_estimate}</strong> {race} population (+/- {specific_race_population$perc_moe})"
  ) |>
    lapply(htmltools::HTML)

  sr_plot_choropleth_tracts(census_data_by_tract = specific_race_population, tool_tip_labels = labels, ...)

}

#' Choropleth maps by census tract using leaflet
#'
#' Leaflet plot showing census tracts within a district and a fill color for the tracts based on data included
#' in the parameters.
#'
#' @param census_data_by_tract Data frame with data by census tract that we want to plot. Must contain
#'      a 'GEOID' column because this column is merged with the census tract shapefile (`tract_shapefile`). There should only be one row per GEOID.
#' @param tract_shapefile Shapefiles for the same census tracts as `census_data_by_tract`.
#'      Can be imported with `[sr_census_tracts_in_district()]`. Can also use `[tigris::tracts()]`. Can also use
#' @param school_location A data frame containing the latitude and longitude of the school/s. The columns
#'      for these two data points should be called 'latitude' and 'longitude'. This information
#'      can be retreived from CCD school directories, which can be imported with [`sr_ccd_directory()`].
#' @param tool_tip_labels Tool tips to use, as HTML. Example,  which only shows the census tract name:
#'      `glue::glue("<strong>{census_data_by_tract$NAME}</strong> |> lapply(htmltools::HTML)`
#'
#' @returns A leaflet choropleth map of census tracts.
#'
#' @export
sr_plot_choropleth_tracts <- function(census_data_by_tract, tract_shapefile, school_location, tool_tip_labels) {

  tract_shapefile |>
    dplyr::left_join(census_data_by_tract, by = c("GEOID")) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(4326, quiet = TRUE) |>
    leaflet::leaflet() |>
    # leaflet::setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) |>
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

#' Formatting for Highcharts tooltip
#'
#' @keywords internal
sr_plot_format_html_tooltip <- function(group_color, y_text, total_number) {

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

#' Create Highcharts tooltip for plots
#'
#' Creates tool tips in a style that works for `schoolreportr` plots. May not work for other plots.
#' Works best for grouped line charts.
#'
#' @param plt Plot in which to add tool tip.
#' @param group_color Column name, as a string, that represents a grouping variable.
#' @param y_text The column name, as a string, of the y-axis variable.
#' @param total_number The column name, as a string of a column containing a number
#'      we want to add to the tool tip.
#'
#' @export
sr_plot_create_tooltip <- function(plt, group_color, y_text, total_number) {

  tool_tip_html <- sr_plot_format_html_tooltip(group_color, y_text, total_number)

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
#' @keywords internal
sr_plot_add_title_axis_labels <- function(plt, plt_title, plt_x_label = 'School Year', plt_y_label) {

  plt |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = plt_x_label)) |>
    highcharter::hc_yAxis(title = list(text = plt_y_label))

}

#' Grouped line chart
#'
#' Produces a highcharts grouped line chart. Since the data, axis, and grouping
#' variables are parameters in the function, this function will work with any data.
#'
#' @param .data The data frame to plot.
#' @param x_col The column name, as a string, of the plot's x-axis. Generally, this
#'      will be a time variable such as years.
#' @param y_col The column name, as a string, of the y-axis. If this column is a percentage,
#'      it should be as a whole number (78.8) and not a decimal (.788).
#' @param group_col The column name, as a string, of the grouping column. The plot
#'      will produce different lines for each group in this column.
#' @param plt_title The plot title, as a string.
#' @param x_var_title The x-axis title, as a string.
#' @param y_var_title The y-axis title, as a string.
#' @param y_percentage Boolean, signifying whether the y-axis is a percentage.
#'      Defaults to `FALSE`.
#'
#' @returns A highcharts line plot
#'
#' @export
sr_plot_grouped_line <- function(.data, x_col, y_col, group_col, plt_title = NULL,
                                 x_var_title = NULL, y_var_title = NULL, y_percentage = FALSE) {

  plt <- highcharter::hchart(.data, "line", highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]))  |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = x_var_title)) |>
    highcharter::hc_legend(enabled = TRUE) |>
    highcharter::hc_exporting(enabled = TRUE)

  if (y_percentage) {

    plt <- plt |>
      sr_plot_percentage(y_var_title)

  } else {

    plt <- plt |> highcharter::hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

#' Bar chart
#'
#' Produces a highcharts bar chart. Since the data, axis, and grouping
#' variables are parameters in the function, this function will work with any data.
#' Use [`sr_plot_grouped_bar()`] for grouped bar chart.
#'
#' @param .data The data frame to plot.
#' @param x_col The column name, as a string, of the plot's x-axis. Generally, this
#'      will be a time variable such as years.
#' @param y_col The column name, as a string, of the y-axis. If this column is a percentage,
#'      it should be as a whole number (78.8) and not a decimal (.788).
#' @param plt_title The plot title, as a string.
#' @param x_var_title The x-axis title, as a string.
#' @param y_var_title The y-axis title, as a string.
#' @param y_percentage Boolean, signifying whether the y-axis is a percentage.
#'      Defaults to `FALSE`.
#'
#' @export
sr_plot_bar <- function(.data, x_col, y_col, plt_title = NULL,
                                x_var_title = NULL, y_var_title = NULL, y_percentage = FALSE) {

  if (y_percentage) {
    tooltip_format <- "{point.y:,.0f}%"
  } else {
    tooltip_format <- "{point.y:,.0f}"
  }

  plt <- highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]]),
    tooltip = list(pointFormat = tooltip_format)
  ) |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = x_var_title)) |>
    highcharter::hc_exporting(enabled = TRUE)

  if (y_percentage) {

    plt <- plt |>
      sr_plot_percentage(y_var_title)

  } else {

    plt <- plt |> highcharter::hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

#' Grouped bar chart
#'
#' Produces a highcharts grouped bar chart. Since the data, axis, and grouping
#' variables are parameters in the function, this function will work with any data.
#'
#' @param .data The data frame to plot.
#' @param x_col The column name, as a string, of the plot's x-axis. Generally, this
#'      will be a time variable such as years.
#' @param y_col The column name, as a string, of the y-axis. If this column is a percentage,
#'      it should be as a whole number (78.8) and not a decimal (.788).
#' @param group_col The column name, as a string, of the grouping column. The plot
#'      will produce different lines for each group in this column.
#' @param plt_title The plot title, as a string.
#' @param x_var_title The x-axis title, as a string.
#' @param y_var_title The y-axis title, as a string.
#' @param y_percentage Boolean, signifying whether the y-axis is a percentage.
#'      Defaults to `FALSE`.
#'
#' @export
sr_plot_grouped_bar <- function(.data, x_col, y_col, group_col, plt_title = NULL,
                                x_var_title = NULL, y_var_title = NULL, y_percentage = FALSE) {

  if (y_percentage) {
    tooltip_format <- "<b>{series.name}:</b> {point.y:,.0f}%"
  } else {
    tooltip_format <- "<b>{series.name}:</b> {point.y:,.0f}"
  }

  plt <- highcharter::hchart(
    .data, "column",
    highcharter::hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]),
    tooltip = list(pointFormat = tooltip_format)
  ) |>
    highcharter::hc_title(text = plt_title) |>
    highcharter::hc_xAxis(title = list(text = x_var_title)) |>
    highcharter::hc_exporting(enabled = TRUE)

  if (y_percentage) {

    plt <- plt |>
      sr_plot_percentage(y_var_title)

  } else {

    plt <- plt |> highcharter::hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

#' Create percentages on axis labels
#'
#' @keywords internal
sr_plot_percentage <- function(plt, y_var_title) {

  # make the y-axis a percentage from 0 to 100%
  plt |>
    highcharter::hc_yAxis(
      title = list(text = y_var_title),
      labels = list(format = '{value}%'),
      min = 0, max = 100
    )

}

#' Plot total enrollment by year
#'
#' Create a highcharts line plot that shows total enrollment in all schools by year.
#' This function has little use outside the Rmarkdown file as it relies on the
#' specific data in the Rmarkdown file.
#'
#' @param .data Data frame containing enrollment data. This data frames comes from
#'      `sr_ccd_enrollment` and is then filtered to only contain total enrollment.
#' @param school_names A string containing the school name(s)
#' @param grade_span A string containing which grades are included (example: `"10-12"`).
#'      This information is used in the plot title.
#'
#' @examples
#' \dontrun{
#' total_enrollment <- sr_ccd_enrollment("schools", "050306000073", 2017:2020, 10:12) |>
#'    dplyr::filter(race == 'Total')
#'
#' sr_plot_total_enrollment_by_year(total_enrollment, "Bentonville High School", "10-12")
#' }
#'
#' @returns A highcharts line plot.
#'
#' @export
sr_plot_total_enrollment_by_year <- function(.data, school_names, grade_span) {

  sr_check_required_cols(.data, c('school_year_both', 'enrollment'))

  school_year_x_label <- 'School Year'

  plt_tooltip <- paste0("<b>", school_names, ":</b>  {point.y:.0f} students")

  highcharter::hchart(
    .data, "line",
    highcharter::hcaes(x = .data$school_year_both, y = .data$enrollment),
    tooltip = list(pointFormat = plt_tooltip)
  )  |>
    sr_plot_add_title_axis_labels(
      plt_title = glue::glue("{school_names}\nStudent Enrollment by School Year (grades {grade_span})"),
      plt_x_label = school_year_x_label,
      plt_y_label = glue::glue("Enrollment in grades {grade_span}")
    )  |>
    highcharter::hc_exporting(enabled = TRUE)

}
