# functions for the files that create the dashboard

leaflet_district_schools <- function(district_shapefile, school_information, state_fips, year) {

  # create leaflet plot with district boundaries and all 9-12 schools

  # parameters:
  #   leaid_number: a single value for the LEAID number of the district to show boundaries for
  #   school_information: data frame with required school information and lat / long

  numeric_cols <- c('latitude', 'longitude', 'free_or_reduced_price_lunch', 'enrollment')

  # school coordinates
  school_information <- school_information %>%
    mutate(across(all_of(numeric_cols), ~as.numeric(.))) %>%
    mutate(perc_free_or_reduced = round(free_or_reduced_price_lunch / enrollment, 2) * 100,
           perc_free_or_reduced = glue("{perc_free_or_reduced}%"),
           enrollment = scales::comma(enrollment)) %>%
    # jitter schools so they dont overplot
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(4326, quiet = TRUE) %>%
    # create tooltip labels
    mutate(tool_tip = glue(
      "<strong>{school_name}</strong><br>
      {street_location}, {city_location} {zip_location}<br>
      <strong>Grades Offered:</strong> {lowest_grade_offered} - {highest_grade_offered}<br>
      <strong>Enrollment:</strong> {enrollment}<br>
      <strong>% Free or Reduced Lunch:</strong> {perc_free_or_reduced}<br>
      <strong># FTE Teachers:</strong> {teachers_fte}"
    ))
  print('conversion done')
  tooltip_labels <- as.list(school_information$tool_tip)

  # centroid of district, used to center view of map
  district_centroid <- sf::st_centroid(district_shapefile$geometry)

  # map
  district_shapefile %>%
    leaflet() %>%
    setView(lng = district_centroid[[1]][[1]], lat = district_centroid[[1]][[2]], zoom = 10) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = .3,
      layerId = 1
    ) %>%
    addCircleMarkers(
      data = school_information,
      layerId = 2,
      radius = 6, stroke = FALSE, fillOpacity = 0.5,
      label = lapply(tooltip_labels, htmltools::HTML)
    )

}

table_cities_in_single_district <- function(district_shapefile, year, state_fips) {

  # outputs a table with the populations of all cities within a district
  # parameters:
  #   district_shapefile: shapefile from tigris of the district; can only be a single district
  #   year: year to get populatiosn for

  # populations for all cities in the state
  state_city_pop_geo <- get_estimates(
    geography = 'place',
    product = 'population',
    year = year,
    state = state_fips,
    geometry = TRUE,
    keep_geo_vars = FALSE
  ) %>%
    filter(variable == 'POP')

  # centroid of city, if centroid is within district then city is in district
  city_centroid <- sf::st_centroid(state_city_pop_geo$geometry)

  # calculate whether centroid of city is within district shapefile
  city_rows_in_district <- sf::st_contains(district_shapefile$geometry[[1]], city_centroid, sparse = FALSE) %>%
    # get the row numbers of cities that have the centroid within the district
    which()

  # drop the geometry to make printing the table easier
  cities_in_district <- sf::st_drop_geometry(state_city_pop_geo)[city_rows_in_district, c("NAME", "value")]

  cities_in_district <- cities_in_district %>%
    arrange(desc(value)) %>%
    mutate(
      NAME = str_remove(NAME, " city, .*"),
      value = scales::comma(value)
    )

  return(cities_in_district)

}

filter_join_school_data <- function(school_data, school_information, ncessch_in_district) {

  # the data sets for schools (enrollment, graduation rates, etc) contains the ncessch number,
  # but not school names

  # this function filters these data sets to only include schools in the district
  # and joins the data set with school names to data set with school data
  # data sets are joined by ncessch and columns in each data set with this data need the name 'ncessch'

  school_data %>%
    filter(ncessch %in% !!ncessch_in_district) %>%
    left_join(school_information[c('ncessch', 'school_name')], by = 'ncessch') %>%
    select(ncessch, school_name, everything())

}

tooltip_factor_order <- function(.data, fct_colname, resort_colname) {

  .data %>%
    filter(year == max(year)) %>%
    arrange(desc(.data[[resort_colname]])) %>%
    select(any_of(fct_colname)) %>%
    .[[1]] %>%
    as.vector()
}

create_html_tooltip <- function(group_color, y_text, total_number) {

  str_c(
    '<tr><td style="padding:0"><span style="color:{point.color};font-weight:bold">{point.',
    group_color,
    '}</span>: {point.',
    y_text,
    '} ({point.',
    total_number,
    '} students)</tr></td><br>'
  )
}

custom_hc_tooltip <- function(plt, tool_tip_html) {

  plt %>%
    hc_tooltip(
      pointFormat = tool_tip_html,
      crosshairs = TRUE,
      backgroundColor = "#F0F0F0",
      shared = TRUE,
      borderWidth = 3,
      useHTML = TRUE
    )
}

hc_plot_grouped_perc <- function(.data, x_col, y_col, group_col, tool_tip_html, plt_title,
                                 x_var_title, y_var_title, y_percentage = TRUE, reverse_x = TRUE) {

  plt <- hchart(.data, "line", hcaes(x = .data[[x_col]], y = .data[[y_col]], group = .data[[group_col]]))  %>%
    custom_hc_tooltip(tool_tip_html) %>%
    hc_title(text = plt_title) %>%
    hc_xAxis(title = list(text = x_var_title), reversed = reverse_x) %>%
    hc_legend(enabled = TRUE)

  if (y_percentage) {

    plt <- plt %>%
      plt_hc_percentage(y_var_title)

  } else {

    plt <- plt %>% hc_yAxis(title = list(text = y_var_title))

  }

  return(plt)

}

plt_hc_percentage <- function(plt, y_var_title) {

  # make the y-axis a percentage from 0 to 100%
  plt %>%
    hc_yAxis(
      title = list(text = y_var_title),
      labels = list(format = '{value}%'),
      min = 0, max = 100
    )

}

create_school_demo_linechart <- function(.data, group_col, y_col, x_col, ...) {

  # this function does the data cleaning to create a line chart of demographics,
  # calls the function to create the chart

  # reorder demo factors so tooltip shows the demo in order from highest percent to lowest
  # for most recent year
  demo_order <- tooltip_factor_order(.data, group_col, y_col)

  .data %>%
    # reorder factor so tooltip shows the race in order from highest percent to lowest
    # for most recent year
    mutate(
      demo_col = fct_relevel(.data[[group_col]], demo_order),
      perc_plot = .data[[y_col]] * 100
    ) %>%
    arrange(desc(.data[[x_col]])) %>%
    hc_plot_grouped_perc(y_col = y_col, group_col = group_col, x_col = x_col, ...)
}

add_title_axis_labels <- function(plt, plt_title, plt_x_label = 'School Year', plt_y_label) {

  plt %>%
    hc_title(text = plt_title) %>%
    hc_xAxis(title = list(text = plt_x_label)) %>%
    hc_yAxis(title = list(text = plt_y_label))

}


plt_hc_barchart_percent <- function(.data, tooltip_list) {

  .data %>%
    hchart(
      'column',
      hcaes(x = variable, y = perc, group = geography),
      tooltip = list(valueDecimals = 0, valueSuffix = "%", shared = TRUE)
    ) %>%
    hc_xAxis(title = list(text = NULL))
}
