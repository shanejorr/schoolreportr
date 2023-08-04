library(tidyverse)
devtools::load_all()

# median income -----------

## overall median income of state / district ---------------

# total median household income by census tract
household_income <- 'B25119_001'

# total household median income by census tract
total_median_income <- sr_demographic_data(household_income, state_abb, tracts_in_district$GEOID)

# filter to only keep total demographics by state and census tract
# census_demographics is variable created in Rmarkdown file
total_populations <- census_demographics |>
  filter(variable == 'Total') |>
  select(GEOID, population = estimate, geography)

# total - create weighted average population
total_income_with_pop <- total_median_income |>
  left_join(total_populations, by = c('GEOID', 'geography')) |>
  group_by(geography) |>
  mutate(
    total_geo_pop = sum(population, na.rm = TRUE),
    wgt_avg_pop = population / total_geo_pop,
    wgt_avg_income = estimate * wgt_avg_pop
  ) |>
  summarize(avg_median_income = sum(wgt_avg_income, na.rm = TRUE), .groups = 'drop')

# create visualizations
plt <- sr_plot_bar(
  total_income_with_pop,
  x_col = 'geography',
  y_col = 'avg_median_income',
  plt_title = 'District and State median income',
  y_var_title = 'Median income'
) |>
  highcharter::hc_yAxis(
    labels = list(
      formatter = highcharter::JS("function() { return '$' + Highcharts.numberFormat(this.value / 1000, 0) + 'k'; }")
    )
  )

# choropleth with tracts
median_income_by_tract <- total_median_income |>
  filter(stringr::str_detect(geography, '^District')) |>
  mutate(color_pal = leaflet::colorNumeric("Blues", .data$estimate)(.data$estimate))

labels <- glue::glue(
  "<strong>{median_income_by_tract$NAME}</strong><br/>
   <strong>{scales::dollar(median_income_by_tract$estimate, accuracy = 1)}</strong> (+/- {scales::dollar(median_income_by_tract$moe, accuracy = 1)})"
) |>
  lapply(htmltools::HTML)

sr_plot_choropleth_tracts(
  census_data_by_tract = median_income_by_tract,
  tract_shapefile = tracts_in_district,
  school_location = school_directory,
  tool_tip_labels = labels
)

## by race - weighted average income by race ------------
# B19013[A-I]_001 - example format
race_income_vars <- glue::glue("B19013{stringr::str_to_upper(letters[1:9])}_001")

# total household median income by census tract
total_median_income_race <- sr_demographic_data(race_income_vars, state_abb, tracts_in_district$GEOID)


