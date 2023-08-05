library(tidyverse)
# devtools::load_all()

# median income -----------

# sr_helper_wgt_avg_median_income <- function(census_income, census_population) {
#
#   # rename variables so they are different than the variables in the income dataset
#   census_income_joined <- census_income |> dppyr::select(GEOID, population = estimate, geography)
#
# }

## get income data -----------------

# total median income by census tract and median income by race are in different variables

# total median household income by census tract
household_income <- 'B25119_001'

total_median_income <- sr_demographic_data(household_income, state_abb, tracts_in_district$GEOID)

# median income by race and census tract
# don't need A because it is white alone;
# we want H, which is white alone, not hispanic or latino
race_income_vars <- glue::glue("B19013{stringr::str_to_upper(letters[2:9])}_001")

total_median_income_race <- sr_demographic_data(race_income_vars, state_abb, tracts_in_district$GEOID)

sr_helper_wgt_avg_median_income_race <- function(total_median_income, total_median_income_race) {

  # combine total and by race and add descriptive variable
  total_race_median_income <- total_median_income |>
    bind_rows(total_median_income_race)

  # reword racial categories
  total_race_median_income <- total_race_median_income |>
    mutate(variable = case_match(
      variable,
      'B25119_001' ~ 'Total',
      'B19013B_001' ~ 'Black / African-American',
      'B19013I_001' ~ 'Hispanic / Latinx',
      'B19013H_001' ~ 'White',
      'B19013C_001' ~ 'American Indian or Alaska Native',
      'B19013D_001' ~ 'Asian',
      'B19013E_001' ~ 'Native Hawaiian or other Pacific Islander',
      'B19013G_001' ~ 'Two or more races',
      'B19013F_001' ~ 'Some Other'
    ))

  # everything should have matched
  if (any(is.na(total_race_median_income$variable))) stop("Failed to properly match racial categories to median income census data", call. = FALSE)

  # calculate weighted average median income by race for district

  if (nrow(total_race_median_income) != nrow(census_demographics)) {
    stop('Census population by race census tract data and median income by race census tract data should have same number of rows', call. = FALSE)
  }

  # State and overall district median income, by race
  # for bar chart
  total_race_median_income |>
    drop_na(estimate) |>
    # join data set with population
    left_join(
      census_demographics |> select(GEOID, variable, population = estimate, geography),
      by = c('GEOID', 'variable', 'geography'),
      relationship = 'one-to-one'
    ) |>
    # calculate weighted average median income
    group_by(geography, variable) |>
    mutate(
      total_geo_pop = sum(population, na.rm = TRUE),
      wgt_avg_pop = population / total_geo_pop,
      wgt_avg_income = estimate * wgt_avg_pop
    ) |>
    summarize(avg_median_income = sum(wgt_avg_income, na.rm = TRUE), .groups = 'drop') |>
    mutate(variable = fct_relevel(variable, c('Total','Black / African-American', 'Hispanic / Latinx', 'White')))

}


total_race_median_income <- sr_helper_wgt_avg_median_income_race(total_median_income, total_median_income_race)

sr_plot_grouped_bar(
  total_race_median_income,
  x_col = 'variable',
  y_col = 'avg_median_income',
  group_col = 'geography',
  plt_title = 'District and State median income',
  y_var_title = 'Median income'
) |>
  highcharter::hc_yAxis(
    labels = list(
      formatter = highcharter::JS("function() { return '$' + Highcharts.numberFormat(this.value / 1000, 0) + 'k'; }")
    )
  )

# total by census tract choropleth

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




