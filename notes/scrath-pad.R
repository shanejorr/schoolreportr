# Scratch pad for testing

devtools::load_all()

ak_district <- '3701500'

state_abb <- 'NC'

grades <- 5:7

years <- 2016:2017

# enrollment -------------------------------------------------------



# assessments ------------------------------------------------------

# all years, district and state

all_years_total <- state_assessment %>%
  dplyr::filter(race == 'Total')

subjects <- c('math', 'read')
plt_titles <- c('Math State Assessment Scores', 'Reading State Assessment Scores')

all_year_plts <- purrr::map2(subjects, plt_titles, function(.x, plt_title) {

  all_years_total %>%
    hc_plot_grouped_line(
      'year', glue::glue('{.x}_pct_pass'), 'geography', plt_title,
      NULL, 'Percent passing state assessment', y_percentage = TRUE
    ) %>%
    custom_hc_tooltip(create_html_tooltip('geography', glue::glue('clean_{.x}_pct_pass'),
                                          glue::glue('{.x}_test_num_valid')))

})

# all years, by race, district only
district <- state_assessment %>%
  dplyr::filter(!stringr::str_detect(geography, 'Total'),
                race != 'Total')

all_years_race_district <- purrr::map2(subjects, plt_titles, function(.x, plt_title) {

  district %>%
    hc_plot_grouped_line(
      'year', glue::glue('{.x}_pct_pass'), 'race', plt_title,
      NULL, 'Percent passing state assessment', y_percentage = TRUE
    ) %>%
    custom_hc_tooltip(create_html_tooltip('race', glue::glue('clean_{.x}_pct_pass'),
                                          glue::glue('{.x}_test_num_valid')))

})

# most current year, by race, district and state
# bar chart
all_years_race <- state_assessment %>%
  dplyr::filter(race != 'Total') %>%
  dplyr::arrange(race)



all_years_race_district <- purrr::map2(subjects, plt_titles, function(.x, plt_title) {

  hc_plot_grouped_bar(all_years_race, 'race', glue::glue('{.x}_pct_pass'), 'geography', plt_title)

})
