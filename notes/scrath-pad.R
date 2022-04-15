# Scratch pad for testing

devtools::load_all()

ak_district <- 200001

state_abb <- 'AK'

years <- 2016:2017

state_assessment <- assessment_scores_by_race(state_abb,ak_district, years) %>%
  # make factor for plotting
  dplyr::mutate(year = as.factor(year)) %>%
  # clean up percentages for tool tips
  dplyr::mutate(
    dplyr::across(dplyr::contains('_pct_'),
                  ~paste0(round(.x, 0), "%"), .names = "clean_{.col}")
  )

# Math -----------------------------------

# all years, district and state

all_years_total <- state_assessment %>%
  dplyr::filter(race == 'Total')

plt_params <- list(
  sub = c('math_pct_pass', 'read_pct_pass'),
  title = c('Math State Assessment Scores', 'Reading State Assessment Scores'),

)

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

district %>%
  hc_plot_grouped_line(
    'year', glue::glue('math_pct_pass'), 'race', 'plt_title',
    NULL, 'Percent passing state assessment', y_percentage = TRUE
  ) %>%
  custom_hc_tooltip(create_html_tooltip('race', glue::glue('clean_math_pct_pass'),
                                        glue::glue('math_test_num_valid')))


# most current year, by race, district and state
