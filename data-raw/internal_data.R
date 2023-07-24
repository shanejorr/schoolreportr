# Create datasets that contain state averages for education data
library(tidyverse)

devtools::load_all()

# overall state average for state assessments by race
# get each individual race and all races (99)
race_to_use <- c(seq(1, 9), 99)

state_assessment <- educationdata::get_education_data(level = "school-districts",
                                  source = "edfacts",
                                  topic = "assessments",
                                  filters = list(
                                    fips = 10,
                                    year = 2018,#data_years_available('edfacts'),
                                    grade_edfacts = 99
                                  ),
                                  subtopic = list("race"),
                                  add_labels = TRUE
                        )

# state sat / act participation -----------------

state_test_participation <- tibble()

for (x in fips_states()){

  print(x)

  single_state <- get_crdc_school_test_participation_percentages(NULL, crdc_year, crdc_enroll_race, fips_state = x) %>%
    group_by(race) %>%
    summarize(
      num_sat_act_takers = sum(num_sat_act_takers, na.rm= TRUE),
      enrollment_crdc = sum(enrollment_crdc, na.rm= TRUE)
    ) %>%
    mutate(
      state_perc_takers = num_sat_act_takers / enrollment_crdc,
      state_fips = !!x
    )

  state_test_participation <- bind_rows(state_test_participation, single_state)

}

# lep status -------------------------------------------

state_lep <- tibble()

for (x in fips_states()){

  print(x)

  single_state <- get_crdc_school_enrollment_lep(NULL, crdc_year, fips_state = x) %>%
    summarize(
      lep_limited  = sum(lep_limited , na.rm= TRUE),
      total_students = sum(total_students, na.rm= TRUE)
    ) %>%
    mutate(
      perc_lep_state = lep_limited / total_students,
      state_fips = !!x
    )

  state_lep <- bind_rows(state_lep, single_state)

}

# state assessments ------------------------------------------------

state_assessments <- tibble()

for (x in fips_states()){

  print(x)

  single_state <- get_edfacts_state_assessments(NULL, edfacts_year, fips_state = x) %>%
    group_by(race) %>%
    mutate(
      read_test_num_passers = read_test_num_valid * read_test_pct_prof_midpt,
      math_test_num_passers = math_test_num_valid * math_test_pct_prof_midpt,
      read_test_num_valid = ifelse(is.na(read_test_num_passers), NA_real_, read_test_num_valid),
      math_test_num_valid = ifelse(is.na(math_test_num_passers), NA_real_, math_test_num_valid)
    ) %>%
    summarize(across(contains('_num_'), ~sum(.x, na.rm = TRUE))) %>%
    mutate(
      read_test_pct_prof_midpt = read_test_num_passers  / read_test_num_valid,
      math_test_pct_prof_midpt = math_test_num_passers  / math_test_num_valid,
      state_fips = !!x
    ) %>%
      ungroup()

  state_assessments <- bind_rows(state_assessments, single_state)

}

# enrollment by race------------------------------------------------

enrollment_race <- tibble()

for (x in fips_states()){

  print(x)

  single_state <- get_ccd_enrollment_race(NULL, ccd_year, fips_state = x) %>%
    group_by(race) %>%
    summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      total_enrollment = max(enrollment),
      perc_enrollment_state = enrollment / total_enrollment,
      state_fips = !!x
    )

  enrollment_race <- bind_rows(enrollment_race, single_state)

}

# get state fips codes ------------------

fips_codes <- tidycensus:::fips_codes |>
  dplyr::distinct(state, state_code)
  # dplyr::filter(.data$state == !!state_abbreviation) |>
  # dplyr::pull(.data$state_code) |>
  # unique() |>
  # as.numeric()

usethis::use_data(state_test_participation, state_lep, state_assessments,enrollment_race,fips_codes, internal= TRUE, overwrite = TRUE, compress = 'xz')


# graduation rates ------------------------------------------------

# grad_rates <- tibble()
#
# for (x in fips_states()){
#
#   print(x)
#
#   single_state <- get_edfacts_gradrates(NULL, edfacts_year, fips_state = x) %>%
#     group_by(race) %>%
#     mutate(
#       read_test_num_passers = read_test_num_valid * read_test_pct_prof_midpt,
#       math_test_num_passers = math_test_num_valid * math_test_pct_prof_midpt,
#       read_test_num_valid = ifelse(is.na(read_test_num_passers), NA_real_, read_test_num_valid),
#       math_test_num_valid = ifelse(is.na(math_test_num_passers), NA_real_, math_test_num_valid)
#     ) %>%
#     summarize(across(contains('_num_'), ~sum(.x, na.rm = TRUE))) %>%
#     mutate(
#       read_test_pct_prof_midpt = read_test_num_passers  / read_test_num_valid,
#       math_test_pct_prof_midpt = math_test_num_passers  / math_test_num_valid,
#       state_fips = !!x
#     ) %>%
#     ungroup()
#
#   state_assessments <- bind_rows(single_state)
#
#   usethis::use_data(state_assessments, internal= TRUE, overwrite = TRUE)
# }

# state IB / AP participation ----------------------------------

# state_ib_ab_participation <- tibble()
#
# for (x in fips_states()){
#
#   print(x)
#
#   single_statea <- get_crdc_school_ib_ap(NULL, crdc_year, crdc_enroll_race, fips_state = 1) %>%
#     group_by(race) %>%
#     summarize(
#       enrl_IB = sum(enrl_IB, na.rm= TRUE),
#       enrl_AP = sum(enrl_AP, na.rm= TRUE),
#       enrollment_crdc = sum(enrollment_crdc, na.rm= TRUE)
#     ) %>%
#     mutate(
#       state_perc_IB = enrl_IB / enrollment_crdc,
#       state_perc_IAP = enrl_AP / enrollment_crdc,
#       state_fips = !!x
#     )
#
#   state_ib_ab_participation <- bind_rows(state_ib_ab_participation, single_state)
#
#   usethis::use_data(state_ib_ab_participation, internal= TRUE, overwrite = TRUE)
# }
