# Create datasets that contain state averages for education data
library(tidyverse)


devtools::load_all()

ccd_year <- 2019
crdc_year <- 2017
edfacts_year <- 2018

data(school_directory)

states <- school_directory %>%
  dplyr::select(ncessch, state_location)

rm(school_directory)

# group ncessch numbers by state so we can import by state
grouped_states <- states %>%
  group_by(state_location) %>%
  group_split(.keep = TRUE)

#crdc_enroll_race <- get_eddata_topic_subtopic(NULL, crdc_year, source = 'crdc', topic = "enrollment", subtopic = c("race", "sex")) %>%
#  clean_crdc('enrollment_crdc')

#readr::write_rds(state_enrol_race, 'crdc_enroll_race.rds')

crdc_enroll_race <- readr::read_rds('crdc_enroll_race.rds')

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

readr::write_rds(state_test_participation, 'state_test_part.rds')

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

readr::write_rds(state_lep, 'state_lep.rds')

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

readr::write_rds(state_assessments, 'state_assessments.rds')

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

readr::write_rds(enrollment_race, 'enrollment_race.rds')

usethis::use_data(state_test_participation, state_lep, state_assessments,enrollment_race, internal= TRUE, overwrite = FALSE)


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
