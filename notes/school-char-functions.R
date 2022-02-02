# School characteristic function
#
# Description: Functions used in all files for the school characteristics sub-project
# Shane Orr
# October 4, 2021

download_excel_tea_data <- function(url, sheet_num = 3) {

  # downloads the TEA excel graduation sets and inputs them as data frames

  print(url)

  # download file into a temp file
  temp_filename <- tempfile()
  downloader::download(url, temp_filename, mode = 'wb')

  year <- str_extract(url, "20[0-9][0-9]") %>%
    as.numeric()

  # 2015 does not have a year in the link, so make NA 2015
  if (is.na(year)) year <- 2015

  readxl::read_excel(temp_filename, sheet = sheet_num, col_type = 'text') %>%
    mutate(year = !!year)
}

download_csv_graduation_data <- function(url) {

  # downloads the TEA csv graduation sets and inputs them as data frames

  print(url)

  year <- str_extract(url, "20[0-9][0-9]") %>%
    as.numeric()

  vroom::vroom(url, col_types = cols(.default = "c")) %>%
    mutate(year = !!year)

}

select_filter_grad_cols <- function(.data) {

  # columns for denominator
  grad_denominator_cols <- str_c(names(graduation_rate_cols_mapping()), "D")

  # column for graduation percentage
  grad_rate_cols <- str_c(names(graduation_rate_cols_mapping()), "R_GRAD")

  .data %>%
    filter(str_detect(str_to_lower(DISTNAME), !!isd_names)) %>%
    mutate(
      DISTNAME = str_to_title(DISTNAME),
      CAMPNAME = str_to_title(CAMPNAME)
    ) %>%
    select(year, school_name = CAMPNAME, district_name = DISTNAME,
           any_of(grad_denominator_cols), any_of(grad_rate_cols)) %>%
    # many years contain two rows for each school
    # one is for state accountability and one is for federal, they are both almost the same
    # keeping the first one will keep rows for federal accountability
    distinct(year, school_name, district_name, .keep_all = TRUE)

}

download_excel_enrolled_data <- function(url, year) {

  # downloads the TEA excel graduation sets and inputs them as data frames

  print(url)

  # download file into a temp file
  temp_filename <- tempfile()
  downloader::download(url, temp_filename, mode = 'wb')

  readxl::read_excel(temp_filename, skip = 9) %>%
    drop_na(Code) %>%
    filter(
      str_detect(str_to_lower(District), !!isd_names),
      Institution != 'Total high school graduates'
    ) %>%
    mutate(across(c(District, Name, Institution), ~change_case(.x))) %>%
    mutate(Institution = case_when(
      # combine all institution names into one category
      !Institution %in% c('Not Trackable', 'Not Found') ~ "Attending TX public institution",
      Institution == 'Not Trackable' ~ 'Unknown',
      Institution == 'Not Found' ~ 'Not attending TX public institution',
      TRUE ~ Institution
    ),
    Institution = str_remove(Institution, " [(].*[)]$")
    ) %>%
    mutate(year = !!year) %>%
    select(year, district_name = District, school_name = Name, institution_name = Institution, num_students = Students) %>%
    group_by(year, district_name, school_name, institution_name) %>%
    summarize(num_students = sum(num_students), .groups = 'drop') %>%
    # calculate percentage of total students
    group_by(year, district_name, school_name) %>%
    mutate(perc_total = num_students / sum(num_students, na.rm = T)) %>%
    ungroup()

}

graduation_rate_cols_mapping <- function() {

  c(
    CAMP_ALL = 'Total - Overall',
    CAMP_AA = 'Race - Black',
    CAMP_AS = 'Race - Asian',
    CAMP_HS = 'Race - Hispanic',
    CAMP_MU = 'Race - Two or more',
    CAMP_NA = 'Race - American Indian',
    CAMP_PI = 'Race - Pacific Islander',
    CAMP_WH = 'Race - White',
    CAMP_ECN = 'Economic Status - Economically Disadvantaged',
    CAMP_NECN = 'Economic Status - Not Economically Disadvantaged',
    CAMP_FEM = 'Gender - Female',
    CAMP_MAL = 'Gender - Male',
    CAMP_BE = 'ESL - Bilingual or ESL',
    CAMP_IMM = 'Immigration Status - Immigrant',
    CAMP_LEP = 'ELL - English Language Learner'
  )

}

change_case <- function(col) {

  # everything is in all uppercase
  # change case to title, with some slight tweaks

  col <- str_to_title(col)
  col <- str_replace_all(col, " H S$", " HS")
  col <- str_replace_all(col, " J H$", " JH")
  col <- str_replace_all(col, " El$", " Elementary")
  col <- str_replace_all(col, " Isd", " ISD")

}

clean_crdc <- function(.data, metric_colname) {

  .data %>%
    filter(
      sex == 'Total',
      disability == 'Total'
    ) %>%
    filter(race != 'Total') %>%
    select(ncessch, year, leaid, race, all_of(metric_colname))

}

get_dataset_from_list_name <- function(.data, list_name_re) {

  .data[str_detect(names(.data), list_name_re)]

}
