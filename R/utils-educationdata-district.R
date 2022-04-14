#' Get state assessment data for a district
#'
#' Imports district data from Ed Facts using the Urban Institute's API. The data contains state assessment information.
#'
#' @param lea_number The school's LEAID number, as a string.
#' @param years The years from which you want to retrieve data. No data will be returned if data
#'      is not available for the year.
#' @param grade Grades to import. Defaults to all grades (99)
#'
#' @returns A list with each element containing a data frame with school data. The list is named,
#'      which identifies the data contained in the list element.
#'
#' @export
state_assessments_district <- function(lea_number, years, grade = 99) {

  # get each individual race and all races (99)
  race_to_use <- c(seq(1, 9), 99)

  educationdata::get_education_data(level = "school-districts",
                     source = "edfacts",
                     topic = "assessments",
                     filters = list(
                       leaid_num = lea_number,
                       year = 2018,#data_years_available('edfacts'),
                       grade_edfacts = grade
                      ),
                     subtopic = list("race"),
                     add_labels = TRUE
                    )
}

kansas_district <- 2000001


a <- state_assessments_district(kansas_district, c(2009, 2018))
