#' Import district shapefile
#'
#' Shapefiles for school boundaries do not exist. The closest we have are district boundary shapefiles.
#'
#' @keywords internal
shapefiles_district <- function(state, leaid_number, year) {

  message("Getting district boundary shapefile ...")



}


tigris::school_districts(state = 'TX', cb = FALSE, year = 2019)
