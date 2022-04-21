# Scratch pad for testing

devtools::load_all()

ak_district <- '3701500'

state_abb <- 'NC'

grades <- 5:7

years <- 2016:2017

# enrollment -------------------------------------------------------
library(schoolreportr)

knit_dashboard('fresno.html', '0614550', 'CA', 2000:2020, 6:12)


schoolreportr:::data_years_available('ccd')


a <- a %>%
  distinct(ncessch)
