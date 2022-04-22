# Scratch pad for testing

nces_num <- c('050306001618', '050306000073') # two high schools in bentonville
state_abb <- 'AR'
years <- 2016:2017
grades <- 11:12
org_level <- 'schools'

output_filename <- 'bville.html'

# enrollment -------------------------------------------------------
library(schoolreportr)

knit_dashboard(output_filename, org_level, nces_num, years, state_abb, grades)
