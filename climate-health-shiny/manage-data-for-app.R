#Manage data for shiny app

#For the general project-----
#analogous to heat-hosp-california/shiny/tmap-static/manage-data-for-app.R
#I need to move the data around a little bit so it's in a sub-folder

## Load data----
#Here's the dataset that I use to create the histograms.
#Note that here is relative to the project
#April 22, 2025: update to omit the race variables. Just use the SES
#May 26, 2025: lighten this up. See old versions as needed
#variables
library(here)
library(tidyverse)
setwd(here("data-processed"))

load("zcta_ca_geo_simplified.RData")

load("scenario_compare_full_join_poverty_ac.RData")



#May 26, 2025 updated data for tree & ac analysis
#where we control for confounding
setwd(here("data-processed"))
load("tree_ac_summary_overall_pt_ci.RData")
load("tree_ac_summary_by_zcta_for_map.RData")




## save data------
#save it in a sub-folder that the shiny app can access
#to lighten up r-shiny code, save this
setwd(here("climate-health-shiny","data"))
save(zcta_ca_geo_simplified,file="zcta_ca_geo_simplified.RData")

save(scenario_compare_full_join_poverty_ac,
     file="scenario_compare_full_join_poverty_ac.RData")


#May 26, 2025 update
setwd(here("climate-health-shiny","data"))
save(tree_ac_summary_overall_pt_ci,
     file="tree_ac_summary_overall_pt_ci.RData")
save(tree_ac_summary_by_zcta_for_map,
     file="tree_ac_summary_by_zcta_for_map.RData")

# For Maren's app-----
# April 11, 2025
#adding data for Maren's shiny app as well.
setwd(here("data-processed","data-processed-heat-hosp-green-hia"))
load("irr_long.RData")
load("ird_long.RData")
load("n_cases_diff_long.RData")
load("measures_wide_by_scenario_zcta.RData")
load("measures_wide_by_scenario_zcta_for_maps.RData")


#save to the data folder for the shiny apps
setwd(here("climate-health-shiny","data","heat-hosp-green-hia"))
save(irr_long, file="irr_long.RData")
save(ird_long, file="ird_long.RData")
save(n_cases_diff_long, file="n_cases_diff_long.RData")
save(measures_wide_by_scenario_zcta,
     file="measures_wide_by_scenario_zcta.RData")
save(
  measures_wide_by_scenario_zcta_for_maps,
  file="measures_wide_by_scenario_zcta_for_maps.RData"
)


