#Define the order in which to source the scripts
#For the project on effect modification of wildfire-hospitalization RD by A/C, green space, 
#and maybe other intervention variables

#Sep 22, 2025: renaming file to be more general
#Oct 23, 2025: updating with new scripts

#Note that the script for the wf effects must precede the heat script
#because the wf script includes the covariates

library(here)
source(here("scripts","read-wf-hosp.R"))
source(here("scripts","read-heat-hosp.R"))

#Fit models for effect of intervention variables on the rate difference
source(here("scripts","model-RD-as-outcome-final.R"))

#Define scenarios and predict alternative values of RD based on scenario values
source(here("scripts","predict-values-alt.r"))

#summarize
source(here("scripts","predict-values-alt-summary.r"))
