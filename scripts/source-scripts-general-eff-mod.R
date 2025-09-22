#Define the order in which to source the scripts
#For the project on effect modification of wildfire-hospitalization RD by A/C, green space, 
#and maybe other intervention variables

#Sep 22, 2025: renaming file to more general

library(here)

#The only files that need to be run are these

#Load data, create some new variables, combine together
source(here("scripts","data-mgmt-analyses-before-modeling-RD-as-outcome.R"))

#Fit models for effect of intervention variables on the rate difference
source(here("scripts","model-RD-as-outcome.R"))

#Define scenarios and predict alternative values of RD based on scenario values
source(here("scripts","predict-values-alt-tree-ac.R"))

