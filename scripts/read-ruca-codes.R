#Read ruca codes

#I had initially included this in this script,
#scripts/get-zcta-california-geo.R
#but I'm separating for clarity and so we don't need to run the geometry script
#when we just want the ruca data

library(tidyverse)
library(here)
library(readr)
library(readxl)

# Load rural urban commuting codes----
setwd(here("data-input","zcta-ruca"))
zcta_ruca2010=read_xlsx("RUCA2010zipcode.xlsx", sheet="Data") %>% 
  mutate(
    zcta=as.integer(ZIP_CODE),
    ruca_cat4=cut(RUCA1,breaks=c(0,3,6,9,10)),
    
    #collapse into three
    ruca_cat3=cut(RUCA1,breaks=c(0,3,6,10))
  ) %>% 
  dplyr::select(zcta,STATE,starts_with("ruca_cat"))

zcta_ruca2010
setwd(here("data-processed"))
save(zcta_ruca2010,file="zcta_ruca2010.RData")

zcta_ruca2010 %>% 
  group_by(ruca_cat3) %>% 
  summarise(n=n())
zcta_ruca2010 %>% 
  group_by(ruca_cat4) %>% 
  summarise(n=n())