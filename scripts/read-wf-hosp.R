#filename: read-wf-hosp
#Oct 14, 2025: this script is just for wrangling
#the wildfire data, and then it will flow into the other script
#which includes the heat outcome data as well
library(here)
library(santoku)#alternatives to cut_number
library(tidyverse)
library(mapview)

# Overview-----
# Building upon the effect measure modification models from the paper,
#fit models where the ZCTA-specific RD is the outcome, and 
#Certain modifiable characteristics
#like AC and green space are the exposures.
#And then we control for confounders such as income or race/ethnicity.

#Conduct a meta-regression where more precise observations are weighted more heavily.

# Read in socio-demographic and other spatial data-------
## ZCTA pop, area, pop. density & RUCA--------
#bring in these data on ZCTA population, area, and pop. density

setwd(here("data-processed"))
load("lookup_pop_zcta.RData")

lookup_pop_zcta
#make the data wide form for the model, rather than long-form, 
#which I have done here
#climate-health/scripts/read-wrangle-eff-estimates.R

#and read in RUCA data as well
setwd(here("data-processed"))
load(file="zcta_ruca2010.RData")
zcta_ruca2010

## air basin and biome-----
#air-basin lookup
load(file="lookup_zcta_air_basin.RData")

#biome lookup
load("zcta_ca_biome_highest.RData")
lookup_zcta_ca_biome=zcta_ca_biome_highest %>% 
  distinct(zcta,BIOME_NAME) %>% 
  rename(biome_name=BIOME_NAME) %>% #I prefer lowercase
  mutate(
    biome_name_freq=fct_infreq(biome_name)
  )

lookup_zcta_ca_biome
table(lookup_zcta_ca_biome$biome_name_freq)

## Impervious surfaces---------
#Bring in data on impervious surfaces as well
setwd(here("data-input","hpi-data-direct-download"))
lookup_imperv=read_csv("imp-surface-2011.csv") %>% 
  rename(
    zcta = geoid,
    imperv_prop=value, #proportion impervious surfaces
  ) %>% 
  #get percentile (rank)
  mutate(imperv_rank=percent_rank(imperv_prop)) %>% 
  #just keep what we need for this
  dplyr::select(zcta, contains("imper"))

lookup_imperv

#Because the data Chen shared already includes some covariates,
#it will simplest to include all of the covariates here 




# Read wf outcome data and combine with other data------
setwd(here("data-input","data-shared-by-chen"))
outcome_wf=read_csv("hpi3_data_for_meta_figure6.csv") %>% 
  #Note these data include the effect estimates from above as well.
  #It appears the effect estimates above have about 100 more observations, though,
  #so perhaps keep that data above and then link with these data on the
  #distribution of the effect modifiers
  
  mutate(
    #Check definition of signal to noise artio:
    #signal to noise ratio: mean of rate differences divided by standard deviation
    snr_check=w_hat_mu/w_hat_sd,
    
    #indicator for above 0 or not
    rd_quo_gt_zero=case_when(
      is.na(w_hat_mu)~NA_integer_,
      w_hat_mu>0~1,
      w_hat_mu<0~0,
    ),
    
    #To more easily remember the name of the quo RD, create these variables
    #also to better remember that it's per 100,000 person-days throughout,
    #do this
    #May 22, 2025: to reduce length of variables, use more concise notation for quo
    #and target
    #elsewhere, I've used simply "0" for status quo and "1" for alternative
    #I've also used "quo" vs "alt"
    #I actually like that because 0 and 1 can get mixed up with other numbers
    rd_100k_quo_pt=w_hat_mu,
    rd_100k_quo_sd=w_hat_sd
    
  ) %>% 
  rename(
    
    #rename some vars to preferred style
    #air conditioning proportion 
    employed_prop=employed,
    above_poverty_prop=abovepoverty,
    edu_bach_prop =bachelorsed,
    edu_hs_prop=inhighschool,#I assume this is high-school educaiton prop
    ac_prop=AC,
    tree_canopy_prop=treecanopy,
    park_access_prop=parkaccess,
    car_prop=automobile,#proportion own automobile
    insured_prop=insured,
    income_per_capita=percapitaincome,
    race_white=white,
    race_black=black,
    race_asian=asian,
    race_latino=latino,
    race_native_am=NativeAm,
    race_pi=PacificIsl,#pacific islanders,
    pop_dens_10k=popden #not sure what per 10k means
  ) %>% 
  #remove these
  dplyr::select(
    -starts_with("overall"),
    -starts_with("w_"),
    -starts_with("FinalL"),#final lat and long
    #remove pop_dens_10k, as I'm not sure how it's defined
    -pop_dens_10k
  ) %>% 
  #bring in the data on zcta population and pop density per sq mile
  left_join(lookup_pop_zcta,by="zcta") %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  left_join(lookup_zcta_air_basin,by="zcta") %>% 
  left_join(lookup_zcta_ca_biome,by="zcta") %>% 
  left_join(lookup_imperv,by="zcta") %>% #Sep 18, 2025

  #Bring in heat data. For now, assume everything is an IRD. We can
  #adjust the naming later.
  #Hold off on the heat data. I'm going to do it differently.
  #  left_join(heat_kh_n_cases_diff_est,by="zcta") %>% #heat data as well
  mutate(
    outcome="wf", #if this becomes long-form, keep track of that
    
    #also create this variable in case I need to re-calculate the rate difference
    #in different summed combinations
    #ie not expressed per 100k
    rd_quo_pt=rd_100k_quo_pt/100000,#rate difference per 1 person-day
    rd_quo_sd=rd_100k_quo_sd/100000,#rate difference per 1 person-day
    
    #number of cases per day without respect to population
    n_cases_diff_quo_pt=rd_quo_pt*pop, 
    #number of cases per day without respect to population
    n_cases_diff_quo_sd=rd_quo_sd*pop, 
    
    #for the model, let's create weights as the inverse of the modeled SD
    #what's the smallest standard deviation?
    rd_quo_sd_min=min(rd_quo_sd,na.rm=T),
    #divide the minimum by each SD. Higher values are less precise.
    rd_quo_sd_min_prop=rd_quo_sd_min/rd_quo_sd,
    rd_quo_sd_inv=1/rd_quo_sd,
    rd_quo_weight=rd_quo_sd_min_prop, 
    
    #find outliers
    #no grouping needed as elsewhere, as the data are wide-form
    #Use lo and hi rather than lo and hi, as I may use other percentile
    #values elsewhere
    rd_quo_lo=quantile(rd_quo_pt,probs=c(0.05), na.rm=T),
    rd_quo_hi=quantile(rd_quo_pt,probs=c(0.95), na.rm=T),
    rd_quo_outlier=case_when(
      rd_quo_pt<rd_quo_lo~1,
      rd_quo_pt>rd_quo_hi~1,
      TRUE~0),
    
    #transform some vars for modeling
    pop_dens_mi2_sqrt=sqrt(pop_dens_mi2),
    tree_canopy_prop_sqrt=sqrt(tree_canopy_prop),
    tree_canopy_prop_log=log(tree_canopy_prop),
    
    #categorize poverty for stratified analyses & modeling
    above_poverty_prop_cat3=santoku::chop_equally(
      above_poverty_prop,groups=3),
    
    #categorize imp. surface and tree canpy as well
    tree_canopy_prop_cat3=santoku::chop_equally(
      tree_canopy_prop,groups=3),
    
    tree_canopy_prop_cat4=santoku::chop_equally(
      tree_canopy_prop,groups=4),
    tree_canopy_prop_cat5=santoku::chop_equally(
      tree_canopy_prop,groups=5),
    
    imperv_prop_cat3=santoku::chop_equally(
      imperv_prop,groups=3),
    imperv_prop_cat4=santoku::chop_equally(
      imperv_prop,groups=4),
    imperv_prop_cat5=santoku::chop_equally(
      imperv_prop,groups=5),
    
    #I also need to collapse the top two ruca categories
    
    
    
  ) %>% 
  dplyr::select(-est,-contains("SNR"),
                #there are a few dem. vars I'm not using. drop
                -contains("severe"),
                #also drop state. Dont need
                -contains("STATE")
                ) %>% 
  
  dplyr::select(
    zcta,
    starts_with("rd_"),
    starts_with("n_cases_diff"),
    everything()
  ) 

# Save-----
#save this so I don't have to run every time
setwd(here("data-processed"))
save(outcome_wf,file="outcome_wf.RData")

#a version with no outliers
outcome_wf_no_outliers=outcome_wf %>% 
  filter(rd_quo_outlier==0)

#outcome_wf_no_outliers %>% View()
#save for markdown
setwd(here("data-processed"))
save(outcome_wf_no_outliers,
     file="outcome_wf_no_outliers.RData")


#For future wrangling, I may need a list of just the covariatse (no outcome data)
#So pull those away
covars=outcome_wf %>% 
  dplyr::select(-contains("rd_"),-contains("n_cases"),-contains("outcome"))

#Brief checks -------
#More checks are here
#To make sure no infinities in RD as there are for heat
#Nope. None. good.
summary(outcome_wf$rd_100k_quo_pt)
outcome_wf %>% 
  ggplot(aes(x=rd_100k_quo_pt))+
  geom_histogram()

outcome_wf_no_outliers %>% 
  ggplot(aes(x=rd_100k_quo_pt))+
  geom_histogram()


#are there any with n cases prevented but no pop?
#No, good
# outcome_wf %>% 
#   filter(pop<2) %>% 
#   View()

#How does the categorization look?
outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat3) %>% 
  summarise(n=n())

outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat4) %>% 
  summarise(n=n())

outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat5) %>% 
  summarise(n=n())


outcome_wf_no_outliers %>% 
  group_by(tree_canopy_prop_cat3) %>% 
  summarise(n=n())
outcome_wf_no_outliers %>% 
  group_by(tree_canopy_prop_cat4) %>% 
  summarise(n=n())

#is there data in the cross-tabs?
outcome_wf_no_outliers %>% 
  group_by(tree_canopy_prop_cat3,ruca_cat3) %>% 
  summarise(n=n())

outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat3,ruca_cat3) %>% 
  summarise(n=n())

outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat4,ruca_cat3) %>% 
  summarise(n=n())


outcome_wf_no_outliers %>% 
  group_by(imperv_prop_cat3,biome_name_freq) %>% 
  summarise(n=n())

outcome_wf_no_outliers %>% 
  group_by(tree_canopy_prop_cat3,biome_name_freq) %>% 
  summarise(n=n())


