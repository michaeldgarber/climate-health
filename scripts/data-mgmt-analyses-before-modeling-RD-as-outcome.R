#Filenmae: data-mgmt-analyses-before-modeling-RD-as-outcome.R

#Revised May 16, 2025
#Revised May 21, 2025
#Revised Sep 18, 2025 to add imp. surface data
# Revised Sep 19 to use imp. surface data directly from HPI
library(santoku)#alternatives to cut_number
library(here)
library(tidyverse)
library(mapview)


# Overview-----
# Building upon the effect measure modification models from the paper,
#fit models where the ZCTA-specific RD is the outcome, and 
#Certain modifiable characteristics
#like AC and green space are the exposures.
#And then we control for confounders such as income or race/ethnicity.

#Conduct a meta-regression where more precise observations are weighted more heavily.

# Read in data------

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


#Sep 19, 2025
#Consider adding Kristen's data on heat so I can check out how it works
#in models..
#At the moment I don't fully understand the units but
#general direction should still be modelable
setwd(here("data-input","data-kristen-sent-August-2024"))
heat_kh_n_cases_diff_per_pop = read_csv("joined_rel.csv") %>% 
  #use the 95th percentile, one day
  rename(zcta=ZCTA5CE10,
         rd_heat_max95_1=max95_1) %>% 
  #and then just keep those two for now. I might later
  #bring in the SNR to calculate SD
  dplyr::select(zcta, rd_heat_max95_1)


heat_kh_n_cases_diff_per_pop


# Combine data-----
setwd(here("data-input","data-shared-by-chen"))
wf_eff_emm_wide=read_csv("hpi3_data_for_meta_figure6.csv") %>% 
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
  left_join(heat_kh_n_cases_diff_per_pop,by="zcta") %>% #heat data as well
  mutate(
    
    #also create this variable in case I need to re-calculate the rate difference
    #in different summed combinations
    #ie not expressed per 100k
    rd_quo_pt=rd_100k_quo_pt/100000,#rate differene per 1 person-day
    rd_quo_sd=rd_100k_quo_sd/100000,#rate differene per 1 person-day
    
    n_cases_diff_quo_pt=rd_quo_pt*pop, #number of cases per day without respect to population
    n_cases_diff_quo_sd=rd_quo_sd*pop, #number of cases per day without respect to population
    
    #for the model, let's create weights as the inverse of the modeled SD
    #what's the smallest standard deviation?
    rd_quo_sd_min=min(rd_quo_sd,na.rm=T),
    #divide the minimum by each SD. Higher values are less precise.
    rd_quo_sd_min_prop=rd_quo_sd_min/rd_quo_sd,
    rd_quo_sd_inv=1/rd_quo_sd,
    rd_quo_weight=rd_quo_sd_min_prop, 
    
    #find outliers
    #no grouping needed as elsewhere, as the data are wide-form
    rd_quo_1st=quantile(rd_quo_pt,probs=c(0.01), na.rm=T),
    rd_quo_99th=quantile(rd_quo_pt,probs=c(0.99), na.rm=T),
    rd_quo_outlier=case_when(
      rd_quo_pt<rd_quo_1st~1,
      rd_quo_pt>rd_quo_99th~1,
      TRUE~0),
    
    #transform some vars for modeling
    pop_dens_mi2_sqrt=sqrt(pop_dens_mi2),
    tree_canopy_prop_sqrt=sqrt(tree_canopy_prop),
    tree_canopy_prop_log=log(tree_canopy_prop),
    
    #categorize poverty for stratified analyses & modeling
    above_poverty_prop_cat=santoku::chop_equally(
      above_poverty_prop,groups=3)
    
  ) %>% 
  dplyr::select(
    zcta,
    starts_with("rd_100k_quo"),
    everything()
  ) 

#save this so I don't have to run every time
setwd(here("data-processed"))
save(wf_eff_emm_wide,file="wf_eff_emm_wide.RData")

#wf_eff_emm_wide %>% dplyr::select(zcta,contains("rd_quo")) %>% View()
names(wf_eff_emm_wide)
# Analyses and checks before modeling------
## check weights-----
wf_eff_emm_wide %>% 
  ggplot(aes(x=rd_quo_sd_min_prop))+
  geom_histogram()
wf_eff_emm_wide %>% 
  ggplot(aes(x=rd_quo_sd_min_prop,y=rd_quo_sd))+
  geom_point()

wf_eff_emm_wide %>% 
  ggplot(aes(x=rd_quo_weight,y=rd_quo_sd))+
  geom_point()

wf_eff_emm_wide %>% 
  ggplot(aes(x=rd_quo_weight,y=rd_quo_sd_inv))+
  geom_point()


##map and distribution of RD quo----
setwd(here("data-processed"))
load("zcta_ca_geo_simplified.RData")
zcta_ca_geo_simplified %>% 
  left_join(wf_eff_emm_wide,by="zcta") %>% 
  mapview(zcol="rd_100k_quo_pt")

wf_eff_emm_wide

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=rd_100k_quo_pt))+
  geom_histogram()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=rd_100k_quo_pt))+
  geom_histogram()


## distribution of covariates------
### pop density & RUCA-----
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=pop_dens_mi2_sqrt))+
  geom_histogram()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=pop_dens_mi2))+
  geom_histogram()


#pop dens vs RUCA
wf_eff_emm_wide %>% 
  ggplot(aes(x=ruca_cat,y=pop_dens_mi2_sqrt))+
  geom_point()

wf_eff_emm_wide %>% 
  ggplot(aes(x=ruca_cat,y=pop_dens_mi2))+
  geom_point()

#rd vs ruca
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat,y=rd_100k_quo_pt))+
  geom_boxplot()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  group_by(ruca_cat) %>% 
  summarise(
    rd_100k_quo_pt_sum=sum(rd_100k_quo_pt,na.rm=T),
    rd_100k_quo_pt_med=median(rd_100k_quo_pt,na.rm=T))

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  filter(ruca_cat=="(0,3]") %>% 
  ggplot(aes(x=rd_100k_quo_pt)) +
  geom_histogram()

### air conditioning------
#ac
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ac_prop))+
  geom_histogram()

#rd vs ac
wf_eff_emm_wide %>% 
  ggplot(aes(x=basin_name,y=ac_prop))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#rd x ac x ruca
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ac_prop, y=rd_100k_quo_pt))+
  geom_point()+
  facet_grid(
    rows="ruca_cat"
  )+
  geom_smooth(method="lm")



### tree canopy------
# tree canopy
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop))+
  geom_histogram()

#more normal. use this instead
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop_sqrt))+
  geom_histogram()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop_log))+
  geom_histogram()

#rd vs tree canopy
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop, y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop_sqrt, y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  filter(rd_100k_quo_pt<5.5) %>% 
  ggplot(aes(x=tree_canopy_prop_log, y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()


#There appears to be a stronger association at lower
#tree canopy values
#When tree canopy is square rooted, it does fit better.
#But the fit is still not good.

# tree canopy x RD x ruca cat
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  #  filter(rd_100k_quo_pt<10) %>% #omit influence
  ggplot(aes(x=tree_canopy_prop_sqrt, y=rd_100k_quo_pt))+
  geom_point()+
  facet_grid(
    rows="ruca_cat"
  )+
  geom_smooth(method="lm")

#this is actually semi-interesting: in cities, a stronger
#association between tree canopy and the risk difference.
#also, curiously, a strong association in rural areas. maybe
#that means desert areas have a stronger RD

### impervious surfaces-----
#how many obs w imp surface data do we have?
wf_eff_emm_wide %>% 
  filter(is.na(imperv_prop)==F)
#added Sep 18, 2025
#It's pretty normal
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=imperv_prop))+
  geom_histogram()

#map imp surfaces
zcta_ca_geo_simplified %>% 
  left_join(wf_eff_emm_wide,by="zcta") %>% 
  mapview(zcol="imperv_prop")

#rd vs imp surfaces
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=imperv_prop, y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()


#rd vs imp surface x ruca
nrow(wf_eff_emm_wide)
wf_eff_emm_wide %>% 
  group_by(ruca_cat) %>% 
  summarise(imperv_prop_m=mean(imperv_prop,na.rm=T),
            n=n())
wf_eff_emm_wide %>% 
#  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=imperv_prop, y=rd_100k_quo_pt))+
  geom_point()+
  facet_grid(
    rows="ruca_cat"
  )+
  geom_smooth(method="lm")


### air basin-------
#rd vs air basin
wf_eff_emm_wide %>% 
  ggplot(aes(x=basin_name,y=rd_100k_quo_pt))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

wf_eff_emm_wide %>% 
  group_by(basin_name) %>% 
  summarise(n_zcta=n())


#tree canopy vs air basin
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=basin_name, y=tree_canopy_prop))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#tree canopy x RD x air basin
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  filter(rd_100k_quo_pt<10) %>% #omit influence
  ggplot(aes(x=tree_canopy_prop_sqrt, y=rd_100k_quo_pt))+
  geom_point()+
  facet_grid(
    rows="basin_name"
  )+
  geom_smooth(method="lm")


### biome------
wf_eff_emm_wide %>% 
  ggplot(aes(x=biome_name_freq,y=rd_100k_quo_pt))+
  geom_boxplot()

#tree canopy x RD x biome
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=tree_canopy_prop_sqrt, y=rd_100k_quo_pt))+
  geom_point()+
  facet_grid(
    rows="biome_name_freq"
  )+
  geom_smooth(method="lm")

#biome x RUCA
wf_eff_emm_wide %>% 
  group_by(biome_name_freq,ruca_cat) %>% 
  summarise(n=n())



### others and bivariate------
#above poverty
wf_eff_emm_wide %>% 
  ggplot(aes(x=above_poverty_prop))+
  geom_histogram()

table(wf_eff_emm_wide$above_poverty_prop_cat)

#above poverty x rd
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=above_poverty_prop,y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()

#above poverty categorical x rd
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=above_poverty_prop_cat,y=rd_100k_quo_pt))+
  geom_boxplot()

#insurance distribution
#pretty normal. go with it.
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop))+
  geom_histogram()

wf_eff_emm_wide %>% 
  mutate(insured_prop_sqrt=sqrt(insured_prop)) %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop_sqrt))+
  geom_histogram()


#insurance vs RD
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop,y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()


#insurance vs air basin
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop, y=tree_canopy_prop))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#tree canopy vs insurance
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop, y=tree_canopy_prop))+
  geom_point()+
  geom_smooth()



#insurance vs poverty
names(wf_eff_emm_wide)
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop, y=above_poverty_prop))+
  geom_point()+
  geom_smooth()

#insurance v race latino
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=insured_prop, y=above_poverty_prop))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#rd vs poverty
wf_eff_emm_wide %>% 
  ggplot(aes(x=above_poverty_prop,y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()

#rd vs race latino
wf_eff_emm_wide %>% 
  ggplot(aes(x=race_latino,y=rd_100k_quo_pt))+
  geom_point()


#rd vs pop dens
wf_eff_emm_wide %>% 
  ggplot(aes(x=pop_dens_mi2,y=rd_100k_quo_pt))+
  geom_point()+
  geom_smooth()


### heat as outcome vs some covars---------
#### tree canopy------
wf_eff_emm_wide %>% 
  ggplot(aes(x=tree_canopy_prop, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()

wf_eff_emm_wide %>% 
  ggplot(aes(x=tree_canopy_prop_sqrt, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()

wf_eff_emm_wide %>% 
  group_by(STATE) %>% 
  summarise(
    corr_spearman=cor(
      x=tree_canopy_prop_sqrt,
      y=rd_heat_max95_1,
      method="spearman"),
    corr_pearson=cor(
      x=tree_canopy_prop_sqrt,
      y=rd_heat_max95_1,
      method="pearson")
  ) %>% 
  dplyr::select(-STATE) 

#### impervious surface--------
wf_eff_emm_wide %>% 
  ggplot(aes(x=imperv_prop, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()

wf_eff_emm_wide %>% 
  ggplot(aes(x=imperv_rank, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()


wf_eff_emm_wide %>% 
  group_by(STATE) %>% 
  summarise(
    corr_spearman=cor(
      x=imperv_prop,
      y=rd_heat_max95_1,
      method="spearman"),
    corr_pearson=cor(
      x=imperv_prop,
      y=rd_heat_max95_1,
      method="pearson")
  ) %>% 
  dplyr::select(-STATE) 

#by RUCA
wf_eff_emm_wide %>% 
  group_by(ruca_cat) %>% 
  summarise(
    corr_spearman=cor(
      x=imperv_prop,
      y=rd_heat_max95_1,
      method="spearman"),
    corr_pearson=cor(
      x=imperv_prop,
      y=rd_heat_max95_1,
      method="pearson")
  ) 


#that one's kinda interesting at the top of the distribution
#try stratify by ruca
wf_eff_emm_wide %>% 
  ggplot(aes(x=imperv_prop, y=rd_heat_max95_1))+
  geom_point()+
  facet_grid(
    rows="ruca_cat"
  )+
  geom_smooth(method="lm")

wf_eff_emm_wide %>% 
  ggplot(aes(x=imperv_rank, y=rd_heat_max95_1))+
  geom_point()+
  facet_grid(
    rows="ruca_cat"
  )+
  geom_smooth(method="lm")




wf_eff_emm_wide %>% 
  ggplot(aes(x=imperv_prop, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()

#### ac------
wf_eff_emm_wide %>% 
  ggplot(aes(x=ac_prop, y=rd_heat_max95_1))+
  geom_point()+
  geom_smooth()





## summarized stratified RD-------
### by air basin-------
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(y=rd_100k_quo_pt, x=tree_canopy_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="basin_name")

#weighted mean risk difference by air basin
rd_100k_quo_mean_wt_by_air_basin=wf_eff_emm_wide %>% 
  group_by(basin_name) %>% 
  summarise(rd_100k_quo_pt_mean_wt=weighted.mean(
    x=rd_100k_quo_pt,
    w=pop
  ))

rd_100k_quo_mean_wt_by_air_basin

#map spatial variation by air basin
load("california_air_basins_over_county.RData")
california_air_basins_over_county %>% 
  left_join(
    rd_100k_quo_mean_wt_by_air_basin,by="basin_name"
  ) %>% 
  mapview(zcol="rd_100k_quo_pt_mean_wt")



### by biome------
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(y=rd_100k_quo_pt, x=tree_canopy_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="biome_name")

#weighted mean risk difference by biome
rd_100k_quo_mean_wt_by_biome=wf_eff_emm_wide %>% 
  group_by(biome_name) %>% 
  summarise(rd_100k_quo_pt_mean_wt=weighted.mean(
    x=rd_100k_quo_pt,
    w=pop
  ))

rd_100k_quo_mean_wt_by_biome

setwd(here("data-input","biome-california"))
load("biomes_14_california.RData") 
biomes_14_california %>% 
  rename(biome_name=BIOME_NAME) %>% 
  left_join(
    rd_100k_quo_mean_wt_by_biome,by="biome_name"
  ) %>% 
  mapview(zcol="rd_100k_quo_pt_mean_wt")

### by RUCA--------
names(wf_eff_emm_wide)
rd_100k_quo_mean_wt_by_ruca=wf_eff_emm_wide %>% 
  group_by(ruca_cat) %>% 
  summarise(
    rd_100k_quo_pt_med=median(rd_100k_quo_pt,na.rm=T),
    rd_100k_quo_pt_mean_wt=weighted.mean(
        x=rd_100k_quo_pt,
        w=pop
  ))

rd_100k_quo_mean_wt_by_ruca

#map RUCAs
#lower numbers are more urban
zcta_ca_geo_simplified %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  mapview(zcol="ruca_cat")

zcta_ca_geo_simplified %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  group_by(ruca_cat) %>% 
  summarise(n_zctas=n()) %>% 
  mapview(zcol="ruca_cat")




# Final modeling dataset-------
#perhaps remove extreme outliers before modeling
summary(wf_eff_emm_wide$rd_100k_quo_pt)

wf_eff_emm_wide_no_outliers=wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0)

#save for markdown
setwd(here("data-processed"))
save(wf_eff_emm_wide_no_outliers,
     file="wf_eff_emm_wide_no_outliers.RData")

#Modeling continued here
#scripts/model-RD-as-outcome.R
