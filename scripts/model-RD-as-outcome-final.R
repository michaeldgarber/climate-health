#Continued from
#Script relies on
#source(here("scripts","read-wf-hosp.R"))
#source(here("scripts","read-heat-hosp.R"))

#October 15, 2025:
#Modeling decisions:
#We're using heat and wildfire as outcomes
#And the modifiers of interest are 
#Oct 17, 2025
#Decision to use imp surface as the main modifier for both heat
#and wildfire.
#Omit other combinations. Others are explored here
#climate-health/scripts/model-RD-as-outcome-exploratory.R
#Present final models at the end of the code.

# October 23, 2025:
#For writing, it will be easier to have two exposures, to focus less
#on a particula effect modifier.
#Decision:
#one model for each outcome. Include imp and tree in each model.

library(jtools) #for summarizing glm outputs with more customization

# heat as outcome--------
## impervious surface as modifier of interest------
### scatterplots------
names(outcome_heat_hi99_2)

outcome_heat_hi99_2_no_outliers %>% 
  # filter(rd_100k_quo_pt<20) %>% 
  # filter(rd_100k_quo_pt>-20) %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()

#does it vary by biome?
#we don't have enough data
outcome_heat_hi99_2_no_outliers %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="biome_name_freq")

#actually not that much variability here either. 
outcome_heat_hi99_2_no_outliers %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="ruca_cat3")

### model building------
glm_heat_imp=glm(
  rd_quo_pt~imperv_prop,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_heat_imp)

#insured rural biome. avoid longer abbrev than that
#We don't have enough obs for it.
glm_heat_imp_i_r_b=glm(
  rd_quo_pt~imperv_prop+insured_prop + ruca_cat3 +
    biome_name_freq,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

#do other covars make a difference?
glm_heat_imp_i_i_r_b=glm(
  rd_quo_pt~imperv_prop+insured_prop + 
    #income_per_capita+edu_bach_prop+
    ruca_cat3 +
    biome_name_freq,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_heat_imp_i_i_r_b)

summary(glm_heat_imp_i_r_b)


#biome doesn't really make a difference. urban-rural does, as does insured
#so we're good.


#simplify because there are very few desert obs (biome)
glm_heat_imp_i_r=glm(
  rd_quo_pt~imperv_prop+insured_prop + 
    ruca_cat3 ,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_heat_imp_i_r)

#add poverty too even though there's limited association?
#No, it's associated with insured proportion so leave it just
#to insured prop

glm_heat_imp_i_p_r=glm(
  rd_quo_pt~imperv_prop+insured_prop + above_poverty_prop+
    ruca_cat3 ,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

#sure
summary(glm_heat_imp_i_p_r)


#adding interaction terms
#maybe simpler to just leave out?
#not enough data
#graphically not as much difference (see above)
glm_heat_imp_i_r_b_int=glm(
  rd_quo_pt~imperv_prop+insured_prop + ruca_cat3 +
    biome_name_freq+
    imperv_prop*ruca_cat3+
    imperv_prop*biome_name_freq
    ,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_heat_imp_i_r_b_int)
jtools::summ(
  glm_heat_imp_i_r_b_int)

#alternate presentation
jtools::summ(
  glm_heat_imp,
#  model.fit=F, #for brevity in the output, dont' show
  digits=3)

#okay, now add rural and biome and insured

## both imp and tree in the model------
#as below, include biome as well
glm_heat_imp_tree_i_r_b=glm(
  rd_quo_pt~imperv_prop+tree_canopy_prop_sqrt+insured_prop + 
    ruca_cat3 
  + biome_name_freq
  ,
  data=outcome_heat_hi99_2_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_heat_imp_tree_i_r_b)

#impervious surface still a strong predictor for heat. okay.
#tree canopy very weak..

# wildfire as outcome-----

## imp surface-----
### scatterplots------
outcome_wf_no_outliers %>% 
  # filter(rd_100k_quo_pt<20) %>% 
  # filter(rd_100k_quo_pt>-20) %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()

#actually something
#try stratified as well
outcome_wf_no_outliers %>% 
  # filter(rd_100k_quo_pt<20) %>% 
  # filter(rd_100k_quo_pt>-20) %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="biome_name_freq")


outcome_wf_no_outliers %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()+
  facet_grid(rows="ruca_cat3")


outcome_wf_no_outliers %>% 
  ggplot(aes(y=rd_100k_quo_pt,x=imperv_prop))+
  geom_point()+
  geom_smooth()

#insured and above pov
outcome_wf_no_outliers %>% 
  ggplot(aes(y=insured_prop,x=above_poverty_prop))+
  geom_point()+
  geom_smooth()

#They're very closely associated. So probably just need one.

### model building-------
names(outcome_wf_no_outliers)
glm_wf_imp_i_r=glm(
  rd_quo_pt~imperv_prop+insured_prop +
    ruca_cat3 ,
  data=outcome_wf_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_wf_imp_i_r)

#adding poverty as well
glm_wf_imp_i_r_p=glm(
  rd_quo_pt~imperv_prop+insured_prop + above_poverty_prop+
    ruca_cat3 ,
  data=outcome_wf_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_wf_imp_i_r_p)


## both imp and tree in the model------
# when both imp and tree are in the model
#and include biome even if it's not a strong confounder for imp.
#wait, actually it is a strong confounder for imp.
glm_wf_imp_tree_i_r_b=glm(
  rd_quo_pt~imperv_prop+tree_canopy_prop_sqrt+insured_prop + 
    ruca_cat3 
   + biome_name_freq
  ,
  data=outcome_wf_no_outliers,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight
)

summary(glm_wf_imp_tree_i_r_b)


# Final models------

summary(glm_heat_imp_tree_i_r_b) #final. both exposures
summary(glm_wf_imp_tree_i_r_b)

#vs
summary(glm_heat_imp_i_r)
summary(glm_wf_imp_i_r)

names(outcome_wf_no_outliers)


