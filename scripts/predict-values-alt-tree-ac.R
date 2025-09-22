#Predict values under the alternative scenario

#Do it separately for tree canopy and air conditioning

#Use the models to predict alternative values
library(here)
source(here("scripts","model-RD-as-outcome.R"))


#Follow similar to this, but it will be a little different, 
#climate-health/scripts/define-scenarios-functions.R

#as we have to consider values of more confounders

#May 23, 2025: 
#Following call with Tarik, we're using interaction terms for both biome and 
#rural-urban
library(here)

#Initial thinking on scenarios------
#Test the idea on a dataset where tree canopy is raised to the 80th percentile
#for that RUCA
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat, y=tree_canopy_prop_sqrt))+
  geom_boxplot()

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=biome_name_freq, y=tree_canopy_prop_sqrt))+
  geom_boxplot()

#It doesn't seem to vary that much by metropolitan area
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=biome_name_freq, y=tree_canopy_prop_sqrt))+
  geom_boxplot()+
  facet_grid(rows="ruca_cat")

#Tree canopy should be relative to biome. Not necessarily relative to ruca, as there some
#cities with high green within biome
#Note the strategy may differ for AC, 
#so I don't know if we can simply use same strategy.
#Yea, less AC use in temperate conifer forests. That said, many do have 100% AC.

wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=biome_name_freq, y=ac_prop))+
  geom_boxplot()


summary(wf_eff_emm_wide_no_outliers$tree_canopy_prop_sqrt)

#Data management before scenarios------
names(wf_eff_emm_wide_no_outliers)
wf_eff_emm_wide_df_for_scenarios=wf_eff_emm_wide_no_outliers %>% 
  dplyr::select(zcta,contains("rd_quo"),
                contains("n_cases_diff"),#would like to keep this around
                contains("tree"),contains("ac_prop"),
                contains("insured"),
                contains("ruca"),contains("biome"),
                contains("pop")#may need for weighting
                )

wf_eff_emm_wide_df_for_scenarios
  

# Test a scenario------
## Define alternative value------
# Test a scenario for tree canopy
wf_eff_emm_wide_df_for_scenarios_test= wf_eff_emm_wide_df_for_scenarios %>% 
  mutate( target_percentile=.8) %>% 
  group_by(biome_name_freq) %>% 
  mutate(
    tree_canopy_prop_sqrt_alt=quantile(
      tree_canopy_prop_sqrt,probs=target_percentile,na.rm=T
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    zcta_intervene=case_when(
      tree_canopy_prop_sqrt<tree_canopy_prop_sqrt_alt~1,
      TRUE~0
    )
  )

#Let's do just 4 scenarios, as it keeps the dataset smaller.
#i.e., for that biome, 20th, 40th, 80th, 100th
wf_eff_emm_wide_df_for_scenarios_test %>% View()

#Now, predict values using the predict function. Easier this way than doing it manually
#because it will provide standard errors and consider all variables in the model
## Predict outcome under alternative value------
tree_canopy_input_df_test=wf_eff_emm_wide_df_for_scenarios_test %>% 
  filter(zcta_intervene==1) %>% 
  #the target value needs to have the same variable name as the main exposure
  #variable for the predict function to work, so overwrite this:
  mutate(
    tree_canopy_prop_sqrt_original=tree_canopy_prop_sqrt,
    tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_alt)

tree_canopy_input_df_test %>% View()
  

#The model, from the other script, is
pred_tree_scenario_80=as_tibble(
  stats::predict(
    glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome,#the model
    tree_canopy_input_df_test, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    rd_alt_pt=fit,
    rd_alt_se=se.fit,
    residual_scale_alt=residual.scale
  )

pred_tree_scenario_80 %>% View()
nrow(pred_tree_scenario_80)
nrow(tree_canopy_input_df_test)

#also model the baseline so it can be compared
tree_canopy_input_df_test_original=tree_canopy_input_df_test %>% 
  dplyr::select(-tree_canopy_prop_sqrt) %>% 
  rename(tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_original)

pred_tree_scenario_quo_test=as_tibble(
  stats::predict(
    glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome,#the model
    tree_canopy_input_df_test_original, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    #pred for model-predicted status quo (baseline) value
    rd_quo_pred_pt=fit,
    rd_quo_pred_se=se.fit,
    residual_scale_quo=residual.scale
  )



## Link with original dataset and compare--------
#Note we can use bind_cols rather than left join
#because zcta is not in the data. It's the same order
compare_tree_scenario_80=tree_canopy_input_df_test %>% 
  bind_cols(pred_tree_scenario_80) %>% 
  bind_cols(pred_tree_scenario_quo_test) %>% 
  #calculate the difference between the original and alternative
  #scenarios
  #Note this could also be defined with uncertainty
  #by incorporating the standard error of the target
  #and the standard error of the quo
  mutate(
    #act denotes difference between actual baseline value and alternative predicted value
    #pred denotes difference between predicted baseline value and alternate pred value
    rd_diff_act_pt=rd_alt_pt-rd_quo_pt,
    rd_diff_pred_pt=rd_alt_pt-rd_quo_pred_pt)

compare_tree_scenario_80 %>% View()

#usually this number will be negative because we expect the RD
#to be SMALLER under the alternative scenario, that is, that
#wildfires will have a SMALLER effect
#for example
3-5
#alternative - quo
compare_tree_scenario_80 %>% 
  ggplot(aes(x=rd_diff_act_pt))+
  geom_histogram()

compare_tree_scenario_80 %>% 
  ggplot(aes(x=rd_diff_pred_pt))+
  geom_histogram()

compare_tree_scenario_80 %>% View()


#stratify RD diff by RUCA
compare_tree_scenario_80 %>% 
  ggplot(aes(x=ruca_cat,y=rd_diff_act_pt))+
  geom_boxplot()

compare_tree_scenario_80 %>% 
  ggplot(aes(x=ruca_cat,y=rd_quo_pt)) +
  geom_boxplot()

tree_canopy_input_df_test %>% 
  group_by(ruca_cat) %>% 
  summarise(
    rd_quo_pt_sum=sum(rd_quo_pt,na.rm=T),
    rd_quo_pt_med=median(rd_quo_pt,na.rm=T)
  ) 
  
#t for target; b fr quo
#can we just sum rate differences? I'm not so sure. Probably better to re-calculate
#from scratch
compare_tree_scenario_80 %>% 
  group_by(ruca_cat) %>% 
  summarise(
    rd_diff_act_pt_mean_wt=weighted.mean(x=rd_diff_act_pt, w=pop,na.rm=T),
    rd_diff_pred_pt_mean_wt=weighted.mean(
      x=rd_diff_pred_pt,w=pop,na.rm=T
    ),
    rd_diff_act_pt_med=median(rd_diff_act_pt, na.rm=T),
    rd_quo_pt_med=median(rd_quo_pt,na.rm=T),
    rd_quo_sum=sum(rd_quo_pt,na.rm=T),
    rd_quo_pred_sum=(sum(rd_quo_pred_pt,na.rm=T)),
    rd_alt_sum=sum(rd_alt_pt,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    rd_diff_alt_vs_quo_actual=rd_alt_sum-rd_quo_sum,
    rd_diff_alt_v_quo_pred=rd_alt_sum-rd_quo_pred_sum
  ) %>% 
  View()
#interesting..when summed, the RD is quite high at quo for cities

# Run scenarios for both tree and A/C-------
#July 15, 2025: define residuals as well
## tree canopy-------
### Predict baseline values-----
#Baseline predicted values for each zcta
pred_tree_scenario_quo=as_tibble(
  stats::predict(
    glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome,#the final model
    wf_eff_emm_wide_df_for_scenarios, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    #pred for model-predicted status quo (baseline) value
    rd_quo_pred_pt=fit,
    rd_quo_pred_se=se.fit,
    residual_scale_quo=residual.scale
  ) %>% 
  bind_cols(wf_eff_emm_wide_df_for_scenarios) %>% 
  mutate(
    resid_diff =rd_quo_pt-rd_quo_pred_pt, 
    ratio_estimate_resid=rd_quo_pred_pt/resid_diff
  ) %>% 
  dplyr::select(zcta,contains("rd_quo_pred"),
                contains("resid")
  )


pred_tree_scenario_quo

#Take a look at residuals

pred_tree_scenario_quo %>% 
  ggplot(aes(x=resid_diff))+
  geom_histogram()

### Summarize residuals------
rsd_tree=pred_tree_scenario_quo %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(rsd=sd(resid_diff,na.rm=T)) %>% 
  ungroup()



#a lookup for the residuals to link
lookup_zcta_tree_resid_diff=pred_tree_scenario_quo %>% 
  dplyr::select(zcta,resid_diff)


### Run function over several scenarios-------
define_scenario_pt_tree=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_tree_for_model= wf_eff_emm_wide_df_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(biome_name_freq) %>% 
    mutate(
      tree_canopy_prop_sqrt_alt=quantile(
        tree_canopy_prop_sqrt,probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      zcta_intervene=case_when(
        tree_canopy_prop_sqrt<tree_canopy_prop_sqrt_alt~1,
        TRUE~0
      ),
      
      #Because the old model expects to see the variable name,
     # "tree_canopy_prop_sqrt", #in the model, we have to rename
      #the alternative value to that value. So do:

    #the target value needs to have the same variable name as the main exposure
    #variable for the predict function to work, so overwrite this:
      tree_canopy_prop_sqrt_original=tree_canopy_prop_sqrt,
      tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_alt
      ) 

  #and then we only predict values in the filtered data use the filtered data in the model  
  #and then we're only fitting the model in the filtered dataset
  
  input_df_tree_for_model_filtered=input_df_tree_for_model %>% 
      filter(zcta_intervene==1)  

  
    #Model the alternative RD
    pred_tree=as_tibble(
      stats::predict(
        glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome,#the model
        input_df_tree_for_model_filtered, #input dataset
        type = "response",
        se.fit = TRUE
      )) %>% 
      rename(
        rd_alt_pt=fit,
        rd_alt_se=se.fit,
        residual_scale_alt=residual.scale
      )

    #merge it back together
    input_df_tree_for_model_filtered_zcta_only=input_df_tree_for_model_filtered %>% 
      dplyr::select(zcta)

    #append the zcta to the modeled data
    scenario_tree_merged_filtered=input_df_tree_for_model_filtered_zcta_only %>% 
      bind_cols(pred_tree) 
    
    #and then left join the modeled data back with the unfiltered data
    scenario_tree_merged_df=input_df_tree_for_model %>% 
      left_join(scenario_tree_merged_filtered,by="zcta") %>% 
      #and get the tree canopy data back with the right terms
      dplyr::select(-tree_canopy_prop_sqrt) %>% 
      mutate(
        tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_original,
      ) %>% 
      #organize data
      dplyr::select(
        zcta,
        contains("canopy"),
        contains("rd_quo"),contains("_alt"),everything()
      )
    

    return(scenario_tree_merged_df)

}

#### Test tree canopy function-----
#test function
hi=define_scenario_pt_tree(.5)

hi %>% dplyr::select(contains("tree"),contains("rd")) %>% View()
names(hi)
hi %>% 
  ggplot(aes(x=rd_quo_pt)) +
  geom_histogram()

hi %>% 
  ggplot(aes(x=rd_alt_pt)) +
  geom_histogram()

#### Run tree canopy function several times----------
#and combine with the other data
#Run it ten times and combine data
#zero to 1, 0 options
#For testing use fewer, e.g., by .2 rather than by .1
target_percentile_vector =seq(0, 1, by=.2)


#oh! also link in the predicted status-quo values
tree_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_tree) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_tree_scenario_quo,by="zcta") %>% 
  mutate(
    #some calculations that apply everywhere
    #number of cases per day under the predicted baseline scenario
    n_cases_diff_quo_pred_pt=rd_quo_pred_pt*pop,  
    n_cases_diff_quo_pred_se=rd_quo_pred_se*pop,  
    
    n_cases_diff_alt_pt=rd_alt_pt*pop, #number of cases per day without respect to population
    n_cases_diff_alt_se=rd_alt_se*pop  #number of cases per day without respect to population,
    
  )




tree_scenarios_all_pt %>% dplyr::select(contains("n_cases")) %>% View()
#how many obs per target percentile
#done. This is working as expected now.
tree_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)

#save one for markdown map
names(tree_scenarios_all_pt)


#### Bootstrap the confidence intervals using modeled standard errors------
#write a function and run it 1,000 times
names(tree_scenarios_all_pt)
tree_scenarios_all_for_boot=tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_tree)

tree_scenarios_all_for_boot



names(tree_scenarios_all_for_boot)

tree_scenario_boot=function(s_id_val){
  
  out=tree_scenarios_all_for_boot %>% 
    group_by(target_percentile) %>% 
    mutate(
      s_id=s_id_val, #replication ID,
      
      #July 15, 2025: changing how I define the sd
      #using residual standard deviation instead of predicted
      #standard error
      rd_quo_pred_boot=rnorm(
        n=n(),
        mean=rd_quo_pred_pt,
        sd=rsd),
      
      rd_alt_boot=rnorm(
        n=n(),
        mean=rd_alt_pt,
        sd=rsd)
    ) %>% 
    ungroup()
}

names(tree_scenarios_all_pt)
#link in everything else
library(purrr)
##### Define boot reps-------
n_boot_reps=500
#100 goes pretty quick. 1,000 takes a bit.
tree_scenario_boot_df=1:n_boot_reps %>% 
  map_dfr(tree_scenario_boot)  

#I need population
setwd(here("data-processed"))
load("lookup_pop_zcta.RData")
lookup_pop_zcta
lookup_zcta_ca_biome

#also need intervention status
lookup_zcta_intervene_tree_scenario=tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)

tree_scenario_boot_df_wrangle=tree_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_pop_zcta,by="zcta") %>% 
  left_join(lookup_zcta_intervene_tree_scenario,
            by=c("zcta","target_percentile")) %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  left_join(lookup_zcta_ca_biome,by="zcta") %>% #link biome Jul 14, 2025
  mutate(
    #number of cases caused per day without respect to population: status-quo
    n_cases_diff_quo_pred_boot=rd_quo_pred_boot*pop,
    
    #number of cases caused per day without respect to population: alternative
    n_cases_diff_alt_boot=rd_alt_boot*pop,
    
    #also need the corresponding differences, which will
    #be useful for the zcta-specific summaries.
    #In discussion with Tarik, we're only using the difference between predicted status-quo
    #and predicted alternative
    
    #difference between predicted baseline and predicted actual
    rd_diff_pred_boot=rd_alt_boot-rd_quo_pred_boot,
    
    #difference in n cases
    n_cases_diff_diff_pred_boot=n_cases_diff_alt_boot-n_cases_diff_quo_pred_boot 
  )

# tree_scenario_boot_df_wrangle %>% 
#   filter(zcta_intervene==1) %>% 
#   View()

#tree_scenario_boot_df_wrangle %>% View()


## air conditioning-------
### predict baseline values-------
pred_ac_scenario_quo=as_tibble(
  stats::predict(
    glm_rd_ac_conf_ins_ruca_biome_int_ruca_biome,#the final model
    wf_eff_emm_wide_df_for_scenarios, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    #pred for model-predicted status quo (baseline) value
    rd_quo_pred_pt=fit,
    rd_quo_pred_se=se.fit,
    residual_scale_quo=residual.scale
  ) %>% 
  bind_cols(wf_eff_emm_wide_df_for_scenarios) %>% 
  mutate(
    resid_diff =rd_quo_pt-rd_quo_pred_pt, 
    ratio_estimate_resid=rd_quo_pred_pt/resid_diff
    ) %>% 
  dplyr::select(zcta,contains("rd_quo_pred"),
                contains("resid")
                )

pred_ac_scenario_quo

#Take a look at residuals

pred_ac_scenario_quo %>% 
  ggplot(aes(x=resid_diff))+
  geom_histogram()



### Summarize residuals------
rsd_ac=pred_ac_scenario_quo %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(rsd=sd(resid_diff,na.rm=T)) %>% 
  ungroup()

rsd_ac

#Copy and paste code above. Not using exact same function, though,
#because it's easier to keep the data wide form.

### Run function over several scenarios-------
names(wf_eff_emm_wide_df_for_scenarios)
#essentially same function as above but using ac instead of tree canopy
define_scenario_pt_ac=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_ac_for_model= wf_eff_emm_wide_df_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(biome_name_freq) %>% 
    mutate(
      ac_prop_alt=quantile(
        ac_prop,
        probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      zcta_intervene=case_when(
        ac_prop<ac_prop_alt~1,
        TRUE~0
      ),
      
      ac_prop_original=ac_prop,
      ac_prop=ac_prop_alt
    ) 
  
  
  input_df_ac_for_model_filtered=input_df_ac_for_model %>% 
    filter(zcta_intervene==1)  
  
  
  #Model the alternative RD
  pred_ac=as_tibble(
    stats::predict(
      glm_rd_ac_conf_ins_ruca_biome_int_ruca_biome,#the model
      input_df_ac_for_model_filtered, #input dataset
      type = "response",
      se.fit = TRUE
    )) %>% 
    rename(
      rd_alt_pt=fit,
      rd_alt_se=se.fit,
      residual_scale_alt=residual.scale
    )
  
  #merge it back together
  input_df_ac_for_model_filtered_zcta_only=input_df_ac_for_model_filtered %>% 
    dplyr::select(zcta)
  
  #append the zcta to the modeled data
  scenario_ac_merged_filtered=input_df_ac_for_model_filtered_zcta_only %>% 
    bind_cols(pred_ac) 
  
  #and then left join the modeled data back with the unfiltered data
  scenario_ac_merged_df=input_df_ac_for_model %>% 
    left_join(scenario_ac_merged_filtered,by="zcta") %>% 
    #and get the ac canopy data back with the right terms
    dplyr::select(-ac_prop) %>% 
    mutate(
      ac_prop=ac_prop_original,
    ) %>% 
    #organize data
    dplyr::select(
      zcta,
      contains("ac_prop"),
      contains("rd_quo"),contains("_alt"),everything()
    )
  
  return(scenario_ac_merged_df)
  
}

### test it---------
hi_ac=define_scenario_pt_ac(.5)
hi_ac %>% View()

### Run function several times------

#oh! also link in the predicted status-quo values
ac_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_ac) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_ac_scenario_quo,by="zcta") %>% 
  mutate(
    #some calculations that apply everywhere
    #number of cases per day under the predicted baseline scenario
    n_cases_diff_quo_pred_pt=rd_quo_pred_pt*pop,  
    n_cases_diff_quo_pred_se=rd_quo_pred_se*pop,  
    
    n_cases_diff_alt_pt=rd_alt_pt*pop, #number of cases per day without respect to population
    n_cases_diff_alt_se=rd_alt_se*pop #number of cases per day without respect to population
  )

ac_scenarios_all_pt

### Bootsrap confidence intervals-------

### Bootstrap the confidence intervals using modeled standard errors------
#write a function and run it 1,000 times
names(ac_scenarios_all_pt)
ac_scenarios_all_for_boot=ac_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_ac)

#ac_scenarios_all_for_boot %>% View()

names(ac_scenarios_all_for_boot)

#Function has to be differenrt because different input data set
ac_scenario_boot=function(s_id_val){
  
  out=ac_scenarios_all_for_boot %>% 
    group_by(target_percentile) %>% 
    mutate(
      s_id=s_id_val, #replication ID,
      
      rd_quo_pred_boot=rnorm(
        n=n(),
        mean=rd_quo_pred_pt,
        sd=rsd),
      
      rd_alt_boot=rnorm(
        n=n(),
        mean=rd_alt_pt,
        sd=rsd)
    ) %>% 
    ungroup()
}

names(ac_scenarios_all_pt)
#link in everything else
n_boot_reps #defined above
ac_scenario_boot_df=1:n_boot_reps %>% #100 goes pretty quick. 1,000 takes a bit.
  map_dfr(ac_scenario_boot)  


#also need intervention status
lookup_zcta_intervene_ac_scenario=ac_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)

ac_scenario_boot_df_wrangle=ac_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_pop_zcta,by="zcta") %>% 
  left_join(lookup_zcta_intervene_ac_scenario,
            by=c("zcta","target_percentile")) %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  left_join(lookup_zcta_ca_biome,by="zcta") %>% #link biome Jul 14, 2025
  mutate(
    #number of cases caused per day without respect to population: status-quo
    n_cases_diff_quo_pred_boot=rd_quo_pred_boot*pop,
    
    #number of cases caused per day without respect to population: alternative
    n_cases_diff_alt_boot=rd_alt_boot*pop,
    
    #also need the corresponding differences, which will
    #be useful for the zcta-specific summaries.
    #In discussion with Tarik, we're only using the difference between predicted status-quo
    #and predicted alternative
    
    #difference between predicted baseline and predicted actual
    rd_diff_pred_boot=rd_alt_boot-rd_quo_pred_boot,
    
    #difference in n cases
    n_cases_diff_diff_pred_boot=n_cases_diff_alt_boot-n_cases_diff_quo_pred_boot 
  )

# ac_scenario_boot_df_wrangle %>% 
#   filter(zcta_intervene==1) %>% 
#   View()

#ac_scenario_boot_df_wrangle %>% View()


# summary------
#calculate summary rate differences
names(tree_scenarios_all_pt)


## Summary functions for point estimates------

summarise_ungroup_n_cases_diff_rd_pop=function(df){
  df %>% 
    summarise(
      n_zcta =n(),
      n_cases_diff_alt_pt=sum(n_cases_diff_alt_pt,na.rm=T),
      n_cases_diff_quo_pt=sum(n_cases_diff_quo_pt),
      
      n_cases_diff_quo_pred_pt=sum(n_cases_diff_quo_pred_pt,na.rm=T),
      
      #Note the weighted mean IS the same. GOod! I've calculated it correctly.
      rd_alt_pt_med=median(rd_alt_pt,na.rm=T),#for comparison with below
      rd_alt_pt_mean_wt=weighted.mean(x=rd_alt_pt,w=pop,na.rm=T), #for comparison with below
      
      pop=sum(pop)
    ) %>% 
    ungroup()

}

mutate_rd_diff=function(df){
  df %>% 
    mutate(
      rd_alt_pt=n_cases_diff_alt_pt/pop,
      rd_quo_pt=n_cases_diff_quo_pt/pop,
      rd_quo_pred_pt=n_cases_diff_quo_pred_pt/pop,
      
      #difference between predicted alternative and actual
      rd_diff_act_pt=rd_alt_pt-rd_quo_pt, 
      #difference between predicted baseline and predicted actual
      rd_diff_pred_pt=rd_alt_pt-rd_quo_pred_pt,
      
      #convert to 100k as well. Only the ones we're presenting
      rd_100k_alt_pt=rd_alt_pt*100000,
      rd_100k_quo_pred_pt=rd_quo_pred_pt*100000,
      rd_100k_diff_pred_pt=rd_diff_pred_pt*100000,
      
      
      #difference in n cases. One is difference between actual
      #and predicted, and the other is between predicted status-quo
      #and predicted alternative
      n_cases_diff_diff_act_pt=n_cases_diff_alt_pt-n_cases_diff_quo_pt, 
      n_cases_diff_diff_pred_pt=n_cases_diff_alt_pt-n_cases_diff_quo_pred_pt,
      
    ) 
}

## Summary functions for bootstrap estimates------
#Very similar, just a different var name, and only include
#those things that woudl vary by rep
summarise_ungroup_n_cases_diff_rd_pop_boot=function(df){
  df %>% 
    summarise(
      n_cases_diff_alt_boot=sum(n_cases_diff_alt_boot,na.rm=T),
      n_cases_diff_quo_pred_boot=sum(n_cases_diff_quo_pred_boot,na.rm=T),
      #for comparison with below
      rd_alt_boot_mean_wt=weighted.mean(x=rd_alt_boot,w=pop,na.rm=T), 

      pop=sum(pop)#doesn't vary but needed for calculations

    ) %>% 
    ungroup()
}

mutate_rd_diff_boot=function(df){
  df %>% 
    mutate(
      rd_alt_boot=n_cases_diff_alt_boot/pop,
      rd_quo_pred_boot=n_cases_diff_quo_pred_boot/pop,
      
      #difference between predicted baseline and predicted actual
      rd_diff_pred_boot=rd_alt_boot-rd_quo_pred_boot,

      #Don't really need to present per 100k here yet. Do it below
      #when summarize over replicates
      
      
      #difference in n cases
      n_cases_diff_diff_pred_boot=n_cases_diff_alt_boot-n_cases_diff_quo_pred_boot
      
    ) 
}


## Point estimates------
### tree------
#### overall--------
tree_summary_overall=tree_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  #to facilitate the shiny, make this
  mutate(
    intervene_var="tree"
  )

setwd(here("data-processed"))
save(tree_summary_overall,file="tree_summary_overall.RData")

tree_summary_overall %>% View()



#### by RUCA----------
tree_summary_ruca_cat=tree_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,ruca_cat) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="tree"
  )

setwd(here("data-processed"))
save(tree_summary_ruca_cat,file="tree_summary_ruca_cat.RData")

#### by biome----------
names(tree_scenarios_all_pt)
tree_summary_biome_name_freq=tree_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,biome_name_freq) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="tree"
  )

setwd(here("data-processed"))
save(tree_summary_biome_name_freq,
     file="tree_summary_biome_name_freq.RData")

tree_summary_biome_name_freq %>% 
  filter(target_percentile==.8) %>% 
  View()

#### by zcta---------
#I need an overall one for the map
#This should work
tree_summary_by_zcta=tree_scenarios_all_pt %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="tree"
  )

names(tree_summary_by_zcta)
tree_summary_by_zcta_for_map=tree_summary_by_zcta %>% 
  dplyr::select(zcta,
                intervene_var,
                target_percentile,
                zcta_intervene,
                tree_canopy_prop,
                tree_canopy_prop_sqrt,
                tree_canopy_prop_sqrt_alt,

                pop,ruca_cat,biome_name_freq,
                contains("100k"),
                contains("n_cases_diff_diff")
                )

tree_summary_by_zcta_for_map
setwd(here("data-processed"))
save(tree_summary_by_zcta_for_map,
     file="tree_summary_by_zcta_for_map.RData")


### ac---------
#### overall-------
ac_summary_overall=ac_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="ac"
  )

setwd(here("data-processed"))
save(ac_summary_overall,file="ac_summary_overall.RData")

#ac_summary_overall %>% View()

#### by RUCA------------
ac_summary_ruca_cat=ac_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,ruca_cat) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="ac"
  )

setwd(here("data-processed"))
save(ac_summary_ruca_cat,file="ac_summary_ruca_cat.RData")

#### by biome--------
ac_summary_biome_name_freq=ac_scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,biome_name_freq) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="ac"
  )

setwd(here("data-processed"))
save(ac_summary_biome_name_freq,file="ac_summary_biome_name_freq.RData")

#by the way, what does A/C look like in selected biomes?
#One of the A/C scenarios didn't work
wf_eff_emm_wide %>% 
  filter(rd_quo_outlier==0) %>%
  group_by(biome_name_freq) %>%
  summarise(
    ac_prop_20th=quantile(ac_prop,probs=c(0.2),na.rm=T),
    ac_prop_40th=quantile(ac_prop,probs=c(0.4),na.rm=T),
    ac_prop_60th=quantile(ac_prop,probs=c(0.6),na.rm=T),
  ) %>% 
  ungroup()

#### by zcta for a map------------
ac_summary_by_zcta=ac_scenarios_all_pt %>% 
  mutate_rd_diff() %>% 
  mutate(
    intervene_var="ac"
  )


ac_summary_by_zcta_for_map=ac_summary_by_zcta %>% 
  dplyr::select(zcta,
                intervene_var,
                target_percentile,
                zcta_intervene,
                ac_prop,
                ac_prop_alt,
                pop,ruca_cat,biome_name_freq,
                contains("100k"),
                contains("n_cases_diff_diff")
  )

ac_summary_by_zcta_for_map
setwd(here("data-processed"))
save(ac_summary_by_zcta_for_map,
     file="ac_summary_by_zcta_for_map.RData")

## Confidence intervals------
### Write functions-------
#alright this is working..
#create a function for this
summarise_create_confidence_intervals=function(df){
  df %>% 
    summarise(
      
      rd_alt_ll=quantile(rd_alt_boot,probs=c(0.025),na.rm=T),
      rd_alt_ul=quantile(rd_alt_boot,probs=c(0.975),na.rm=T),
      
      rd_quo_pred_ll=quantile(rd_quo_pred_boot,probs=c(0.025),na.rm=T),
      rd_quo_pred_ul=quantile(rd_quo_pred_boot,probs=c(0.975),na.rm=T),
      
      rd_diff_pred_ll=quantile(rd_diff_pred_boot,probs=c(0.025),na.rm=T),
      rd_diff_pred_ul=quantile(rd_diff_pred_boot,probs=c(0.975),na.rm=T),

      
      n_cases_diff_alt_ll=quantile(n_cases_diff_alt_boot,probs=c(0.025),na.rm=T),
      n_cases_diff_alt_ul=quantile(n_cases_diff_alt_boot,probs=c(0.975),na.rm=T),
      
      n_cases_diff_quo_pred_ll=quantile(
        n_cases_diff_quo_pred_boot,probs=c(0.025),na.rm=T),
      n_cases_diff_quo_pred_ul=quantile(
        n_cases_diff_quo_pred_boot,probs=c(0.975),na.rm=T),
      
      n_cases_diff_diff_pred_ll=quantile(
        n_cases_diff_diff_pred_boot,probs=c(0.025),na.rm=T),
      
      n_cases_diff_diff_pred_ul=quantile(
        n_cases_diff_diff_pred_boot,probs=c(0.975),na.rm=T)
    ) %>% 
    ungroup() %>% 
    mutate(
      #per 100k as well. multiplying by the constant is fine.
      rd_100k_alt_ll=rd_alt_ll*100000,
      rd_100k_alt_ul=rd_alt_ul*100000,
      
      rd_100k_quo_pred_ll=rd_quo_pred_ll*100000,
      rd_100k_quo_pred_ul=rd_quo_pred_ul*100000,

      rd_100k_diff_pred_ll=rd_diff_pred_ll*100000,
      rd_100k_diff_pred_ul=rd_diff_pred_ul*100000
    )
}

### summarize - tree------
#### overall--------
tree_summary_overall_ci=tree_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(tree_summary_overall_ci,
     file="tree_summary_overall_ci.RData")


#### By RUCA---------
names(tree_scenario_boot_df_wrangle)
tree_summary_ruca_cat_ci=tree_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,ruca_cat,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile,ruca_cat) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(tree_summary_ruca_cat_ci,
     file="tree_summary_ruca_cat_ci.RData")
tree_summary_ruca_cat_ci %>% View()

#### by biome_name_freq---------
names(tree_scenario_boot_df_wrangle)
tree_summary_biome_name_freq_ci=tree_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,biome_name_freq,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile,biome_name_freq) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(tree_summary_biome_name_freq_ci,
     file="tree_summary_biome_name_freq_ci.RData")
tree_summary_biome_name_freq_ci %>% View()

### summarize - ac------
#### overall------
ac_summary_overall_ci=ac_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(ac_summary_overall_ci,
     file="ac_summary_overall_ci.RData")

#### By RUCA------
names(ac_scenario_boot_df_wrangle)
ac_summary_ruca_cat_ci=ac_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,ruca_cat,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile,ruca_cat) %>% 
  summarise_create_confidence_intervals()

ac_summary_ruca_cat_ci %>% View()
setwd(here("data-processed"))
save(ac_summary_ruca_cat_ci,
     file="ac_summary_ruca_cat_ci.RData")


#### by biome------
names(ac_scenarios_all_pt)
ac_summary_biome_name_freq_ci=ac_scenario_boot_df_wrangle %>% 
  filter(zcta_intervene==1) %>% 
  group_by(target_percentile,biome_name_freq,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(target_percentile,biome_name_freq) %>% 
  summarise_create_confidence_intervals()

ac_summary_biome_name_freq_ci %>% View()
setwd(here("data-processed"))
save(ac_summary_biome_name_freq_ci,
     file="ac_summary_biome_name_freq_ci.RData")


# combine them together for shiny------
## overall summary-----
tree_summary_overall_pt_ci=tree_summary_overall %>% 
  left_join(tree_summary_overall_ci,by="target_percentile")

ac_summary_overall_pt_ci=ac_summary_overall %>% 
  left_join(ac_summary_overall_ci,by="target_percentile")

tree_ac_summary_overall_pt_ci =tree_summary_overall_pt_ci %>% 
  bind_rows(ac_summary_overall_pt_ci) %>% 
  dplyr::select(intervene_var,target_percentile,everything()) %>% 
  #and only pick those that will be used for shiny
  dplyr::select(
    intervene_var,target_percentile,n_zcta, pop,
    
    contains("rd_100k_quo_pred"), 
    contains("rd_100k_alt"),
    contains("rd_100k_diff_pred"),
    
    contains("n_cases_diff_quo_pred"),
    contains("n_cases_diff_alt"),
    contains("n_cases_diff_diff_pred")
  )

names(tree_ac_summary_overall_pt_ci)  

setwd(here("data-processed"))
save(tree_ac_summary_overall_pt_ci,
     file="tree_ac_summary_overall_pt_ci.RData")


## zcta level for maps------
tree_ac_summary_by_zcta_for_map=tree_summary_by_zcta_for_map %>% 
  bind_rows(ac_summary_by_zcta_for_map) %>% 
  dplyr::select(intervene_var,target_percentile,everything()) %>% 
  #For the shiny, I want this:
  mutate(
    zcta_intervene_char=as.character(zcta_intervene),
    
    #sometimes I want this as yes/no
    zcta_intervene_char_yn=case_when(
      zcta_intervene_char==1~"Yes",
      zcta_intervene_char==0~"No",
    )
  )
  

setwd(here("data-processed"))
save(tree_ac_summary_by_zcta_for_map,
     file="tree_ac_summary_by_zcta_for_map.RData")


## by RUCA------
tree_summary_ruca_cat_pt_ci=tree_summary_ruca_cat %>% 
  left_join(tree_summary_ruca_cat_ci,by=c("target_percentile",
                                          "ruca_cat")
  )

tree_summary_ruca_cat_pt_ci

ac_summary_ruca_cat_pt_ci=ac_summary_ruca_cat %>% 
  left_join(ac_summary_ruca_cat_ci,by=c("target_percentile",
                                          "ruca_cat")
  )

tree_ac_summary_ruca_cat_pt_ci =tree_summary_ruca_cat_pt_ci %>% 
  bind_rows(ac_summary_ruca_cat_pt_ci) %>% 
  dplyr::select(intervene_var,ruca_cat,target_percentile,everything()) %>% 
  #and only pick those that will be used for shiny
  dplyr::select(
    intervene_var,target_percentile,ruca_cat,n_zcta, pop,
    
    contains("rd_100k_quo_pred"), 
    contains("rd_100k_alt"),
    contains("rd_100k_diff_pred"),
    
    contains("n_cases_diff_quo_pred"),
    contains("n_cases_diff_alt"),
    contains("n_cases_diff_diff_pred")
  )


## by biome------
tree_summary_biome_name_freq_pt_ci=tree_summary_biome_name_freq %>% 
  left_join(tree_summary_biome_name_freq_ci,by=c("target_percentile",
                                          "biome_name_freq")
  )

tree_summary_biome_name_freq_pt_ci

ac_summary_biome_name_freq_pt_ci=ac_summary_biome_name_freq %>% 
  left_join(ac_summary_biome_name_freq_ci,by=c("target_percentile",
                                        "biome_name_freq")
  )

tree_ac_summary_biome_name_freq_pt_ci =tree_summary_biome_name_freq_pt_ci %>% 
  bind_rows(ac_summary_biome_name_freq_pt_ci) %>% 
  dplyr::select(intervene_var,biome_name_freq,target_percentile,everything()) %>% 
  #and only pick those that will be used for shiny
  dplyr::select(
    intervene_var,target_percentile,biome_name_freq,n_zcta, pop,
    
    contains("rd_100k_quo_pred"), 
    contains("rd_100k_alt"),
    contains("rd_100k_diff_pred"),
    
    contains("n_cases_diff_quo_pred"),
    contains("n_cases_diff_alt"),
    contains("n_cases_diff_diff_pred")
  )

  