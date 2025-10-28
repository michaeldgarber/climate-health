#Predict values under the alternative scenarios

library(here)
source(here("scripts","model-RD-as-outcome-final.R"))

#Oct 17, 2025:
#Make analyses relative to RUCA. I'm ignoring biome here now.
#For previous comments, see previous versions

#Oct 23, 2025
#For imperviosu surfaces, it shoudl be relative to RUCA,
#but for tree canopy, it should be relative to biome. Ready, go
#glm_heat_imp_tree_i_r_b

#what if we just did both for each. are there enough obs?
#That would allow me to use the same group-by
#I still feel as though biome shouldn't matter for imp., though
#So just do it differently

#Something to keep in mind: the percentile calculation
#will be flipped for tree vs imp because we want lower is good for imp
#and higher is good for tree
outcome_wf_no_outliers %>% 
  group_by(biome_name_freq,ruca_cat3) %>% 
  summarise(n=n())
outcome_heat_hi99_2_no_outliers %>% 
  group_by(biome_name_freq,ruca_cat3) %>% 
  summarise(n=n())


outcome_wf_no_outliers %>% 
  ggplot(aes(x=imperv_prop,y=biome_name_freq))+
  geom_boxplot()
outcome_wf_no_outliers %>% 
  ggplot(aes(x=imperv_prop,y=ruca_cat3))+
  geom_boxplot()

outcome_wf_no_outliers %>% 
  ggplot(aes(x=tree_canopy_prop,y=biome_name_freq))+
  geom_boxplot()
outcome_wf_no_outliers %>% 
  ggplot(aes(x=tree_canopy_prop,y=ruca_cat3))+
  geom_boxplot()


#Initial thinking on scenarios------
#Test the idea on a dataset where impervious surfaces is raised to the 80th percentile
#for that RUCA
outcome_wf_no_outliers %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat3, y=imperv_prop))+
  geom_boxplot()

outcome_wf_no_outliers %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat3, y=imperv_prop))+
  geom_boxplot()

#It doesn't seem to vary that much by metropolitan area
outcome_wf_no_outliers %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat3, y=imperv_prop))+
  geom_boxplot()+
  facet_grid(rows="ruca_cat3")

#impervious surfaces should be relative to biome. Not necessarily relative to ruca, as there some
#cities with high green within biome
#Note the strategy may differ for AC, 
#so I don't know if we can simply use same strategy.
#Yea, less AC use in temperate conifer forests. That said, many do have 100% AC.

outcome_wf_no_outliers %>% 
  filter(rd_quo_outlier==0) %>% 
  ggplot(aes(x=ruca_cat3, y=ac_prop))+
  geom_boxplot()


summary(outcome_wf_no_outliers$imperv_prop)

#Data management before scenarios------
#Previously, I took a step to reduce the size of the dataset
#before putting it through the scenario model.
#That's a good idea. I can also combine the two datasets and then select the

#For scenarios
outcomes_wf_heat_for_scenarios=outcome_heat_hi99_2_no_outliers %>% 
  bind_rows(outcome_wf_no_outliers) %>% 
  dplyr::select(zcta,
                contains("outcome"),
                contains("rd_quo"),
                contains("n_cases_diff"),#would like to keep this around
                contains("tree"),contains("imperv"),
                contains("insured"),
                contains("ruca"),contains("biome"),
                contains("pop")#may need for weighting
                )

outcomes_wf_heat_for_scenarios %>% 
  group_by(outcome) %>% 
  summarise(n=n())
  
#Now separate them again
outcome_heat_for_scenarios=outcomes_wf_heat_for_scenarios %>% 
  filter(outcome=="heat")

outcome_wf_for_scenarios=outcomes_wf_heat_for_scenarios %>% 
  filter(outcome=="wf")



# Test a scenario------
## Define alternative value------
# Test a scenario for heat & imperv
#For imperv, lower is better, so set intervention as those above it
#consider systematicizing this in a function later as compared with tree.
#For simplicity, should I just inverse it?
#No, I'd say flip it later if needed for simplicity (eg 80th percentile good)
#But changing it would also require changing it upstream when the models are fit.


test_heat_imperv= outcomes_wf_heat_for_scenarios %>% 
  filter(outcome=="heat") %>% 
  mutate( target_percentile=.2) %>% 
  group_by(ruca_cat3) %>% 
  mutate(
    imperv_prop_alt=quantile(
      imperv_prop,
      probs=target_percentile,na.rm=T
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    zcta_intervene=case_when(
      imperv_prop>imperv_prop_alt~1,
      TRUE~0
    )
  )
test_heat_imperv


#Now, predict values using the predict function. Easier this way than doing it manually
#because it will provide standard errors and consider all variables in the model
## Predict outcome under alternative value------
test_heat_imperv_intervene=test_heat_imperv %>% 
  filter(zcta_intervene==1) %>% 
  #the target value needs to have the same variable name as the main exposure
  #variable for the predict function to work, so overwrite this:
  mutate(
    imperv_prop_original=imperv_prop,
    imperv_prop=imperv_prop_alt)

#test_heat_imperv_intervene %>% View()
  

#The model, from the other script, is
summary(glm_heat_imp_tree_i_r_b) #final. both exposures
summary(glm_wf_imp_tree_i_r_b)
pred_heat_imperv_80= as_tibble(
  stats::predict(
    glm_heat_imp_tree_i_r_b,#the model
    test_heat_imperv_intervene, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    rd_alt_pt=fit,
    rd_alt_se=se.fit,
    residual_scale_alt=residual.scale
  )

nrow(pred_heat_imperv_80)
nrow(test_heat_imperv_intervene)

#also model the baseline so it can be compared
test_heat_imperv_intervene_quo=test_heat_imperv_intervene %>% 
  dplyr::select(-imperv_prop) %>% 
  rename(imperv_prop=imperv_prop_original)

pred_heat_imperv_quo=as_tibble(
  stats::predict(
    glm_heat_imp_tree_i_r_b,#the model
    test_heat_imperv_intervene_quo, #input dataset
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
compare_heat_imperv_scenario_80=test_heat_imperv_intervene %>% 
  bind_cols(pred_heat_imperv_80) %>% 
  bind_cols(pred_heat_imperv_quo) %>% 
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

#compare_heat_imperv_scenario_80 %>% View()

#usually this number will be negative because we expect the RD
#to be SMALLER under the alternative scenario, that is, that
#heat will have a SMALLER effect
#for example
3-5
#alternative - quo
compare_heat_imperv_scenario_80 %>% 
  ggplot(aes(x=rd_diff_act_pt))+
  geom_histogram()

compare_heat_imperv_scenario_80 %>% 
  ggplot(aes(x=rd_diff_pred_pt))+
  geom_histogram()

#compare_heat_imperv_scenario_80 %>% View()


#stratify RD diff by RUCA
compare_heat_imperv_scenario_80 %>% 
  ggplot(aes(x=ruca_cat3,y=rd_diff_act_pt))+
  geom_boxplot()

compare_heat_imperv_scenario_80 %>% 
  ggplot(aes(x=ruca_cat3,y=rd_quo_pt)) +
  geom_boxplot()

test_heat_imperv_intervene %>% 
  group_by(ruca_cat3) %>% 
  summarise(
    rd_quo_pt_sum=sum(rd_quo_pt,na.rm=T),
    rd_quo_pt_med=median(rd_quo_pt,na.rm=T)
  ) 
  
#t for target; b fr quo
#can we just sum rate differences? I'm not so sure. Probably better to re-calculate
#from scratch
compare_heat_imperv_scenario_80 %>% 
  group_by(ruca_cat3) %>% 
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

# Define scenarios -------
#July 15, 2025: define residuals as well
#Oct 23, 2025: two outcomes, two exposures
#The prediction of baseline values has to be different
#because the models are different.
## outcome: heat-------

### Predict baseline values-----
#Can be the same model for imp and tree as both are in the model
#Baseline predicted values for each zcta
pred_heat_scenario_quo=as_tibble(
  stats::predict(
    glm_heat_imp_tree_i_r_b,#the final model
    outcome_heat_for_scenarios, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    #pred for model-predicted status quo (baseline) value
    rd_quo_pred_pt=fit,
    rd_quo_pred_se=se.fit,
    residual_scale_quo=residual.scale
  ) %>% 
  bind_cols(outcome_heat_for_scenarios) %>% 
  mutate(
    resid_diff =rd_quo_pt-rd_quo_pred_pt, 
    ratio_estimate_resid=rd_quo_pred_pt/resid_diff
  ) %>% 
  dplyr::select(zcta,contains("rd_quo_pred"),
                contains("resid")
  )



#Take a look at residuals

pred_heat_scenario_quo %>% 
  ggplot(aes(x=resid_diff))+
  geom_histogram()

# summarize residuals
rsd_heat=pred_heat_scenario_quo %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(rsd=sd(resid_diff,na.rm=T)) %>% 
  ungroup()



#a lookup for the residuals to link
lookup_zcta_heat_resid_diff=pred_heat_scenario_quo %>% 
  dplyr::select(zcta,resid_diff)


## Run function both for imp and tree
### heat & imperv--------
# Define function
#a different function for heat and tree because the
#group-by is different and the definition of the target is different

names(outcome_heat_for_scenarios)
define_scenario_pt_heat_imperv=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_heat_imperv_for_model= outcome_heat_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(ruca_cat3) %>% 
    mutate(
      imperv_prop_alt=quantile(
        imperv_prop,
        probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      #Again, the sign is flipped for imperv vs tree
      #greater than here, less than for tree
      zcta_intervene=case_when(
        imperv_prop>imperv_prop_alt~1,
        TRUE~0
      ),
      
      #Because the old model expects to see the variable name,
     # "imperv_prop", #in the model, we have to rename
      #the alternative value to that value. So do:

    #the target value needs to have the same variable name as the main exposure
    #variable for the predict function to work, so overwrite this:
      imperv_prop_original=imperv_prop,
      imperv_prop=imperv_prop_alt
      ) 

  #and then we only predict values in the filtered data use the filtered data in the model  
  #and then we're only fitting the model in the filtered dataset
  
  input_df_heat_imperv_for_model_filtered=input_df_heat_imperv_for_model %>% 
      filter(zcta_intervene==1)  

    #Model the alternative RD
    pred_heat_imperv=as_tibble(
      stats::predict(
        glm_heat_imp_tree_i_r_b,#the model
        input_df_heat_imperv_for_model_filtered, #input dataset
        type = "response",
        se.fit = TRUE
      )) %>% 
      rename(
        rd_alt_pt=fit,
        rd_alt_se=se.fit,
        residual_scale_alt=residual.scale
      )

    #merge it back together
    input_df_filtered_zcta_only=input_df_heat_imperv_for_model_filtered %>% 
      dplyr::select(zcta)

    #append the zcta to the modeled data
    scenario_heat_imperv_merged_filtered=input_df_filtered_zcta_only %>% 
      bind_cols(pred_heat_imperv) 
    
    #and then left join the modeled data back with the unfiltered data
    scenario_heat_imperv_merged_df=input_df_heat_imperv_for_model %>% 
      left_join(scenario_heat_imperv_merged_filtered,by="zcta") %>% 
      #and get the exposure data back with the right terms
      dplyr::select(-imperv_prop) %>% 
      mutate(
        imperv_prop=imperv_prop_original,
      ) %>% 
      #organize data
      dplyr::select(
        zcta,
        contains("imperv"),
        contains("canopy"),
        contains("rd_quo"),contains("_alt"),everything()
      )
    

    return(scenario_heat_imperv_merged_df)

}

#test function
test_heat_imperv=define_scenario_pt_heat_imperv(.5)

test_heat_imperv %>% 
  ggplot(aes(x=rd_quo_pt)) +
  geom_histogram()

test_heat_imperv %>% 
  ggplot(aes(x=rd_alt_pt)) +
  geom_histogram()



### heat & tree-----
#same function. Note the sign needs to be flipped. We're also grouping by a different
#variable here
names(outcome_heat_for_scenarios)
define_scenario_pt_heat_tree=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_heat_tree_for_model= outcome_heat_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(biome_name_freq) %>% 
    mutate(
      tree_canopy_prop_sqrt_alt=quantile(
        tree_canopy_prop_sqrt,
        probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      #Again, the sign is flipped for tree vs tree
      #greater than here, less than for tree
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
  
  input_df_heat_tree_for_model_filtered=input_df_heat_tree_for_model %>% 
    filter(zcta_intervene==1)  
  
  #Model the alternative RD
  pred_heat_tree=as_tibble(
    stats::predict(
      glm_heat_imp_tree_i_r_b,#the model
      input_df_heat_tree_for_model_filtered, #input dataset
      type = "response",
      se.fit = TRUE
    )) %>% 
    rename(
      rd_alt_pt=fit,
      rd_alt_se=se.fit,
      residual_scale_alt=residual.scale
    )
  
  #merge it back together
  input_df_filtered_zcta_only=input_df_heat_tree_for_model_filtered %>% 
    dplyr::select(zcta)
  
  #append the zcta to the modeled data
  scenario_heat_tree_merged_filtered=input_df_filtered_zcta_only %>% 
    bind_cols(pred_heat_tree) 
  
  #and then left join the modeled data back with the unfiltered data
  scenario_heat_tree_merged_df=input_df_heat_tree_for_model %>% 
    left_join(scenario_heat_tree_merged_filtered,by="zcta") %>% 
    #and get the exposure data back with the right terms
    dplyr::select(-tree_canopy_prop_sqrt) %>% 
    mutate(
      tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_original,
    ) %>% 
    #organize data
    dplyr::select(
      zcta,
      contains("imp"),
      contains("canopy"),
      contains("rd_quo"),contains("_alt"),everything()
    )
  
  
  return(scenario_heat_tree_merged_df)
  
}

test_heat_tree=define_scenario_pt_heat_tree(.5)
test_heat_tree=define_scenario_pt_heat_tree(.8)

test_heat_tree %>% 
  ggplot(aes(x=rd_quo_pt)) +
  geom_histogram()

test_heat_tree %>% 
  ggplot(aes(x=rd_alt_pt)) +
  geom_histogram()


## outcome: wf-------
### predict baseline----------
pred_wf_scenario_quo=as_tibble(
  stats::predict(
    glm_wf_imp_tree_i_r_b,#the final model
    outcome_wf_for_scenarios, #input dataset
    type = "response",
    se.fit = TRUE
  )) %>% 
  rename(
    #pred for model-predicted status quo (baseline) value
    rd_quo_pred_pt=fit,
    rd_quo_pred_se=se.fit,
    residual_scale_quo=residual.scale
  ) %>% 
  bind_cols(outcome_wf_for_scenarios) %>% 
  mutate(
    resid_diff =rd_quo_pt-rd_quo_pred_pt, 
    ratio_estimate_resid=rd_quo_pred_pt/resid_diff
  ) %>% 
  dplyr::select(zcta,contains("rd_quo_pred"),
                contains("resid")
  )

pred_wf_scenario_quo

pred_wf_scenario_quo %>% 
  ggplot(aes(x=resid_diff))+
  geom_histogram()

# summarize residuals
rsd_wf=pred_wf_scenario_quo %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(rsd=sd(resid_diff,na.rm=T)) %>% 
  ungroup()



#a lookup for the residuals to link
lookup_zcta_wf_resid_diff=pred_wf_scenario_quo %>% 
  dplyr::select(zcta,resid_diff)


### wf & imperv--------
# Define function
#a different function for wf and tree because the
#group-by is different and the definition of the target is different

names(outcome_wf_for_scenarios)
define_scenario_pt_wf_imperv=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_wf_imperv_for_model= outcome_wf_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(ruca_cat3) %>% 
    mutate(
      imperv_prop_alt=quantile(
        imperv_prop,
        probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      #Again, the sign is flipped for imperv vs tree
      #greater than here, less than for tree
      zcta_intervene=case_when(
        imperv_prop>imperv_prop_alt~1,
        TRUE~0
      ),
      
      #Because the old model expects to see the variable name,
      # "imperv_prop", #in the model, we have to rename
      #the alternative value to that value. So do:
      
      #the target value needs to have the same variable name as the main exposure
      #variable for the predict function to work, so overwrite this:
      imperv_prop_original=imperv_prop,
      imperv_prop=imperv_prop_alt
    ) 
  
  #and then we only predict values in the filtered data use the filtered data in the model  
  #and then we're only fitting the model in the filtered dataset
  
  input_df_wf_imperv_for_model_filtered=input_df_wf_imperv_for_model %>% 
    filter(zcta_intervene==1)  
  
  #Model the alternative RD
  pred_wf_imperv=as_tibble(
    stats::predict(
      glm_wf_imp_tree_i_r_b,#the model
      input_df_wf_imperv_for_model_filtered, #input dataset
      type = "response",
      se.fit = TRUE
    )) %>% 
    rename(
      rd_alt_pt=fit,
      rd_alt_se=se.fit,
      residual_scale_alt=residual.scale
    )
  
  #merge it back together
  input_df_filtered_zcta_only=input_df_wf_imperv_for_model_filtered %>% 
    dplyr::select(zcta)
  
  #append the zcta to the modeled data
  scenario_wf_imperv_merged_filtered=input_df_filtered_zcta_only %>% 
    bind_cols(pred_wf_imperv) 
  
  #and then left join the modeled data back with the unfiltered data
  scenario_wf_imperv_merged_df=input_df_wf_imperv_for_model %>% 
    left_join(scenario_wf_imperv_merged_filtered,by="zcta") %>% 
    #and get the exposure data back with the right terms
    dplyr::select(-imperv_prop) %>% 
    mutate(
      imperv_prop=imperv_prop_original,
    ) %>% 
    #organize data
    dplyr::select(
      zcta,
      contains("imperv"),
      contains("canopy"),
      contains("rd_quo"),contains("_alt"),everything()
    )
  
  
  return(scenario_wf_imperv_merged_df)
  
}

#test function
test_wf_imperv=define_scenario_pt_wf_imperv(.5)

test_wf_imperv %>% 
  ggplot(aes(x=rd_quo_pt)) +
  geom_histogram()

test_wf_imperv %>% 
  ggplot(aes(x=rd_alt_pt)) +
  geom_histogram()



### wf & tree-----
#same function. Note the sign needs to be flipped. We're also grouping by a different
#variable here
names(outcome_wf_for_scenarios)
define_scenario_pt_wf_tree=function(target_percentile_val){
  
  #Define the intervention zip codes
  input_df_wf_tree_for_model= outcome_wf_for_scenarios %>% 
    #Note the value is a proportion (eg 0.5), not 50
    mutate(target_percentile=target_percentile_val) %>% 
    group_by(biome_name_freq) %>% 
    mutate(
      tree_canopy_prop_sqrt_alt=quantile(
        tree_canopy_prop_sqrt,
        probs=target_percentile,na.rm=T
      )
    ) %>% 
    ungroup() %>% 
    mutate(
      #Again, the sign is flipped for tree vs tree
      #greater than here, less than for tree
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
  
  input_df_wf_tree_for_model_filtered=input_df_wf_tree_for_model %>% 
    filter(zcta_intervene==1)  
  
  #Model the alternative RD
  pred_wf_tree=as_tibble(
    stats::predict(
      glm_wf_imp_tree_i_r_b,#the model
      input_df_wf_tree_for_model_filtered, #input dataset
      type = "response",
      se.fit = TRUE
    )) %>% 
    rename(
      rd_alt_pt=fit,
      rd_alt_se=se.fit,
      residual_scale_alt=residual.scale
    )
  
  #merge it back together
  input_df_filtered_zcta_only=input_df_wf_tree_for_model_filtered %>% 
    dplyr::select(zcta)
  
  #append the zcta to the modeled data
  scenario_wf_tree_merged_filtered=input_df_filtered_zcta_only %>% 
    bind_cols(pred_wf_tree) 
  
  #and then left join the modeled data back with the unfiltered data
  scenario_wf_tree_merged_df=input_df_wf_tree_for_model %>% 
    left_join(scenario_wf_tree_merged_filtered,by="zcta") %>% 
    #and get the exposure data back with the right terms
    dplyr::select(-tree_canopy_prop_sqrt) %>% 
    mutate(
      tree_canopy_prop_sqrt=tree_canopy_prop_sqrt_original,
    ) %>% 
    #organize data
    dplyr::select(
      zcta,
      contains("imp"),
      contains("canopy"),
      contains("rd_quo"),contains("_alt"),everything()
    )
  
  
  return(scenario_wf_tree_merged_df)
  
}

test_wf_tree=define_scenario_pt_wf_tree(.5)
test_wf_tree=define_scenario_pt_wf_tree(.8)

test_wf_tree %>% 
  ggplot(aes(x=rd_quo_pt)) +
  geom_histogram()

test_wf_tree %>% 
  ggplot(aes(x=rd_alt_pt)) +
  geom_histogram()




## Run each function over all target percentiles----------
#and combine with the other data
#Run it ten times and combine data
#zero to 1, 0 options
#For testing use fewer, e.g., by .2 rather than by .1
target_percentile_vector =seq(0, 1, by=.2)

#This applies in all scenarios so
mutate_n_cases_diff=function(df){
  df %>% 
  mutate(
    #some calculations that apply everywhere
    #number of cases per day under the predicted baseline scenario
    n_cases_diff_quo_pred_pt=rd_quo_pred_pt*pop,  
    n_cases_diff_quo_pred_se=rd_quo_pred_se*pop,  
    
    #number of cases per day without respect to population
    n_cases_diff_alt_pt=rd_alt_pt*pop, 
    n_cases_diff_alt_se=rd_alt_se*pop 
  )

}

### heat imperv------
#oh! also link in the predicted status-quo values
heat_imperv_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_heat_imperv) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_heat_scenario_quo,by="zcta") %>% 
  mutate_n_cases_diff() %>% 
  #keep track of intervention variable for when I bring them together
  mutate(intervene_var="imperv")



#how many obs per target percentile
#done. This is working as expected now.
heat_imperv_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)

### heat tree-----
heat_tree_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_heat_tree) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_heat_scenario_quo,by="zcta") %>% 
  mutate_n_cases_diff() %>% 
  #keep track of intervention variable for when I bring them together
  mutate(intervene_var="tree")

names(heat_tree_scenarios_all_pt)
heat_tree_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)

heat_imperv_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)



### wf imperv-----
wf_imperv_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_wf_imperv) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_wf_scenario_quo,by="zcta") %>% 
  mutate_n_cases_diff() %>% 
  #keep track of intervention variable for when I bring them together
  mutate(intervene_var="imperv")


wf_imperv_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)


### wf tree-----
wf_tree_scenarios_all_pt=target_percentile_vector %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_wf_tree) %>% 
  list_rbind() %>% 
  #bring in the predicted status-quo values
  left_join(pred_wf_scenario_quo,by="zcta") %>% 
  mutate_n_cases_diff() %>% 
  mutate(intervene_var="tree")


wf_imperv_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)
wf_tree_scenarios_all_pt %>% 
  group_by(target_percentile,zcta_intervene) %>% 
  summarise(n=n()) %>% 
  print(n=100)


### Combine them together-----
#For later summary, it will be easier to just have them all together.
#I have outcome, intervention variable, and the rest
scenarios_all_pt=heat_imperv_scenarios_all_pt %>% 
  bind_rows(
    heat_tree_scenarios_all_pt,
    wf_imperv_scenarios_all_pt,
    wf_tree_scenarios_all_pt
  ) %>% 
  #For summary purposes, it'd be nice to have the scenarios all in the same
  #direction. Meaning for imp surfaces I may consider flipping the scale
  #How about
  mutate(
    target_percentile_unified=case_when(
      intervene_var=="imperv"~1-target_percentile,
      TRUE~target_percentile
    )
  ) %>% 
  dplyr::select(zcta,outcome,intervene_var,zcta_intervene,
                contains("target_percentile"),everything())

names(scenarios_all_pt)
scenarios_all_pt %>% 
  dplyr::select(zcta,outcome,intervene_var,contains("target_percentile")) %>% 
  View()


# Boostrap confidence intervals----------

#Bootstrap the confidence intervals using modeled standard errors
#This single function should work everywhere

## Some data management beforehand----
#write a function and run it 1,000 times
names(heat_imperv_scenarios_all_pt)
heat_imperv_scenarios_all_for_boot=heat_imperv_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,intervene_var, outcome,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_heat)

heat_imperv_scenarios_all_for_boot



heat_tree_scenarios_all_for_boot=heat_tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,intervene_var,outcome,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_heat)

heat_tree_scenarios_all_for_boot


wf_imperv_scenarios_all_for_boot=wf_imperv_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,intervene_var,outcome,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_wf)

wf_imperv_scenarios_all_for_boot



wf_tree_scenarios_all_for_boot=wf_tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,intervene_var,outcome,
                rd_quo_pred_pt,#the modeled status-quo result
                rd_quo_pred_se, #standard error for modeled status-quo result
                contains("rd_alt")) %>% 
  bind_cols(rsd_wf)

wf_tree_scenarios_all_for_boot




## The function------
#Write it more generally so I can drop in any dataset:
scenario_boot=function(s_id_val,input_data_for_boot){
  
  out=input_data_for_boot %>% 
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


#link in everything else
library(purrr)
## Define boot reps and do boostrapping-------
#100 goes pretty quick. 1,000 takes a bit.
#Let's go 500 so I don't break my computer
n_boot_reps=500

## Boostrap each scenario-------
### heat & imperv-------
#Note this revision allows me to generalize for any dataset

heat_imperv_scenario_boot_df=1:n_boot_reps %>% 
  map_dfr(
    ~scenario_boot (    #the function
        heat_imperv_scenarios_all_for_boot,# the name of the input data function
        s_id_val = .x #the thing to iterate over using same dataset
          )
  )

heat_imperv_scenario_boot_df

### heat & tree-------
heat_tree_scenario_boot_df=1:n_boot_reps %>% 
  map_dfr(
    ~scenario_boot (    #the function
      heat_tree_scenarios_all_for_boot,# the name of the input data function
      s_id_val = .x #the thing to iterate over using same dataset
    )
  )

heat_tree_scenario_boot_df

### wf & imperv-------
wf_imperv_scenario_boot_df=1:n_boot_reps %>% 
  map_dfr(
    ~scenario_boot (    #the function
      wf_imperv_scenarios_all_for_boot,# the name of the input data function
      s_id_val = .x #the thing to iterate over using same dataset
    )
  )

wf_imperv_scenario_boot_df

### wf & tree-------
wf_tree_scenario_boot_df=1:n_boot_reps %>% 
  map_dfr(
    ~scenario_boot (    #the function
      wf_tree_scenarios_all_for_boot,# the name of the input data function
      s_id_val = .x #the thing to iterate over using same dataset
    )
  )

wf_tree_scenario_boot_df










## wrangle bootstrap reps------
#I need population
setwd(here("data-processed"))
load("lookup_pop_zcta.RData")
lookup_pop_zcta
lookup_zcta_ca_biome

#need intervention status for each
lookup_zcta_intervene_heat_imperv_scenario=heat_imperv_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)

lookup_zcta_intervene_heat_tree_scenario=heat_tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)

lookup_zcta_intervene_wf_imperv_scenario=wf_imperv_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)

lookup_zcta_intervene_wf_tree_scenario=wf_tree_scenarios_all_pt %>% 
  dplyr::select(zcta,target_percentile,zcta_intervene)


#Write a function that I do for every boostrap dataset
wrangle_boot_df=function(df){
  df %>% 
    left_join(lookup_pop_zcta,by="zcta") %>% 
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
}

### heat imperv----
heat_imperv_scenario_boot_df_wrangle=heat_imperv_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_zcta_intervene_heat_imperv_scenario,
            by=c("zcta","target_percentile")) %>% 
  wrangle_boot_df()

names(heat_imperv_scenario_boot_df_wrangle)

### heat tree----
heat_tree_scenario_boot_df_wrangle=heat_tree_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_zcta_intervene_heat_tree_scenario,
            by=c("zcta","target_percentile")) %>% 
  wrangle_boot_df()

### wf imperv----
wf_imperv_scenario_boot_df_wrangle=wf_imperv_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_zcta_intervene_wf_imperv_scenario,
            by=c("zcta","target_percentile")) %>% 
  wrangle_boot_df()


### wf tree----
wf_tree_scenario_boot_df_wrangle=wf_tree_scenario_boot_df %>% 
  #do some calculations on this dataset to back-calculate n cases diff.
  #Do it after the bootstrap operation so it's easier to do it dynamically
  left_join(lookup_zcta_intervene_wf_tree_scenario,
            by=c("zcta","target_percentile")) %>% 
  wrangle_boot_df()

names(wf_tree_scenario_boot_df_wrangle)

### Combine them together-----
#as with the point estimates..
scenarios_all_boot_df=heat_imperv_scenario_boot_df_wrangle %>% 
  bind_rows(
    heat_tree_scenario_boot_df_wrangle,
    wf_imperv_scenario_boot_df_wrangle,
    wf_tree_scenario_boot_df_wrangle
  ) %>% 
  #same as above. make a new target percentile variable
  #so it's the same in terms of desirability for imperv and tree
  mutate(
    target_percentile_unified=case_when(
      intervene_var=="imperv"~1-target_percentile,
      TRUE~target_percentile
    )
  ) %>% 
  dplyr::select(zcta,outcome,intervene_var,zcta_intervene,
                contains("target_percent"),
                everything())

names(scenarios_all_boot_df)
nrow(scenarios_all_boot_df)
head(scenarios_all_boot_df)



#continued
#scripts/predict-values-alt-summary.r
  