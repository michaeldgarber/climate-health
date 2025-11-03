#Summarizing the predicted alternatives
#Leaving in one code was making it too long

library(here)
#source(here("scripts","predict-values-alt.R"))
scenarios_all_pt

#boot reps
scenarios_all_boot_df

#calculate summary rate differences

names(heat_imperv_scenarios_all_pt)


# Summary functions---------
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
      #for comparison with below
      rd_alt_pt_mean_wt=weighted.mean(x=rd_alt_pt,w=pop,na.rm=T), 
      
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

#add this for the figures
mutate_label_outcome_intervene_var=function(df){
  df %>% 
    mutate(
      outcome_label=case_when(
        outcome=="heat"~ "Heat",
        outcome=="wf" ~ "Wildfire"
  ),
  intervene_var_label=case_when(
    intervene_var=="imperv" ~ "Imperv. surfaces",
    intervene_var=="tree" ~ "Tree canopy",
    )
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


# Point estimates--------
## overall-------
scenarios_all_pt
## by outcome, intervention, and target percentile
scenarios_summary_overall=scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  #add this for the figures
  mutate_label_outcome_intervene_var()


names(scenarios_summary_overall)
scenarios_summary_overall %>% 
  dplyr::select(outcome,intervene_var,target_percentile_unified,
                contains("rd_100k_diff"))

setwd(here("data-processed"))
save(scenarios_summary_overall,
     file="scenarios_summary_overall.RData")

## by RUCA------
scenarios_summary_ruca_cat3=scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,ruca_cat3) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate_label_outcome_intervene_var()

scenarios_summary_ruca_cat3 %>% View()

#assess variabilty by RUCA
scenarios_summary_ruca_cat3 %>% 
  filter(target_percentile_unified==.8) %>% 
  View()

## by biome------
scenarios_summary_biome_name_freq=scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,biome_name_freq) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() %>% 
  mutate_label_outcome_intervene_var()


## by zcta------
scenarios_summary_zcta=scenarios_all_pt %>% 
  filter(zcta_intervene==1) %>% 
  group_by(zcta_intervene,outcome,intervene_var,target_percentile_unified,zcta) %>% 
  summarise_ungroup_n_cases_diff_rd_pop() %>% 
  mutate_rd_diff() 

#scenarios_summary_zcta %>% View()

#need one for a map as well
#Keep everything and add biome and RUCA for information
zcta_ruca2010
names(scenarios_summary_zcta)
lookup_scenario_intervene_var_value_outcome_zcta
nrow(lookup_scenario_intervene_var_value_outcome_zcta)
nrow(scenarios_summary_zcta)
scenarios_summary_zcta_for_map=scenarios_summary_zcta %>% 
  left_join(lookup_zcta_ca_biome, by ="zcta") %>% 
  left_join(zcta_ruca2010,by="zcta") %>% 
  #I also need some information on the actual and target values
  #by zip code, which I have here
  left_join(lookup_scenario_intervene_var_value_outcome_zcta,by=
              c("zcta","intervene_var", "outcome",
                "target_percentile_unified")) %>% 
  #and then limit to these. Basically just omit a few
  #What this doesn't have the old one had is information
  #on the actual value for intervention measure
  dplyr::select(
    -contains("n_zcta"),
    -contains("_act_")
  )
  

scenarios_summary_zcta_for_map %>% View()
#see previous version of this code for the variables
#That included in the summary _for_map

#save
setwd(here("data-processed"))
save(scenarios_summary_zcta_for_map,
     file="scenarios_summary_zcta_for_map.RData")


# Confidence intervals------
## Write functions-------
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


## Calculate CIs-------
###  overall--------
scenarios_summary_overall_ci=scenarios_all_boot_df %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(outcome,intervene_var,target_percentile_unified) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(scenarios_summary_overall_ci,
     file="scenarios_summary_overall_ci.RData")

scenarios_summary_overall_ci %>% View()

### by ruca_cat3---------
scenarios_summary_ruca_cat3_ci=scenarios_all_boot_df %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,ruca_cat3,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(outcome,intervene_var,target_percentile_unified,ruca_cat3) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(scenarios_summary_ruca_cat3_ci,
     file="scenarios_summary_ruca_cat3_ci.RData")
scenarios_summary_ruca_cat3_ci %>% View()

### by biome---------
scenarios_summary_biome_name_freq_ci=scenarios_all_boot_df %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,biome_name_freq,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(outcome,intervene_var,target_percentile_unified,biome_name_freq) %>% 
  summarise_create_confidence_intervals()

setwd(here("data-processed"))
save(scenarios_summary_biome_name_freq_ci,
     file="scenarios_summary_biome_name_freq_ci.RData")

### by zcta------
#this one takes a while.
scenarios_summary_zcta_ci=scenarios_all_boot_df %>% 
  filter(zcta_intervene==1) %>% 
  group_by(outcome,intervene_var,target_percentile_unified,zcta,s_id) %>% 
  summarise_ungroup_n_cases_diff_rd_pop_boot() %>% 
  mutate_rd_diff_boot() %>%  #this gets a summary for each s_id
  #then collapse over s_id for conf intervals
  group_by(outcome,intervene_var,target_percentile_unified,zcta) %>% 
  summarise_create_confidence_intervals()



# combine point with CI------
#Do this later. Before I had said that I'd only pick these:
#and only pick those that will be used for shiny
# dplyr::select(
#   intervene_var,target_percentile_unified,ruca_cat3,n_zcta, pop,
#   
#   contains("rd_100k_quo_pred"), 
#   contains("rd_100k_alt"),
#   contains("rd_100k_diff_pred"),
#   
#   contains("n_cases_diff_quo_pred"),
#   contains("n_cases_diff_alt"),
#   contains("n_cases_diff_diff_pred")

## overall -----
scenarios_summary_overall_pt_ci=scenarios_summary_overall %>% 
  left_join(scenarios_summary_overall_ci,
            by= c("outcome","intervene_var","target_percentile_unified")
  )

setwd(here("data-processed"))
save(scenarios_summary_overall_pt_ci,
     file="scenarios_summary_overall_pt_ci.RData")


scenarios_summary_overall_pt_ci %>% 
  dplyr::select(outcome,intervene_var,target_percentile_unified,
                contains("rd_100k_diff"))



## by RUCA------
scenarios_summary_ruca_cat3_pt_ci=scenarios_summary_ruca_cat3 %>% 
  left_join(scenarios_summary_ruca_cat3_ci,
            by=c("outcome","intervene_var","target_percentile_unified",
                                                 "ruca_cat3")
  )

setwd(here("data-processed"))
save(scenarios_summary_ruca_cat3_pt_ci,
     file="scenarios_summary_ruca_cat3_pt_ci.RData")


scenarios_summary_ruca_cat3_pt_ci %>% 
  dplyr::select(outcome,intervene_var,target_percentile_unified,
                ruca_cat3,
                contains("rd_100k_diff"))


## by biome_name_freq------
scenarios_summary_biome_name_freq_pt_ci=scenarios_summary_biome_name_freq %>% 
  left_join(scenarios_summary_biome_name_freq_ci,
            by=c("outcome","intervene_var","target_percentile_unified",
                 "biome_name_freq")
  )

setwd(here("data-processed"))
save(scenarios_summary_biome_name_freq_pt_ci,
     file="scenarios_summary_biome_name_freq_pt_ci.RData")


scenarios_summary_biome_name_freq_pt_ci %>% 
  dplyr::select(outcome,intervene_var,target_percentile_unified,
                biome_name_freq,
                contains("rd_100k_diff"))

names(scenarios_summary_biome_name_freq_pt_ci)

### by zcta------
