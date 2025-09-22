#Define scenarios
#In this code, we define the scenario and write functions to iterate
#over several scenarios including uncertainty
#Began Jan 21, 2025
#Revised May 21, 2025 to focus on air conditioning and tree canopy.
#Old scripts in old folder

#Revise this to be more general, including all of the effect modifiers
#loaded in the previous script
#Assume that this is run
library(here)
source(here("scripts","read-wrangle-eff-estimates.R"))


# Define some scenarios-----
#Note wildfire is defined dichotomously as  days with wildfire PM2.5 ≥ 15 μg/m3

#Scenario 1: Bring everyone's AC up to the median (median=0.625)
#How is the distribution of risk differences affected by raising every 
#zip-code's AC prevalence up to the median?

#Scenario 2: Bring everyone's AC up to the 75th percentile
#Scenario 3: Bring everyone's AC up to 100%

#Scenario: baseline: how many cases caused by wildfire?
#It's risk difference, so we can add them up

#The scenario says...if we raised the AC prevalence of those below x to x,
#then this is what their effect estimate would be, assuming a constant this 
#eff-modification measure is a global measure over all of CA
#This isn't controlling for anything, I don't believe, so
#There's a chance the effect of AC on the effect measure could be confounded.
#The goal is to REDUCE the effect of wildfires

#Note rate difference is per 100k



# scenario_target_ac_50th=ac_50th,
# scenario_target_ac_75th=ac_75th,
# #100th percentile is also 100% here, but use the variable name to soft-code
# scenario_target_ac_100th=ac_100th
#Feb 19, 2025: deleting old code that tested the function

#look up ac by zcta among these zctas to present in some vis
# lookup_ac_zcta=wf_emm_zcta %>% 
#   distinct(ac_prop,zcta)
# 
# lookup_ac_zcta
# setwd(here("data-processed"))
# save(lookup_ac_zcta,file="lookup_ac_zcta.RData")



## Write a function (point estimate only)-------
#This takes the target percentile as the input (eg the median)
#and then raises the air conditioning of every zcta up to that value
#Feb 18, 2024: df used to be a part of the function
#(ie df, target_percentile_val)
#but it makes iterating over the function more challenging

#April 17, 2025: writing this more generally now.
#It can be done over all measures by grouping by measure, I believe
names(wf_emm_zcta)
define_scenario_pt_only=function(target_percentile_val){
  
  #Feb 18, 2024:
  #To make this easier to iterate over several times,
  #include the dataset as part of the function
  out=wf_emm_zcta %>% 

    #define the value corresponding to the xth percentile
    #This method does so using group-by but without summarise
    #Note the value is a proportion (eg 0.5), not 50
    mutate(
      target_percentile=target_percentile_val) %>% 
    group_by(emm_measure,target_percentile) %>% 
    mutate(
      emm_target=quantile(emm_value,probs=target_percentile_val,na.rm=T)
    ) %>% 
    ungroup() %>% 
    mutate(
      zcta_intervene=case_when(
        emm_value<emm_target~1,
        TRUE~0
      ),
      #The change in the ac proportion to get to that target
      emm_change=case_when(
        zcta_intervene==1~emm_target-emm_value,
        zcta_intervene==0~0
      ),
      #number of iqrs this change equates to
      emm_change_per_iqr=emm_change/iqr,
      
      #Now translate this change in IQRs into the new risk difference
      #Update: I can still have a target rd for those that don't
      #get intervention; it will just be the same as baseline
      rd_target_pt=case_when(
        zcta_intervene==1~    rd_baseline_pt+rd_per_iqr_pt*emm_change_per_iqr,
        zcta_intervene==0~    rd_baseline_pt
      ),
      
      #population intervened upon.
      #Returns zero if not intervened upon under that scenario
      pop_intervene=zcta_intervene*pop
    )
  
  return(out)
}

## check the function-------
wf_emm_zcta_scenario_50=define_scenario_pt_only(.5)
#works

wf_emm_zcta_scenario_50 %>% View()
names(wf_emm_zcta_scenario_50)
summary(wf_emm_zcta_scenario_50$rd_target_pt)
summary(wf_emm_zcta_scenario_50$rd_baseline_pt)


# Run the function using three distinct values-------
#Now try the function with a different target value
wf_emm_zcta_scenario_75=define_scenario_pt_only(.75)
summary(wf_emm_zcta_scenario_75$rd_target_pt)

wf_emm_zcta_scenario_100=define_scenario_pt_only(1)

summary(wf_emm_zcta_scenario_100$rd_target_pt)
wf_emm_zcta_scenario_100
#works

#stack them together
wf_emm_scenarios_all_pt_3_options=wf_emm_zcta_scenario_50 %>% 
  bind_rows(
    wf_emm_zcta_scenario_75,
    wf_emm_zcta_scenario_100
  )


names(wf_emm_scenarios_all_pt_3_options)
table(wf_emm_scenarios_all_pt_3_options$target_percentile)


# Run the function using 20 possible values------
#Allow the user to enter any of 20 possible values, 0 to 100 by 5s
100/5

#zero to 1, 20 options
zero_to_1_20 =seq(0, 1, by=.05)
zero_to_1_20

#20 options
#To make this easier, I need the dataset included in the function
#definition above

#
wf_emm_scenarios_all_pt=zero_to_1_20 %>% 
  #map and then list_rbind is the updated version of map_dfr
  #in purrr
  map(define_scenario_pt_only) %>% 
  list_rbind() %>% 
  #Some mutates used for visualization later.
  mutate(
    zcta_intervene_char=as.character(zcta_intervene),
    
    #sometimes I want this as yes/no
    zcta_intervene_char_yn=case_when(
      zcta_intervene_char==1~"Yes",
      zcta_intervene_char==0~"No",
    ),
    #target - baseline. So if target has fewer than baseline,
    #as expected, this number should be negative
    rd_diff_pt=rd_target_pt-rd_baseline_pt,
    
    #as an option to slice the number
    #of scenarios for visualization,
    #create a variable that indicates deciles
    target_percentile_decile=case_when(
      target_percentile %in% seq(0, 1, by=.1) ~1,
      TRUE~0
    ),
    
    #for the shiny app, I need it to be a decimal point
    #rounded to exaclty two digits. Let's do this so the text doesn't get
    #too cumbesrone
    target_percentile_original=target_percentile,
    target_percentile= round(as.numeric(target_percentile_original),digits=2)
    )

#save  
setwd(here("data-processed"))
save(wf_emm_scenarios_all_pt,file="wf_emm_scenarios_all_pt.RData")

class(wf_emm_scenarios_all_pt$target_percentile)

wf_emm_scenarios_all_pt %>% View()




#check the decile creation...nice it worked!
wf_emm_scenarios_all_pt %>% 
  distinct(target_percentile,target_percentile_decile)

#This is pretty fast.
names(wf_emm_scenarios_all_pt)

#a lookup for the target percentile and whether it's a decile
lookup_target_percentile_decile=wf_emm_scenarios_all_pt %>% 
  distinct(target_percentile,target_percentile_decile)

lookup_target_percentile_decile
setwd(here("data-processed"))
save(lookup_target_percentile_decile,file="lookup_target_percentile_decile.RData")



#Write a function that considers uncertainty-------
#Can assume both rd_target_pt and rd_per_iqr_pt are from
#a normal distribution and use their SD to propagate the uncertainty
# 8 pm Thinking...this could either be together with the function where
#we vary the scenarios or separate from it. It may be simpler to separate,
#actually. That is, we can define the scenario first.
#Yea, I don't think we need to mix them

#9 pm
#At first, I was trying to include the dataset name in the function
#but I'm having trouble getting that to work, so before the function,
#stack all of the point estimates together and then put
#that dataset inside the function

names(wf_emm_scenarios_all_pt)
bootstrap_rd=function(s_id_val){
  
  out=wf_emm_scenarios_all_pt %>% 
    mutate(
      #call these _s to remind myself that they're sampled and randomly 
      #varying
      s_id = s_id_val,
      #sample the baseline RD and the change in RD per IQR change in AC
      #Define them from a normal distribution
      #Feb 19, 2025: update to include different operations depending
      #on whether the zip is intervened upon or not
      rd_baseline_s=rnorm(
          n=n(),
          mean=rd_baseline_pt,
          sd=rd_baseline_sd
        ),
      #the amount the RD changes based on change in AC
      rd_per_iqr_s=rnorm(
        n=n(),
        mean=rd_per_iqr_pt,
        sd=rd_per_iqr_sd),
      
      #This depends on whether the zip is intervened upon or not in that scenario
      rd_target_s=case_when(
        zcta_intervene==1~ rd_baseline_s+rd_per_iqr_s*emm_change_per_iqr,
        zcta_intervene==0~ rd_baseline_s
      )
    )
  return(out)
}

test_boot_once=bootstrap_rd(1)

#check to confirm it's varying
# test_boot_once %>% 
#   dplyr::select(
#     contains("w_hat_mu"),contains("rd")) %>% 
#   View()

#Run the function X times
#Feb 19, 2025: 1,000 reps was taking too long. Run 100 for the moment
n_boot_reps = 100
s_id_val_list <- seq(from = 1, to = n_boot_reps, by = 1)

#Now this is super simple, as I only have to iterate over one list of values
#(rather than also the list of data frames)
#Note Hadley and Co want us to use map() and list_rbind()
#instead of map_dfr()
#https://purrr.tidyverse.org/reference/map_dfr.html
bootstrap_rd_df=s_id_val_list %>% 
  map(bootstrap_rd) %>% 
  list_rbind()

names(bootstrap_rd_df)
#summarize by zcta and by measure
bootstrap_rd_df %>% 
  group_by(zcta, target_percentile, emm_measure) %>% 
  summarise(
    rd_target_min=min(rd_target_s,na.rm=T),
    rd_target_med=median(rd_target_s,na.rm=T),
    rd_target_max=max(rd_target_s,na.rm=T),
    rd_baseline_med=median(rd_baseline_s,na.rm=T)
  )

#summarize overall risk difference under each scenario
bootstrap_rd_df %>% 
#  filter(zcta_intervene==1) %>% 
  group_by(target_percentile) %>% 
  summarise(
    rd_target_min=min(rd_target_s,na.rm=T),
    rd_target_med=median(rd_target_s,na.rm=T),
    rd_target_mean=mean(rd_target_s,na.rm=T),
    rd_target_max=max(rd_target_s,na.rm=T),
    rd_baseline_med=median(rd_baseline_s,na.rm=T),
    rd_baseline_mean=mean(rd_baseline_s,na.rm=T)
  )

# Compare all combinations of values------
#For a given scenario, compare eg 80th with 30th percentile
wf_emm_scenarios_all_pt %>% 
  filter(target_percentile==.8) %>% 
  View()

wf_emm_scenarios_all_pt

#I wonder if I can do this using grouping?
#What if we want to compare 80th percentile with 30th?
10-9
10-8
10-7

## flesh out the idea-----
names(wf_emm_scenarios_all_pt)
scenario_compare_upper_test=wf_emm_scenarios_all_pt %>% 
  filter(target_percentile==.8) %>% 
  mutate(
    rd_diff_upper_pt=rd_diff_pt,
    target_percentile_upper=target_percentile) %>% 
  dplyr::select(zcta, contains("emm_measure"),contains("upper"))

scenario_compare_lower_test=wf_emm_scenarios_all_pt %>% 
  filter(target_percentile==.4) %>% 
  mutate(
    rd_diff_lower_pt=rd_diff_pt,
    target_percentile_lower=target_percentile) %>% 
  dplyr::select(zcta, contains("emm_measure"),contains("lower"))

scenario_compare_test=scenario_compare_upper_test %>% 
  left_join(scenario_compare_lower_test,by=c("zcta","emm_measure")) %>% 
  mutate(rd_diff_diff_pt=rd_diff_upper_pt-rd_diff_lower_pt)

scenario_compare_test
scenario_compare_test
  
## all combinations----
#in this case it's linear, but it might not always be, so calculate empirically

zero_to_1_20
length(zero_to_1_20)
#1 20 beneath it (including zero)



#what about a "full join"
#April 17, 2025: updated to include joining by measure as well
names(wf_emm_scenarios_all_pt)
scenario_compare_upper=wf_emm_scenarios_all_pt %>% 
  mutate(
    rd_diff_upper_pt=rd_diff_pt,
    target_percentile_upper=target_percentile) %>% 
  dplyr::select(zcta, emm_measure,contains("upper"))

scenario_compare_lower=wf_emm_scenarios_all_pt %>% 
  mutate(
    rd_diff_lower_pt=rd_diff_pt,
    target_percentile_lower=target_percentile) %>% 
  dplyr::select(zcta, emm_measure,contains("lower"))

#the full join does it!
#https://stackoverflow.com/questions/73717164/
#Now I can simply use a case-when syntax to ensure that upper is above lower
#April 17, 2025: updated to include joining by measure as well
scenario_compare_full_join=scenario_compare_upper %>% 
  full_join(
    scenario_compare_lower,
    by=c("zcta","emm_measure"),
    relationship = "many-to-many") %>% 
  arrange(zcta,desc(target_percentile_upper)) %>% 
  mutate(rd_diff_diff_pt=rd_diff_upper_pt-rd_diff_lower_pt)

#that works!
#save for markdown
setwd(here("data-processed"))
save(scenario_compare_full_join,file="scenario_compare_full_join.RData")

# scenario_compare_full_join %>%
#   View()

class(scenario_compare_full_join$target_percentile_upper)
class(scenario_compare_full_join$target_percentile_lower)
scenario_compare_full_join %>% 
  group_by(target_percentile_lower, emm_measure) %>% 
  summarise(n=n())
scenario_compare_full_join %>% 
  filter(
    round(target_percentile_upper,2)==.8) %>% 
  #a strange rounding issue. this works.
  filter(
    round(target_percentile_lower,2)==.3)
