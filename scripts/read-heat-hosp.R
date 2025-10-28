#Read heat-hospitalizations data from Kristen's paper
#Sep 19, 2025
#Began importing data
#Oct 10, 2025: see notes elsewhere. 
#use absolute measures. Revising comments for brevity
#Oct 14, 2025: note the structure of this differs from wildfire because
#there are various heat definitions, so some of it is long form by
#heat definition

#assuming heat index comes from the usual national weather service heat index
#https://www.wpc.ncep.noaa.gov/html/heatindex.shtml


# Import and manage data-------
#Use the _abs file name, and call it n_cases_diff
#for consistency with elsewhere
#That is, the absolute number of cases - effect - not divided by population
#See notes elsewhere
## Read effect estimates-----
setwd(here("data-input","data-kristen-sent-August-2024"))
heat_n_cases_diff_est = read_csv("joined_abs.csv") %>% 
  rename(zcta=ZCTA5CE10) %>% 
  dplyr::select(-starts_with("...")) %>% #remove first column.
  dplyr::select(-ends_with("SNR")) %>% 
  #add heat as a prefix before them all to differentiate in the wide-form dataset
  #rename all but -zcta
  rename_with(~paste0("heat_",.x),-zcta)

#add populatoin data and calculate an IRD

heat_n_cases_diff_est


## Make estimates long form-------
#make it long-form and then grab just one measure
#Also calculate IRD using this long-form data, 
#as that's the most pertinent measure, actually,
#for the associations. Also consistent with the wildfire paper

setwd(here("data-processed"))
load("lookup_pop_zcta.RData") #need population for IRD
#callingf this est because it includes two estimates
heat_est_long=heat_n_cases_diff_est %>% 
  pivot_longer(-zcta) %>% #easy enough.
  #and now rename
  #Oct 14, 2025: elsewhere I use the "quo" suffix
  #to indicate status-quo, 
  #also pt to indicate pt rather than sd
#  so say:
  rename(heat_def=name,
         n_cases_diff_quo_pt=value) %>% 
  left_join(lookup_pop_zcta, by = "zcta") %>%
  #This nomenclature is used elsehwere - makes clear that it's
  #a risk difference per 100k and that it's a point estimate
  mutate(
    #If this is zero, it's missing
    n_cases_diff_quo_pt_with_zeros=n_cases_diff_quo_pt, #keep this around
    n_cases_diff_quo_pt=case_when(
      n_cases_diff_quo_pt_with_zeros==0~NA_real_,
      pop<1~NA_real_,#also set to missing if no pop.
      TRUE~n_cases_diff_quo_pt_with_zeros),
    
    #an indicator of zeros
    n_cases_diff_quo_pt_zero=case_when(
      n_cases_diff_quo_pt_with_zeros==0~1, #for data summary
      TRUE~0
    ),
    
    #This returns some infinities, as some pops are zero.
    #Set those to NA
    rd_100k_quo_pt=case_when(
      pop<1~NA_real_, #if no pop, then this is missing
      TRUE ~ n_cases_diff_quo_pt/pop)*100000

    ) %>% 
  #Keep pop here as well for sd calculations
  dplyr::select(zcta,heat_def,contains("n_cases_diff"),contains("rd_"),contains("pop")) %>% 
  #find outliers in the ird
  mutate(
    #find outliers
    #no grouping needed as elsewhere, as the data are wide-form
    #consider a stricter outlier definition; otherwise things get wonky
    #use flexible (low v high)
    rd_100k_quo_lo=quantile(rd_100k_quo_pt,probs=c(0.05), na.rm=T),
    rd_100k_quo_hi=quantile(rd_100k_quo_pt,probs=c(0.95), na.rm=T),
    #we can call this simply rd_quo_outlier because it's an outlier
    #whether it's per 100k or not
    rd_quo_outlier=case_when(
      rd_100k_quo_pt<rd_100k_quo_lo~1,
      rd_100k_quo_pt>rd_100k_quo_hi~1,
      TRUE~0),
  )

heat_est_long

## Read in SNR in long form-------
## Read signal-to-noise ratios-------
#I still need to figure out how to integrate this with the rest of the estimates
#as well. Whichever definition I pick, I'll need the corresponding sd
setwd(here("data-input","data-kristen-sent-August-2024"))
heat_n_cases_diff_snr_long = read_csv("joined_abs.csv") %>% 
  rename(zcta=ZCTA5CE10) %>% 
  dplyr::select(-starts_with("...")) %>% #remove first column.
  dplyr::select(zcta,ends_with("SNR")) %>% 
  #add heat_ to clarify that these are heat definitions
  #as above
  rename_with(~paste0("heat_",.x),-zcta) %>% 
  #remove SNR from the variable name, and I'll put it in the column name
  rename_with(~ str_remove(., "_SNR")) %>% 
  pivot_longer(-zcta) %>% #easy enough.
  #and now rename
  rename(heat_def=name,
         #analogous naming conventions
         n_cases_diff_quo_snr=value)  

heat_n_cases_diff_snr_long

#Okay, now convert to SD
heat_est_sd_long=heat_est_long %>% 
  left_join(heat_n_cases_diff_snr_long,by=c("zcta","heat_def")) %>% 
  mutate(
    outcome="heat", #keep track vs wildfire as needed
    
    #so, because SNR=est/SD, the SD in terms of absolute cases is
    #and then SD can't be negative, so abs value it
    n_cases_diff_quo_sd=abs(n_cases_diff_quo_pt/n_cases_diff_quo_snr),
    #and then because pop is a constant, we can convert to pop like this
    rd_100k_quo_sd=(n_cases_diff_quo_sd/pop)*100000,
    
    #now that we have the SD, also do this:
    # in certain instances, I need the RD expressed without the
    #per 100k
    rd_quo_pt=rd_100k_quo_pt/100000,#rate difference per 1 person-day
    rd_quo_sd=rd_100k_quo_sd/100000  #rate difference per 1 person-day
    

    )

names(heat_est_sd_long)


## Make wide form again for certain functions------
#To integrate with the correlation functions I created elsewhere,
#make wide form again

heat_rd_wide=heat_est_sd_long %>% 
  #and limit to no outliers for the purpose of the bivar. associations
  filter(rd_quo_outlier==0) %>% 
  #also remove the zeros I think...try it
#  filter(n_cases_diff_quo_pt_zero==0) %>% 
  dplyr::select(-contains("n_cases"),-starts_with("pop"),-contains("_sd")) %>% 
  #This works
  pivot_wider(
    names_from = heat_def,
    values_from=c(rd_100k_quo_pt)
              ) %>% 
  mutate(measure="rd_100k_quo_pt")

#and here's the wide-form data with the n_cases_diff_measure only  
heat_n_cases_diff_wide=heat_n_cases_diff_est %>% 
  mutate(measure="n_cases_diff")
names(heat_n_cases_diff_wide)
names(heat_rd_wide)
nrow(heat_rd_wide)

heat_wide_rd_n_cases_diff=heat_rd_wide %>% 
  bind_rows(heat_n_cases_diff_wide)

heat_wide_rd_n_cases_diff

#may need to remove outliers
summary(heat_rd_wide$heat_max95_1)

heat_wide_joined=heat_wide_rd_n_cases_diff %>% 
  left_join(covars,by="zcta")

#heat_wide_joined %>% View()


# Checks and distributions-----
## misc checks-----
#How many have  zeros for the effect measure?
heat_est_sd_long %>% 
  group_by(heat_def,n_cases_diff_quo_pt_zero) %>% 
  summarise(n=n()) %>% 
  mutate(prop=n/sum(n)) 

#okay, 37% zeros with heat index & 2-day duration
#I now changed it upstream to not be zero

## Examine distributions of the measures---------
### Boxplots-------
heat_est_long %>% 
  ggplot(aes(x=n_cases_diff_quo_pt))+
  geom_boxplot()+
  facet_grid(rows="heat_def")
heat_est_long %>% 
  ggplot(aes(y=n_cases_diff_quo_pt))+
  geom_boxplot()+
  facet_grid(cols=vars(heat_def))+
  theme_bw(base_size=10)

### summary statistics--------
#Try simple med, mean, 25th, etc.
names(heat_est_sd_long)
heat_summary_by_def=heat_est_sd_long %>% 
  group_by(heat_def) %>% 
  summarise(
    n_cases_diff_quo_pt_m=mean(n_cases_diff_quo_pt,na.rm=T),
    n_cases_diff_quo_pt_med=median(n_cases_diff_quo_pt,na.rm=T),
    n_cases_diff_quo_pt_25th=quantile(n_cases_diff_quo_pt,probs=.25,na.rm=T),
    n_cases_diff_quo_pt_75th=quantile(n_cases_diff_quo_pt,probs=.75,na.rm=T),
    
    rd_100k_quo_pt_m=mean(rd_100k_quo_pt,na.rm=T),
    rd_100k_quo_pt_med=median(rd_100k_quo_pt,na.rm=T),
    
    
    #mean and median of the SD
    rd_100k_quo_sd_m=mean(rd_100k_quo_sd,na.rm=T),
    rd_100k_quo_sd_med=median(rd_100k_quo_sd,na.rm=T)
  )

#heat_summary_by_def %>% View()
  #the definitions that look promising are hi99_2 and 3


heat_summary_by_def %>% 
  ggplot(aes(x=heat_def,y=n_cases_diff_quo_pt_med))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

## Explore SD of some of the definitions------
#heat_hi99_2
heat_est_sd_long %>% 
  ggplot(aes(y=rd_100k_quo_sd))+
  geom_boxplot()+
  facet_grid(cols=vars(heat_def))+
  theme_bw(base_size=8)

heat_est_sd_long %>% 
  ggplot(aes(y=rd_100k_quo_sd))+
  geom_boxplot()+
  facet_grid(rows=vars(heat_def))+
  theme_bw(base_size=8)


# Create a dataset limited to the definition we'll use------
#Upon checks above, we'll make this dataset.
# Make a dataset limited to the definition I'll use: 2-day hi
names(heat_est_sd_long)
outcome_heat_hi99_2=heat_est_sd_long %>% 
  filter(heat_def=="heat_hi99_2") %>% 
  mutate(
    #and finally:
    #and some model-related wrangling on this dataset
    #As I did for the wildfire effects, I need to create weights 
    #as the inverse of the modeled SD
    #what's the smallest standard deviation?
    
    #Note I'm doing this calculation here for simplicity. It could also be done
    #earlier, though the minimum would have to be groupwise by heat definition
    rd_quo_sd_min=min(rd_quo_sd,na.rm=T),
    #divide the minimum by each SD. Higher values are less precise.
    rd_quo_sd_min_prop=rd_quo_sd_min/rd_quo_sd,
    rd_quo_sd_inv=1/rd_quo_sd,
    #I want the weights to be more normally distributed, though, so
    #go like this
    rd_quo_weight=sqrt(rd_quo_sd_min_prop)
  ) %>% 
  dplyr::select(zcta,outcome,heat_def,contains("n_cases"),contains("rd_"),everything()) %>% 
  #remove pop and derivates to avoid conflicts with covars
  dplyr::select(-starts_with("pop")) %>% 
  #and bring in the covariates
  left_join(covars,by="zcta")


names(outcome_heat_hi99_2)
names(covars)

#outcome_heat_hi99_2 %>% View()
#Note some are just straight up zeros
#Hmm,maybe exclude the straight-up zeros?
outcome_heat_hi99_2 %>% View()

heat_est_sd_long %>% 
  filter(n_cases_diff_quo_pt==0) %>% 
  print(n=1000)

#plot sd vs weight. should be inverse
#that looks better.
outcome_heat_hi99_2 %>% 
  filter(rd_quo_sd<.0001) %>% 
  ggplot(aes(x=rd_quo_sd,y=rd_quo_weight))+
  geom_point()

outcome_heat_hi99_2 %>% 
  ggplot(aes(x=rd_quo_sd))+
  geom_histogram()

outcome_heat_hi99_2 %>% 
  ggplot(aes(x=rd_quo_weight))+
  geom_histogram()

#I forgot. Do we have outliers included here?
outcome_heat_hi99_2 %>% 
  group_by(rd_quo_outlier) %>% 
  summarise(n=n())

#Yes, they are. Okay, for modeling. They should probably be omitted. Otherwise,
#we get weird results.
outcome_heat_hi99_2_no_outliers=outcome_heat_hi99_2 %>% 
  filter(rd_quo_outlier==0)


