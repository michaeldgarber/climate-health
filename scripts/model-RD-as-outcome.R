#Continued from
#source(here("scripts","data-mgmt-analyses-before-modeling-RD-as-outcome.R"))

# Fit models-------
## AC---------
#linear regression should work. it's a continuous var.
names(wf_eff_emm_wide)

### ac alone--------
glm_rd_ac= glm(
  rd_quo_pt~ac_prop,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers)

glm_rd_ac


### ac - confounding adjustment-------
#e for exposure; conf for confounder
glm_rd_ac_conf = glm(
  rd_quo_pt~ac_prop +above_poverty_prop #+ #basin_name +
#              ruca_cat
          #  pop_dens_mi2_sqrt
                  ,
                  family = gaussian,
                  weights = rd_quo_weight,
                  na.action = "na.exclude", 
                  data = wf_eff_emm_wide_no_outliers)

glm_rd_ac_conf
summary(glm_rd_ac_conf)

### ac interaction models------
#basin
glm_rd_ac_conf_int = glm(
  rd_quo_pt~ac_prop +above_poverty_prop + basin_name +
    basin_name*ac_prop

  ,
  family = gaussian,
  weights = rd_quo_weight,
  na.action = "na.exclude", 
  data = wf_eff_emm_wide_no_outliers)

glm_rd_ac_conf_int
summary(glm_rd_ac_conf_int)

#biome
glm_rd_ac_conf_int_biome = glm(
  rd_quo_pt~ac_prop +above_poverty_prop + biome_name_freq +
    biome_name_freq*ac_prop
  
  ,
  family = gaussian,
  weights = rd_quo_weight,
  na.action = "na.exclude", 
  data = wf_eff_emm_wide_no_outliers)

glm_rd_ac_conf_int_biome
summary(glm_rd_ac_conf_int_biome)

#ac x ruca
names(wf_eff_emm_wide_no_outliers)
table(wf_eff_emm_wide_no_outliers$ruca_cat)
glm_rd_e_tree_ac_prop_conf_int_ruca =glm(
  rd_quo_pt~ac_prop +above_poverty_prop + ruca_cat +
    ac_prop*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_e_tree_ac_prop_conf_int_ruca)

#what if we use the same final
#model for AC as we use for tree can. below?
#Include biome here as well, as ac can vary by biome,
#and we're not going to change biome
glm_rd_ac_conf_ins_biome_int_ruca =glm(
  rd_quo_pt~ac_prop +insured_prop + ruca_cat + biome_name_freq+
    #    above_poverty_prop +
    ac_prop*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_ac_conf_ins_biome_int_ruca)


### AC final model------
#The final model is one that includes an interaction between biome as well
glm_rd_ac_conf_ins_ruca_biome_int_ruca_biome =glm(
  rd_quo_pt~ac_prop +insured_prop + ruca_cat +
    biome_name_freq+
    ac_prop*ruca_cat+
    ac_prop*biome_name_freq,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_ac_conf_ins_ruca_biome_int_ruca_biome)



## tree canopy------
#to shorten name, use tree and assume it's always the square-root version
#### tree canopy alone--------
glm_rd_tree =glm(
          rd_quo_pt~tree_canopy_prop_sqrt,
          family = gaussian,
          na.action = "na.exclude", 
          weights = rd_quo_weight,
          data = wf_eff_emm_wide_no_outliers
  )


summary(glm_rd_tree)

names(wf_eff_emm_wide_no_outliers)
#### confounding adjustment------
summary(wf_eff_emm_wide_no_outliers$tree_canopy_prop_sqrt)
glm_rd_tree_conf =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +above_poverty_prop 
#  + basin_name
  ,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf)
#Interpret. A one-unit increase in the square root of tree canopy
#results in a -1.4 unit decrease in the risk difference
glm_rd_tree_conf

glm_rd_tree_conf$coefficients[2]

#alternate models
glm_rd_tree_conf_ruca =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +above_poverty_prop + ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf_ruca)

#insurance seems to be a stronger predictor than above poverty
glm_rd_tree_conf2 =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +insured_prop + 
    basin_name,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf2)

glm_rd_tree_conf3 =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +insured_prop + 
    biome_name,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf3)


#### tree canopy - interaction model by air basin-----
glm_rd_tree_conf_int_air_basin =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +above_poverty_prop + basin_name +
    tree_canopy_prop_sqrt*basin_name ,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf_int_air_basin)

#### tree canopy - interaction model by biome-----
table(wf_eff_emm_wide_no_outliers$biome_name)
table(wf_eff_emm_wide_no_outliers$biome_name_freq)
glm_rd_tree_conf_int_biome =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +above_poverty_prop + biome_name_freq +
    tree_canopy_prop_sqrt*biome_name_freq,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

summary(glm_rd_tree_conf_int_biome)
glm_rd_tree_conf_int_biome$coefficients

#interpretation: effect of tree canopy is stronger in deserts and weaker
#elsewhere. I suppose that follows?

#Do the math.
#What's the effect of tree canopy in med forests?
#It's the referent group, so it's the main effect
table(wf_eff_emm_wide_no_outliers$biome_name_freq)
glm_rd_tree_conf_int_biome$coefficients[2]
#How does the effect change when it's elsewhere?
#In conifer forests, for exmaple, this is the interaction term
#When this dummy term is 1...
glm_rd_tree_conf_int_biome$coefficients[7]

#yea, so it goes up to .889 meaning it has a POSITIVE effect on the RD
#meaning it CAUSES MORE hospitalizations
glm_rd_tree_conf_int_biome$coefficients[2]+
glm_rd_tree_conf_int_biome$coefficients[7]


#### tree canopy - interaction model by RUCA-----
names(wf_eff_emm_wide_no_outliers)
table(wf_eff_emm_wide_no_outliers$ruca_cat)
glm_rd_tree_conf_int_ruca =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +above_poverty_prop + ruca_cat +
#    pop_dens_mi2+
    tree_canopy_prop_sqrt*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)

#effect of tree canopy does appear to be strongest in most urban
#settings
#and also, curiously, strong in rural areas
summary(glm_rd_tree_conf_int_ruca)

## final tree canopy model after consideration of above
#Use insurance as a confounder. Stratifiy by RUCA
#also include biome probably, so the effect of tree canopy
#can be considered independent of biome, as we're not going to change biome
table(wf_eff_emm_wide_no_outliers$biome_name_freq)
glm_rd_tree_conf_ins_biome_int_ruca =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +insured_prop + ruca_cat +
    biome_name_freq+
    tree_canopy_prop_sqrt*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_tree_conf_ins_biome_int_ruca)

summary(wf_eff_emm_wide_no_outliers$tree_canopy_prop_sqrt)
#so we have, in the most urban areas, an estimate of this
#-1.7280
#That means. for a one-unit increase in the square root of tree
#canopy, the RD goes down -1.7


### tree canopy - final model-----


#May 23, 2025:
#Tarik's thought to add biome as well as an interaction term, allowing
#the effect of tree canopy to vary within biome as well
table(wf_eff_emm_wide_no_outliers$biome_name_freq)
glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +insured_prop + ruca_cat +
    biome_name_freq+
    tree_canopy_prop_sqrt*ruca_cat+
    tree_canopy_prop_sqrt*biome_name_freq,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_tree_conf_ins_ruca_biome_int_ruca_biome)

### insurance?------
names(wf_eff_emm_wide_no_outliers)
glm_rd_e_insured_prop_conf =glm(
  rd_quo_pt~insured_prop +above_poverty_prop + basin_name,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
) 

summary(glm_rd_e_insured_prop_conf)

### Tree canopy and AC together?-------
names(wf_eff_emm_wide_no_outliers)
glm_rd_tree_ac_prop_conf_ins_int_ruca =glm(
  rd_quo_pt~tree_canopy_prop_sqrt +insured_prop + ruca_cat +
    ac_prop+
    tree_canopy_prop_sqrt*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_tree_ac_prop_conf_ins_int_ruca)


# Impervious surface model as well-----
names(wf_eff_emm_wide_no_outliers)
# Use same covars as tree canopy
glm_rd_imperv_conf_ins_biome_int_ruca =glm(
  rd_quo_pt~imperv_prop +insured_prop + ruca_cat +
    biome_name_freq+
    imperv_prop*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_imperv_conf_ins_biome_int_ruca)


# What about modeling heat?-------
#impervious surface as EMM of heat

glm_rd_heat_imperv_conf_ins_biome_int_ruca =glm(
  rd_heat_max95_1~imperv_prop +insured_prop + ruca_cat +
    biome_name_freq+
    imperv_prop*ruca_cat,
  family = gaussian,
  na.action = "na.exclude", 
  weights = rd_quo_weight,
  data = wf_eff_emm_wide_no_outliers
)
summary(glm_rd_heat_imperv_conf_ins_biome_int_ruca)


