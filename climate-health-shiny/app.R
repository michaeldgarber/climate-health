
# Introductory comments--------
#Some options to include in the app
#are 
#1. sliders to show the distribution changes depending on the scenario

#2. a click interfacet that changes the map based on the scenario

#Comment Feb 21, 2025:
#Begin with the dropdown consistent with the interface used for Maren's project
#Then consider the slider option.

#I'm having an issue where it looks like I need the latest leafgl package
#for it to work, and it's not available on cran, so it doesn't work when
#loaded onto the website

# February 24, 2025
# I had code that works locally for both the slider and the dropdown.
#I'm just going to use the slider for this purpose

#Ah! I'm looking through my server logs
#and it says maptiles is required
#https://www.shinyapps.io/admin/#/application/14064821/logs

#March 4, 2025:
#I now have a "lite" version that works without a basemap.
#I'm trying to get it to work with a basemap as well

#April 16, 2025
#I'm revising to only include one input and do the slider thing like
#I did with Maren's
#climate-health-shiny/app-heat-hosp-green-hia-mapview-cards.R
#Comment: This is looking much better.
#Add a summary table at the beginning like I've done with Maren's

# April 21, 2025
#and now I'm building another select tool that selects the
#modifier.



# Load packages and data------
library(shiny)
library(bslib)#to get some new shiny interfaces
library(here)
library(tidyverse)
library(viridis)
library(viridisLite)
library(sf)
library(tmap)
library(tmaptools)
library(mapview)
library(RColorBrewer)
#library(leafgl)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(maptiles)
library(rlang)
library(rsconnect)
## Load data locally------

## Load data within shiny app------
zero_to_1_20 =seq(0, 1, by=.05)

#shiny prefers that the data is relative to the shiny app
#Note shinyapps.io doesn't seem to like it when I change my working directory, so try
#relative to the app script, not the project. unusual for RStudio
#see  heat-hosp-california/shiny/tmap-static/app.R
#April 22, 2025: update - omit race variables from scenarios
load("data/zcta_ca_geo_simplified.RData")

load("data/wf_emm_scenarios_no_race_pt.RData")
load("data/all_scenarios_no_race_summary_by_zcta_pt.RData")
#this is created here
#climate-health-shiny/manage-data-for-app.R
load("data/all_scenarios_no_race_summary_pt.RData")
load("data/all_scenarios_no_race_summary_boot.RData") 

#the slider for some reason doesn't pick up all of the values.
#That doesn't make sense.
# class(wf_emm_scenarios_all_pt$target_percentile)
# summary(wf_emm_scenarios_all_pt$target_percentile)
#why does the shiny not pick up .3?
# wf_emm_scenarios_all_pt %>% 
#   group_by(target_percentile) %>% 
#   summarise(n=n())
# 
# wf_emm_scenarios_all_pt %>% 
#   group_by(target_percentile) %>% 
#   summarise(n=n())

#Update: the _2dig version helped! I explicitly round to 2 values
#in both the slider app and the variable

## Define global variables------
table(all_scenarios_no_race_summary_pt$emm_measure)
# this is now restricted to the SES vars only
emm_measure_options=all_scenarios_no_race_summary_pt %>% 
  distinct(emm_measure) %>% 
  pull()

emm_measure_options

# Define UI part-------
#Use a sidebar like with Maren's
ui <- page_fillable(
  
  title="app",
  #ah, use header instead
  h1("Modification of effect of wildfire on acute-care utilization
     by various measures at the zip-code level"),

  card(
    height = 500,
      p("Question: How would hypothetically changing the distribution of 
        the effect modifiers of the effect of wildfire on respiratory acute-care utilization in
        California affect its total burden and spatial distribution of
        respiratory acute-care utilization?"),
      p("Target percentile refers to the percentile of the effect modifier
        to which all ZCTAs with values below that percentile value would be raised under
        the target scenario.
        For example, if the target scenario corresponds to the 60th percentile of 
        air conditioning,
        below the 60th percentile would have their A/C raised to the 60th percentile."),
      p("  The baseline risk difference refers to the risk difference attributable to wildfire
      under its status-quo value for the effect modifier.
        The target risk difference refers to the risk difference attributable to wildfire
      upon varying the ZCTA's value of the effect modifier.
      The analyses below present the difference in risk differences (RDs) 
        between these two scenarios.")
    ),
  layout_sidebar(
    #setting fillable to FALSE allows it to scroll. Perfect.
    #I like this layout.
    fillable = FALSE,
#    title="hi",
    #April 22, 2025: it makes more sense to pick the measure first
    #and then the slider
  
    sidebar = sidebar(
      #consider a wider width to accommodate longer variable labels
      width=300,
      title="Select inputs",
      
      ## select EMM measure input-------
      #add another sidebar that selects the EMM meausure
      #Follow along here to customize the names of the choices that appear
      
      #     title = "Select effect-measure modifier",
      selectInput(
        "emm_measure", #the variable to select
        label="Effect measure modifier:", #the label
        #          label=c("1","2","3","4","5","6","7","8","9"),
        
        #choices can simply be a vector of the variable names
        #or the names can be changed
        #Prop. instead of proportion
        choices=list(
          "Prop. above poverty"="above_poverty_prop",
          "Prop. with A/C"="ac_prop",
          "Prop. with car"="car_prop",
          "Prop. with bachelor's degree"="edu_bach_prop",
          "Prop. employed"="employed_prop",
          "Per-capita income"="income_per_capita",
          "Proportion insured"="insured_prop",
          "Population density"="pop_dens_10k",
          "Tree canopy share"="tree_canopy_prop"
        ),
#        choices=emm_measure_options,
        
        #can adjust the number to show so that the box is bigger
        selectize = F,
        size=6
      ), #the choices
      
      ## Define slider inputs (target percentile)-----
      
      sliderInput(
        
        "target_percentile", #object name of the variable to select
        "Target percentile:",
        min = 0,
        max = 1,
        value = 0.5,  #what it goes to by default
        step = 0.05,  #the key part
        
        #with the slider, you can specify the "step",
        #how big the increment is between values
        #https://shiny.posit.co/r/articles/build/sliders/
        
        round=2#this seems necessary to get the numbers to be exactly the same..
      ),


          ),# closes sidebar
    
      
    ## summary table card----
    #April 21, 2025: make this an accordion.
    #Do once I have several
       card(
           full_screen = T,
           height = 250,
           card_header("Summary table"),
            tableOutput("table_summary")
            
        ),

    ## histogram with slider input----
    card(
      #Define min size
      #    height=1000,
      full_screen = TRUE,
      card_header("Histogram: difference in risk differences
                  (Target scenario - Baseline scenario)"),
      plotOutput("histogram_rd_diff_slider")
    ),
  
    ##static tmap of diff in RD (slider input) ------
    card(
      card_header("Static map: difference in risk differences
      (Target scenario - Baseline scenario)"),
      p(
        "The difference in risk differences (RDs) (per 100,000),
        comparing the target with the baseline scenarios."),
      #custom name; match it below
      tmapOutput(
        height = "650px" ,
        "tmap_static_rd_diff_slider"
                 )
      ),
    
    ##interactive mapview of diff in RD--------
    #let's add mapview here as well. why not
    card(
      
      full_screen = TRUE,
      card_header("Interactive map: difference in risk differences (Target-Baseline) [Slider]"),
      p(
        "The difference in risk differences (per 100,000),
      comparing the target with the baseline scenarios"),
      #    https://r-spatial.github.io/mapview/reference/mapviewOutput.html
      leafletOutput("mapview_rd_diff_slider")
    )
    ) 
) # close page fillabel

 


# Server part------
#This is where we define the objects and plots such as maps and plots
#Feb 24, 2024: updates to include the "proxy" thing
# define the tmap



tmap_mode("plot")
server <- function(input, output, session) {

 
  ## summary table------
  output$table_summary = renderTable({
    table_out_summary=all_scenarios_no_race_summary_pt %>% 
      dplyr::select(
        emm_measure,
        target_percentile,
        n_zcta_intervene,
        pop_intervene,
        rd_baseline_sum,
        rd_target_sum,
        rd_diff_sum
      ) %>% 
      left_join(all_scenarios_no_race_summary_boot,by=c(
        "target_percentile","emm_measure")) %>% 
      dplyr::select(
        emm_measure,
        contains("target_percentile"),
        pop_intervene,
        n_zcta_intervene,
        contains("rd_baseline_sum"),
        contains("rd_target_sum"), 
        contains("rd_diff_sum")
      ) %>% 
      #filter to the corresponding effect measure modifier
      filter(emm_measure==input$emm_measure) %>% 
      #filter to the corresponding target percentile
      filter(
        #this round step to get it to respond better to the equal sign
        round(target_percentile,2)==input$target_percentile) %>% 
    
    #Work on formatting of numbers as I did with Maren's, but otherwise this is working.
      #Concatenate values so that the confidence intervals are included in the same
      #column
      mutate(
        n_zctas_intervene_char=as.character(
          formatC(n_zcta_intervene,digits=0,format="f",big.mark=",")
        ),
        
        pop_intervene_char=as.character(
          formatC(pop_intervene,digits=0,format="f",big.mark=",")
        ),
        
        #Note the risk differences are already expressed per 100,00
        rd_baseline_pt_per_100k_char=as.character(
          formatC(rd_baseline_sum,digits=1,format="f",big.mark=",")
        ),
        rd_baseline_ll_per_100k_char=as.character(
          formatC(rd_baseline_sum_ll,digits=1,format="f",big.mark=",")
        ),
        rd_baseline_ul_per_100k_char=as.character(
          formatC(rd_baseline_sum_ul,digits=1,format="f",big.mark=",")
        ),
        
        rd_target_pt_per_100k_char=as.character(
          formatC(rd_target_sum,digits=1,format="f",big.mark=",")
        ),
        rd_target_ll_per_100k_char=as.character(
          formatC(rd_target_sum_ll,digits=1,format="f",big.mark=",")
        ),
        rd_target_ul_per_100k_char=as.character(
          formatC(rd_target_sum_ul,digits=1,format="f",big.mark=",")
        ),
        
        rd_diff_pt_per_100k_char=as.character(
          formatC(rd_diff_sum,digits=1,format="f",big.mark=",")
        ),
        rd_diff_ll_per_100k_char=as.character(
          formatC(rd_diff_sum_ll,digits=1,format="f",big.mark=",")
        ),
        rd_diff_ul_per_100k_char=as.character(
          formatC(rd_diff_sum_ul,digits=1,format="f",big.mark=",")
        ),
        
        #now concatenate them together using paste0
        rd_baseline_per_100k_conc = paste0(
          rd_baseline_pt_per_100k_char,  " (", 
          rd_baseline_ll_per_100k_char, ", ",
          rd_baseline_ul_per_100k_char,  ")"
        ),
        rd_target_per_100k_conc = paste0(
          rd_target_pt_per_100k_char," (", 
          rd_target_ll_per_100k_char,", ",
          rd_target_ul_per_100k_char,")"
        ),
        rd_diff_per_100k_conc = paste0(
          rd_diff_pt_per_100k_char," (", 
          rd_diff_ll_per_100k_char,", ",
          rd_diff_ul_per_100k_char,")"
        )
        
      ) %>% 
      dplyr::select(target_percentile, n_zctas_intervene_char,
                    pop_intervene_char,
                    contains("conc")) %>% 
      #now rename those variables to display in the table
      #I'm missing population affected here.
      rename(
        "Target percentile" = target_percentile,
        "N, ZCTAs intervened" = n_zctas_intervene_char,
        "Population affected" =pop_intervene_char,
        "IRD per 100,000, baseline scenario"=rd_baseline_per_100k_conc,
        "IRD per 100,000, target scenario"=rd_target_per_100k_conc,
        "Difference in IRD per 100,000, target scenario"=rd_diff_per_100k_conc
      )
    
    table_out_summary
  })
  
  
  ##histogram (slider)-------
  #as far as I can tell, this is exactly the same. The only difference is
  #how the value is selected (slider vs dropdown)
  output$histogram_rd_diff_slider <- renderPlot(  {
    
    #First create the dataset
    wf_emm_scenarios_no_race_pt %>% 
      #      filter(target_percentile_decile==1) %>% #don't need this step
      #the key step for shiny. The value corresponding to the slider input
      filter(emm_measure==input$emm_measure) %>% 
      filter(
        #this round step to get it to respond better to the equal sign
        round(target_percentile,2)==input$target_percentile) %>%   
      ggplot(aes(x=rd_diff_pt, fill = zcta_intervene_char_yn))+
      #make bins shorter to make the visualization clearer
      #.2 is too coarse;
      geom_histogram(binwidth=.01)+
      #keep the axes constant to
      #visualize how it changes
      scale_x_continuous(
        limits=c(-.45,.1)
      )+
      theme_bw(base_size = 16)+
      labs(
        y="Number of ZCTAs",
        #using \n for line break
        x="Difference in risk differences\nper 100,000\n(Target-Baseline)",
        fill="ZCTA intervened upon")
    
  })
  
  ## static tmap - diff in RD (slider)------
  #comment out; crashing server
  #note this object name (tmap_rd_diff) must match above
  output$tmap_static_rd_diff_slider = renderTmap({

  
    #April 21, 2025: idea to lighten this up.
    #Don't begin with this huge sf file. Begin with the aspatial file
    #and then link to the spatial data at the very end so you don't
    #carry around multiple instances of the spatial data
    input_df_for_map=all_scenarios_no_race_summary_by_zcta_pt %>%
      filter(emm_measure==input$emm_measure) %>% 
      filter(
        #again a strange rounding issue with 0.3
        #I have to do this rounding procedure first
        round(target_percentile,2) ==input$target_percentile) %>%
      dplyr::select(-contains("mean"))
    
    #now link this to the geometry after it's already been filtered
    sf_obj_for_map=zcta_ca_geo_simplified %>% 
      left_join(input_df_for_map,by="zcta")


    tm_obj_static=tm_basemap(
      #March 4, 2025
      #Consider changing the zoom to downsample the resolution
      #of the basemap to avoid it being too much data for shinyapps"
      #zoom options are 0-20
      #https://r-tmap.github.io/tmap-book/layers.html#tile
      zoom=2, #lower level = worse resolution
      
      "CartoDB.Positron")+
      tm_shape(sf_obj_for_map)+
      tm_polygons(
        fill = "rd_diff_sum",
        lwd=0.5,
        fill.legend= tm_legend("Diff. in RDs per 100k"),
        fill.scale=tm_scale_intervals(
          #Don't specify style so that the code listens to
          #the "breaks" code
#          style="pretty",
          # you need n+1 number of breaks
          breaks = c(-.4,-.3,-.2,-.1,0), 
          n=4,#8 categories
          values="matplotlib.plasma")
      )+
      tm_layout(
        legend.position=c("right","top"),
        legend.outside = TRUE,#get legend outside the frame
        meta.margins = c(0, 0, 0, 0.15)
        ) # fixed margin for the legend
    tm_obj_static

  })
  
  ## mapview-------
  
  output$mapview_rd_diff_slider = renderLeaflet({
    
    #create the dataset based on the scenario type.
    #This is the key step! The filters are used as inputs.
    #  #this is kind of like defining a function
    #Note I'm using the _2dig variable to make sure it's exactly
    #the same as the slider input
    
    #Apr 22, 2025: do the same thing, where the filter is done
    #on the aspatial data, and then I link it to the spatial data
    input_df_for_map=all_scenarios_no_race_summary_by_zcta_pt %>%
      filter(emm_measure==input$emm_measure) %>% 
      filter(
        #again a strange rounding issue with 0.3
        #I have to do this rounding procedure first
        round(target_percentile,2) ==input$target_percentile) %>%
      dplyr::select(-contains("mean"))
    
    sf_obj_for_map=zcta_ca_geo_simplified %>% 
      left_join(input_df_for_map,by="zcta")
    
    
    mapview_obj=mapview(
      sf_obj_for_map,#maybe the pipe is the issue
      layer.name="Diff. in RD per 100k (Target-Baseline)",
      zcol="rd_diff_sum",
      col.regions=viridis_pal(option="C"),
      lwd=.5 #make line width less
    )
    
    mapview_obj@map
    
  })

 

}

# Run app-----
shinyApp(ui = ui, server = server)

