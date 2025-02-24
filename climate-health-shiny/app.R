
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
library(leafgl)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

#packageVersion("leafgl")
#devtools::install_github("r-spatial/leafgl")


## Load data locally------
#comment out when running but can use when working interactively on the app
# setwd(here("climate-health-shiny","data"))
# load("wf_ac_scenarios_all_pt.RData")
# load("all_scenarios_summary_by_zcta_pt.RData")
# load("zcta_ca_geo_simplified.RData")
# load("all_scenarios_summary_by_zcta_pt_sf.RData") 
# all_scenarios_summary_by_zcta_pt
# zero_to_1_20 =seq(0, 1, by=.05)


## Load data within shiny app------
zero_to_1_20 =seq(0, 1, by=.05)

#shiny prefers that the data is relative to the shiny app
#Note shinyapps.io doesn't seem to like it when I change my working directory, so try
#relative to the app script, not the project. unusual for RStudio
#see  heat-hosp-california/shiny/tmap-static/app.R
load("data/wf_ac_scenarios_all_pt.RData")
load("data/all_scenarios_summary_by_zcta_pt.RData")
load("data/zcta_ca_geo_simplified.RData")
#this is created here
#climate-health-shiny/manage-data-for-app.R
load("data/all_scenarios_summary_by_zcta_pt_sf.RData") 

#the slider for some reason doesn't pick up all of the values.
#That doesn't make sense.
# class(wf_ac_scenarios_all_pt$target_percentile)
# summary(wf_ac_scenarios_all_pt$target_percentile)
#why does the shiny not pick up .3?
# wf_ac_scenarios_all_pt %>% 
#   group_by(target_percentile) %>% 
#   summarise(n=n())
# 
# wf_ac_scenarios_all_pt %>% 
#   group_by(target_percentile_2dig) %>% 
#   summarise(n=n())

#Update: the _2dig version helped! I explicitly round to 2 values
#in both the slider app and the variable

# Define UI part-------
ui <- page_fluid( 
  
  #create 'cards' for each scenario
  #  https://shiny.posit.co/r/articles/build/layout-guide/
 
  
  ## card for interactive tmap of diff in RD (slider input) ------
  card(
    #Define min size
    #    height=1000,
#    full_screen = TRUE,
    card_header("Map: difference in risk differences (Target-Baseline) [Slider]"),
    p(
      "The difference in risk differences (per 100,000), 
      comparing the target with the baseline scenarios.
      
      mapping package: tmap
      "),
    
    #with the slider, you can specify the "step",
    #how big the increment is between values
    #https://shiny.posit.co/r/articles/build/sliders/
    sliderInput(
      "target_percentile_2dig", #object name of the variable to select
      "Target percentile:",
      min = 0,
      max = 1,
      value = 0.5,  #what it goes to by default
      step = 0.05,  #the key part
      round=2#this seems necessary to get the numbers to be exactly the same..
    ),
    
    
    tmapOutput("tmap_rd_diff_slider")#custom name; match it below
    
  ), #yes, a comma
  
  ## card for static tmap of diff in RD (slider input) ------
  card(
    #Define min size
    #    height=1000,
#    full_screen = TRUE,
    card_header("Static Map: difference in risk differences (Target-Baseline) [Slider]"),
    p(
      "The difference in risk differences (per 100,000), 
      comparing the target with the baseline scenarios.
      Package: tmap"),
    
    #with the slider, you can specify the "step",
    #how big the increment is between values
    #https://shiny.posit.co/r/articles/build/sliders/
    sliderInput(
      "target_percentile_2dig", #object name of the variable to select
      "Target percentile:",
      min = 0,
      max = 1,
      value = 0.5,  #what it goes to by default
      step = 0.05,  #the key part
      round=2#this seems necessary to get the numbers to be exactly the same..
    ),
    
    #custom name; match it below
    tmapOutput(
      height = "1000px" ,
      "tmap_static_rd_diff_slider"
               )
    
  ), #yes, a comma
  
  ## card for mapview of diff in RD (slider input) ------
  #in the latest update, it says renderMapview would work,
  #but it doesn't, so use
  #https://github.com/r-spatial/mapview/issues/254
  card(
    #Define min size
    #    height=1000,
    full_screen = TRUE,
    card_header("Map: difference in risk differences (Target-Baseline) [Slider]"),
    p(
      "The difference in risk differences (per 100,000),
      comparing the target with the baseline scenarios.

      mapping package: mapview
      "),

    sliderInput(
      "target_percentile_2dig", #object name of the variable to select
      "Target percentile:",
      min = 0,
      max = 1,
      value = 0.5,  #what it goes to by default
      step = 0.05,  #the key part
      round=2#this seems necessary to get the numbers to be exactly the same..
    ),


    #custom name; match it below.
#    https://r-spatial.github.io/mapview/reference/mapviewOutput.html
    leafletOutput("mapview_rd_diff_slider")

  ), #yes, a comma
  
  
  
  ## card for histogram of diff in RD (slider input) -----
  #Note that it doesn't always
  card(
    #This can be the same
    full_screen = TRUE,
    card_header("Histogram: difference in risk differences (Target-Baseline) [slider]"),
    
    #with the slider, you can specify the "step",
    #how big the increment is between values
    #https://shiny.posit.co/r/articles/build/sliders/
    sliderInput(
      "target_percentile_2dig", #object name of the variable to select
      "Target percentile:",
      min = 0,
      max = 1,
      value = 0.5,  #what it goes to by default
      step = 0.05,  #the key part
      round=2#this seems necessary to get the numbers to be exactly the same..
      ),

    plotOutput("histogram_rd_diff_slider")
  )
)

# Server part------
#This is where we define the objects and plots such as maps and plots
#Feb 24, 2024: updates to include the "proxy" thing
server <- function(input, output, session) {

  
  ## define server for interactive tmap - diff in RD (slider)------

  #note this object name (tmap_rd_diff) must match above
  output$tmap_rd_diff_slider = renderTmap({

    #create the dataset based on the scenario type.
    #This is the key step! The filters are used as inputs.
    #  #this is kind of like defining a function
    #Note I'm using the _2dig variable to make sure it's exactly
    #the same as the slider input
    summary_df_target_perc_rd_diff_sf=all_scenarios_summary_by_zcta_pt_sf %>%
      #set the target percentile equal to whatever value is selected
      filter(target_percentile_2dig == input$target_percentile_2dig) %>%
      #remove some vars
      dplyr::select(-contains("mean"))

    # define the tmap
    tmap_mode("view")
    
    tm_obj_interactive=tm_basemap("CartoDB.Positron")+
      tm_shape(summary_df_target_perc_rd_diff_sf)+
      tm_polygons(
        fill = "rd_diff_sum",
        lwd=0.5,
        #Change legend. This took some doing to get right.
        #Use X.legend where X corresponds to the layer being
        #visualized, fill here
        #See https://r-tmap.github.io/tmap/reference/qtm.html
        fill.legend= tm_legend("Difference in risk differences (per 100k)"),

        #This is how to change the colors. select a viridis scale
        #https://r-tmap.github.io/tmap/articles/adv_shiny
        #within matplotlib
        fill.scale=tm_scale_intervals(
          #keep number of categories fewer. see
          #https://r-tmap.github.io/tmap/articles/basics_scales

          style="pretty",
          breaks = c(-.4,-.3,-.2,-.1,0), # you need n+1 number of breaks
          n=4,#8 categories
          values="matplotlib.plasma")
      )

    #Feb 21, 2025: this is working locally but not when published to server
    #I tried an equivalent qtm version, and it didn't help. Perhaps I should
    #try the tmap proxy code advised 
    tm_obj_interactive

  })
  
  # #adding the "observe" piece as recommended.
  # observe({
  # 
  #   #Trying this observe part.
  #   summary_df_target_perc_rd_diff_sf=all_scenarios_summary_by_zcta_pt_sf %>%
  #     #set the target percentile equal to whatever value is selected
  #     filter(target_percentile==input$target_percentile) %>%
  #     #remove some vars
  #     dplyr::select(-contains("mean"))
  # 
  #   tmapProxy("tmap_rd_diff_slider", session, {
  #     tm_shape(summary_df_target_perc_rd_diff_sf)+
  #       tm_polygons(
  #         fill = "rd_diff_sum",
  #         lwd=0.5,
  #         fill.legend= tm_legend("Difference in risk differences (per 100k)"),
  #         fill.scale=tm_scale_intervals(
  #           style="pretty",
  #           breaks = c(-.4,-.3,-.2,-.1,0), # you need n+1 number of breaks
  #           n=4,#8 categories
  #           values="matplotlib.plasma")
  #       )
  #   })
  # })
  
  
  ## define server for static tmap - diff in RD (slider)------
  
  #note this object name (tmap_rd_diff) must match above
  output$tmap_static_rd_diff_slider = renderTmap({
    

    summary_df_target_perc_rd_diff_sf=all_scenarios_summary_by_zcta_pt_sf %>%
      filter(target_percentile_2dig==input$target_percentile_2dig) %>%
      dplyr::select(-contains("mean"))
    
    # define the tmap
    tmap_mode("plot")
    
    tm_obj_static=tm_basemap("CartoDB.Positron")+
      tm_shape(summary_df_target_perc_rd_diff_sf)+
      tm_polygons(
        fill = "rd_diff_sum",
        lwd=0.5,
        fill.legend= tm_legend("Difference in risk differences (per 100k)"),
        fill.scale=tm_scale_intervals(
          #Don't specify style so that the code listens to
          #the "breaks" code
#          style="pretty",
          breaks = c(-.4,-.3,-.2,-.1,0), # you need n+1 number of breaks
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
  
  ## define server for mapview-------
  
  output$mapview_rd_diff_slider = renderLeaflet({

    #create the dataset based on the scenario type.
    #This is the key step! The filters are used as inputs.
    #  #this is kind of like defining a function
    #Note I'm using the _2dig variable to make sure it's exactly
    #the same as the slider input
    summary_df_target_perc_rd_diff_sf=all_scenarios_summary_by_zcta_pt_sf %>%
      #set the target percentile equal to whatever value is selected
      filter(target_percentile_2dig==input$target_percentile_2dig) %>%
      #remove some vars
      dplyr::select(-contains("mean"))

   mapview_obj=mapview(
        summary_df_target_perc_rd_diff_sf,#maybe the pipe is the issue
        layer.name="Difference in risk differences per 100k (Target-Baseline)",
        zcol="rd_diff_sum",
        col.regions=viridis_pal(option="C"),
        lwd=1 #make line width less
        )

        mapview_obj@map

  })
  
  
  
  ## define server for histogram (slider)-------
  #as far as I can tell, this is exactly the same. The only difference is
  #how the value is selected (slider vs dropdown)
  output$histogram_rd_diff_slider <- renderPlot(  {
    
    #First create the dataset
    wf_ac_scenarios_all_pt %>% 
      #      filter(target_percentile_decile==1) %>% #don't need this step
      #the key step for shiny. The value corresponding to the slider input
      filter(target_percentile_2dig==input$target_percentile_2dig) %>% 
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
  

    
    
}

# Run app-----
shinyApp(ui = ui, server = server)

#slider idea
# ui <- page_fluid(
#   sliderInput(
#     "slider",
#     label = "Scenario",
#     min = .05,
#     max = 1,
#     value = 1,
#     step = .05
#   ),
#   plotOutput("plot") 
# )


#shinyApp(ui = ui, server = server)
