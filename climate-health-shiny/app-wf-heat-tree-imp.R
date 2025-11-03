
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

# May 26, 2025
#Revise this to only include tree canopy and AC
#Fewer choices

#4 pm cut the histogram for parsimony. 

#Nov 3, 2025:
#Major changes
#adding impervoius surfaces in place of AC
#adding two outcomes now (previously just wildfire)
#This now has a new filename. See previous version here
#climate-health-shiny/app-wf-int.R

#Also note that the target_percentile_unified variable is different

#TO make them foldable I can use "accordion"

#Also,
#Now that we have titles in the accordion, I don't need the titles
#in the cards as well


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


## Load data within shiny app------

#shiny prefers that the data is relative to the shiny app
#Note shinyapps.io doesn't seem to like it when I change my working directory, so try
#relative to the app script, not the project. unusual for RStudio
#see  heat-hosp-california/shiny/tmap-static/app.R
#April 22, 2025: update - omit race variables from scenarios
load("data/zcta_ca_geo_simplified.RData")

zcta_geo_only=zcta_ca_geo_simplified %>% 
  dplyr::select(zcta,geometry)

#this is created here
#climate-health-shiny/manage-data-for-app.R

load("data/scenarios_summary_overall_pt_ci.RData")
load("data/scenarios_summary_zcta_for_map.RData")


## Define global variables------
#intervention measures
intervene_var_options=scenarios_summary_overall_pt_ci %>% 
  distinct(intervene_var) %>% 
  pull()

intervene_var_options

#outcomes
outcome_options=scenarios_summary_overall_pt_ci %>% 
  distinct(outcome) %>% 
  pull()

outcome_options

outcome_label_options=scenarios_summary_overall_pt_ci %>% 
  distinct(outcome_label) %>% 
  pull()
outcome_label_options




# Define UI part-------
#Use a sidebar like with Maren's
#May 12, 2025
#either page_fillable or page_fluid works. I actually prefer
#fluid, I think
#page_fillable keeps the sidebar right next to everything, though,
#which is nice

ui <- page_fillable(
#ui <- page_fluid(  
  title="Green Space × Heat & Wildfire", #appears in browser
  #the title below is what appears on the website
  h1("Simulate how green space modifies effects of heat and wildfire"),

  #gpt suggested replacing this card with a details()
  # card(
  #   height = 800,
  #     p("Question: How would hypothetically changing the distribution of either
  #     impervious surface or tree canopy affect the burden and spatial distribution of
  #     heat- and wildfire-related healthcare utilization?"),
  #     p("Target percentile refers to the percentile of the intervention variable
  #       to which all ZCTAs with values below that percentile value would be raised under
  #       the target scenario.
  #       For example, if the target scenario corresponds to the 60th percentile of 
  #       tree canopy, all ZCTAs
  #       below the 60th percentile would have their tree canopy raised to the 60th percentile."),
  #     p(" For further discussion, please see 
  #       https://michaeldgarber.github.io/climate-health/describe-code-pipeline")
  #   
  #   ),
  
  tags$details(
    tags$summary("Description (click to expand)"),
    p("Question: How would hypothetically changing the distribution of either
     impervious surface or tree canopy affect the burden and spatial distribution
     of heat- and wildfire-related healthcare utilization?"),
    p("Target percentile refers to the percentile of the intervention variable
     to which all ZCTAs with values below that percentile value would be raised
     under the target scenario."),
    
    p("For impervious surfaces, target percentiles are computed based on the complement of 
    the impervious-surfaces measure so that higher percentiles refer to lower levels of 
    impervious surfaces.
    For example, the percentile of 0.8 corresponds to the 20th percentile of 
    impervious surfaces. 
    For tree canopy, 
      target percentiles are directly computed based on the tree-canopy proportion."),
    
    p("For further discussion, see ",
      a(href = "https://michaeldgarber.github.io/climate-health/describe-code-pipeline",
        "code pipeline notes", target = "_blank"), ".")
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
      
      ## select intervention measures-------
      #add another sidebar that selects the EMM meausure
      #Follow along here to customize the names of the choices that appear
      
      #     title = "Select effect-measure modifier",
      selectInput(
        "intervene_var", #the variable to select
        label="Intervention variable:", #the label
        #          label=c("1","2","3","4","5","6","7","8","9"),
        
        #choices can simply be a vector of the variable names
        #or the names can be changed
        #Prop. instead of proportion
        choices=list(
          "Impervous surfaces (%)"="imperv",
          "Tree canopy (%)"="tree"
        ),
#        choices=intervene_var_options,
        
        #can adjust the number to show so that the box is bigger
        selectize = F,
        size=2
      ), #the choices
      
      ## Select outcome---------
      selectInput(
        "outcome", #the variable to select
        label="Outcome variable:", #the label
        #          label=c("1","2","3","4","5","6","7","8","9"),
        
        #choices can simply be a vector of the variable names
        #or the names can be changed
        #Prop. instead of proportion
        choices=list(
          "Heat"="heat",
          "Wildfire"="wf"
        ),
        #        choices=intervene_var_options,
        
        #can adjust the number to show so that the box is bigger
        selectize = F,
        size=2
      ), #the choices

      ## Define slider inputs (target percentile)-----
      
      sliderInput(
        
        "target_percentile_unified", #object name of the variable to select
        "Target percentile:",
        min = 0,
        max = 1,
        value = 0.6,  #what it goes to by default
        step = 0.2,  #the key part
        
        #with the slider, you can specify the "step",
        #how big the increment is between values
        #https://shiny.posit.co/r/articles/build/sliders/
        
        round=2#this seems necessary to get the numbers to be exactly the same..
      )
      ), # closes sidebar, but keep layout_sidebar() open


    ## summary table card----
    #April 21, 2025: make this an accordion.
    #Nov 3, 2025: Make an accordion panel so they're foldable.

    accordion(
      id="folds",
      multiple=TRUE,
      open=c("summary"),
      
      accordion_panel(
        "Summary table",
        value="summary",
       card(
           full_screen = T,
           height = 300,
           #Dont' need because we have an accordion title
#           card_header("Summary table"),
            tableOutput("table_summary")
            
        )
       ),#close accordion_panel()

    ##static tmap of diff in RD (slider input) ------
    accordion_panel(
      "Static map: difference in rate differences (Target - Baseline)",
      value = "static_map",
    card(
      #omit card_header()
#      card_header("Static map: difference in rate differences (Target - Baseline scenario)"),
      p(
        "The difference in rate differences (RDs) per 100,000 person-days,
        comparing the target with the baseline scenarios."),
      #custom name; match it below
      tmapOutput(
        height = "650px" ,
        "tmap_static_rd_diff_slider"
                 )
        )
      ), #close accordion_panel() . need a comma
    
    ##interactive mapview of diff in RD--------
    #let's add mapview here as well. why not
    accordion_panel(
      "Interactive map: difference in rate differences (Target - Baseline)",
      value = "interactive_map",
      card(
      
      full_screen = TRUE,
#      card_header("Interactive map: difference in rate differences (Target-Baseline) [Slider]"),
      p(
        "The difference in rate differences (RDs) per 100,000 person-days,
        comparing the target with the baseline scenarios."),
      #    https://r-spatial.github.io/mapview/reference/mapviewOutput.html
      leafletOutput("mapview_rd_diff_slider")
      )
        ) #close accordion_panel()
      ) #close accordion()
    ) # close layout_sidebar()
  ) #closes page_fillable()
#names(scenarios_summary_overall_pt_ci)


# Server part------
#This is where we define the objects and plots such as maps and plots
#Feb 24, 2024: updates to include the "proxy" thing
# define the tmap

#Nov 3, 2025: to make it faster to load I can add the req() guard
#at the top of each map


tmap_mode("plot")
server <- function(input, output, session) {
  
  
  # Define filtered data appended to map just once------
  #April 21, 2025: idea to lighten this up.
  #Don't begin with this huge sf file. Begin with the aspatial file
  #and then link to the spatial data at the very end so you don't
  #carry around multiple instances of the spatial data
  
  #Nov 3, 2025
  #chatgpt suggests I define this once near the top of server and then use it
  #throughout for speed.
  #I"m using a "reactive" here
  #“Create a reactive expression that filters and joins the data, 
  #then attach caching so it only recomputes when those three input values change.”
  
  df_for_map <- reactive({
    input_df_for_map = scenarios_summary_zcta_for_map %>%
      dplyr::filter(
        intervene_var == input$intervene_var,
        outcome == input$outcome,
        round(target_percentile_unified, 2) == input$target_percentile_unified
      ) %>%
      dplyr::select(-dplyr::contains("mean"))
    
    sf_obj_for_map = zcta_geo_only %>%
      dplyr::left_join(input_df_for_map, by = "zcta")
    
    # return value of the reactive
    sf_obj_for_map
  }) %>%
    bindCache(input$intervene_var, input$outcome, input$target_percentile_unified)
  
  
  

  # summary table------
  output$table_summary = renderTable({
    table_out_summary=scenarios_summary_overall_pt_ci %>% 
      #filter to the corresponding intervention variable
      filter(intervene_var==input$intervene_var) %>% 
      #filter to outcome of interest
      filter(outcome==input$outcome) %>% 
      #filter to the corresponding target percentile
      filter(
        #this round step to get it to respond better to the equal sign
        round(target_percentile_unified,2)==input$target_percentile_unified) %>% 
    
    #Work on formatting of numbers as I did with Maren's, 
      #but otherwise this is working.
      #Concatenate values so that the confidence intervals are included in the same
      #column
      mutate(
        n_zcta_char=as.character(
          formatC(n_zcta,digits=0,format="f",big.mark=",")
        ),
        pop_char=as.character(
          formatC(pop,digits=0,format="f",big.mark=",")
        ),
        
        #Just express the difference in terms of RD and n total cases
        rd_100k_diff_pred_pt_char=as.character(
          formatC(rd_100k_diff_pred_pt,digits=2,format="f",big.mark=",")
        ),
        rd_100k_diff_pred_ll_char=as.character(
          formatC(rd_100k_diff_pred_ll,digits=2,format="f",big.mark=",")
        ),
        rd_100k_diff_pred_ul_char=as.character(
          formatC(rd_100k_diff_pred_ul,digits=2,format="f",big.mark=",")
        ),
        
        #n cases diff diff
        n_cases_diff_diff_pred_pt_char=as.character(
          formatC(n_cases_diff_diff_pred_pt,digits=1,format="f",big.mark=",")
        ),
        n_cases_diff_diff_pred_ll_char=as.character(
          formatC(n_cases_diff_diff_pred_ll,digits=1,format="f",big.mark=",")
        ),
        n_cases_diff_diff_pred_ul_char=as.character(
          formatC(n_cases_diff_diff_pred_ul,digits=1,format="f",big.mark=",")
        ),
        
        #now concatenate them together using paste0
        rd_100k_diff_pred_conc = paste0(
          rd_100k_diff_pred_pt_char,  " (", 
          rd_100k_diff_pred_ll_char, ", ",
          rd_100k_diff_pred_ul_char,  ")"
        ),
        n_cases_diff_diff_pred_conc = paste0(
          n_cases_diff_diff_pred_pt_char,  " (", 
          n_cases_diff_diff_pred_ll_char, ", ",
          n_cases_diff_diff_pred_ul_char,  ")"
        ),
        

      ) %>% 
      dplyr::select(
        intervene_var,
        target_percentile_unified, n_zcta_char,
        pop_char, contains("conc")) %>% 
      #now rename those variables to display in the table
      #I'm missing population affected here.
      rename(
        "Intervention variable"=intervene_var,
        "Target percentile" = target_percentile_unified,
        "N, ZCTAs intervened" = n_zcta_char,
        "Population affected" =pop_char,
        "Diff. in RD per 100k, alt- status quo"=rd_100k_diff_pred_conc,
        "Diff. in N, cases, alt- status quo"=n_cases_diff_diff_pred_conc
      )
    
    table_out_summary
  })



  # static tmap - diff in RD (slider)------
  #comment out; crashing server
  #note this object name (tmap_rd_diff) must match above
  output$tmap_static_rd_diff_slider = renderTmap({

    #This code should make it only render if it's open
    req("static_map" %in% input$folds)   # only run when panel is open
    
    #This is a sort of function
    sf_obj_for_map <- df_for_map()


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
        fill = "rd_100k_diff_pred_pt",
        lwd=0.5,
        fill.legend= tm_legend("Diff. in RDs per 100k"),
        fill.scale=tm_scale_intervals(
          #Don't specify style so that the code listens to
          #the "breaks" code
#          style="pretty",
          # you need n+1 number of breaks
#          breaks = c(-.4,-.3,-.2,-.1,0), 
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
  
  # mapview-------
  
    
  output$mapview_rd_diff_slider = renderLeaflet({

    req("interactive_map" %in% input$folds)   # only run when panel is open
    
    sf_obj_for_map <- df_for_map()


    mapview_obj=mapview(
      sf_obj_for_map,#maybe the pipe is the issue
      layer.name="Diff. in RD per 100k (Target-Baseline)",
      zcol="rd_100k_diff_pred_pt",
      col.regions=viridis_pal(option="C"),
      lwd=.5 #make line width less
    )
    
    #prefer canvas for faster rendering
    mapview_obj@map 
    
  })

  
  # code to suppress maps unless open-------
  # Make hidden outputs pause work (good with accordions)
  outputOptions(output, "tmap_static_rd_diff_slider", suspendWhenHidden = TRUE)
  outputOptions(output, "mapview_rd_diff_slider",    suspendWhenHidden = TRUE)
 

}

# Run app-----
shinyApp(ui = ui, server = server)

