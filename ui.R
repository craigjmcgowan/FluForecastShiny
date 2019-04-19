library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)
library(USAboundaries)
library(htmltools)

##### UI #####
navbarPage(
  "Influenza Forecasts", id = "nav",
           
  tabPanel(
    "Interactive Map",
    
    div(
      class = "outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
      leafletOutput("map_plot", width = "100%", height = "100%"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 400, height = "auto",
                    
                    h2("Forecast Options"),
                    
                    selectInput("res", label = "Forecast Resolution",
                                choices = c("National" = "nat",
                                            "HHS Regions" = "reg",
                                            "States" = "state"),
                                selected = "nat"),
                    selectInput("loc", label = "Location",
                                choices = c("USA" = "US National", "HHS Region 1", "HHS Region 2",
                                            "HHS Region 3", "HHS Region 4", "HHS Region 5",
                                            "HHS Region 6", "HHS Region 7", "HHS Region 8",
                                            "HHS Region 9", "HHS Region 10", state.name)),
                    selectInput("season", label = "Season",
                                choices = c("2014-2015", "2015-2016", "2016-2017",
                                            "2017-2018", "2018-2019"),
                                selected = "2018-2019"),
                    selectInput("model", label = "Model",
                                choices = c("Ensemble" = "ens-month-target-type-based-weights",
                                            "Harmonic Regression" = "Dynamic Harmonic Model",
                                            "Subtype-weighted Historical Average" =
                                              "Subtype Historical Average",
                                            "Unweighted Historical Average" = "Historical Average"),
                                selected = "ens-month-target-type-based-weights"),
                    
                   
                    # Plot
                    plotOutput("time_plot", height = 300),
                    
                    
                    sliderTextInput(inputId = "week", label = "Calendar Week",
                                    choices = c(as.character(40:52), as.character(1:18)),
                                    selected = as.character(this_week), grid = TRUE,
                                    hide_min_max = TRUE)
      )
      
      # tags$div(id="cite",
      #          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      # )
    )
  ),
  
  tabPanel(
    "What's being measured?"
  ),
  
  tabPanel(
    "How are these forecasts made?"
  ),
  
  tabPanel(
    "How accurate are the forecasts?"
  ),
  
  tabPanel(
    "FAQs"
  ),
  
  tabPanel(
    "Contact me"
  )
)
