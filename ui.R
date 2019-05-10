library(shiny)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(leaflet)
library(USAboundaries)
library(htmltools)
library(FluSight)
library(MMWRweek)
library(sf)

##### UI #####
navbarPage(
  "Protea Analytics", id = "nav",
           
  tabPanel(
    "Influenza Forecasts",
    
    div(
      class = "outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
      leafletOutput("mapPlot", width = "100%", height = "100%"),
      
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
                                choices = c("Ensemble",
                                            "Harmonic Regression" = "Dynamic Harmonic Model",
                                            "Subtype-weighted Historical Average" =
                                              "Subtype Historical Average",
                                            "Unweighted Historical Average" = "Historical Average"),
                                selected = "Ensemble"),
                    selectInput("pred_int", label = "Prediction Interval",
                                choices = c("80%", "50%", "None"),
                                selected = "80%"),
                    
                   
                    # Plot
                    plotOutput("time_plot", height = 300),
                    
                    
                    sliderTextInput(inputId = "week", label = "Calendar Week",
                                    choices = c(as.character(40:52), as.character(1:18)),
                                    selected = as.character(this_week), grid = TRUE,
                                    hide_min_max = TRUE)
      )
      
    )
  ),
  
  tabPanel(
    "What's being measured?",
    tags$h1("Measurement of Influenza Activity"),
    tags$h3("Outpatient Influenza-Like Illness"),
    "Influenza is a difficult disease to track, since the symptoms that it causes are also caused by many 
    pathogens, such as coronaviruses and rhinoviruses. While a laboratory test exists to confirm influenza
    infection, it is not widely used in outpatient visits as the treatment for all of the viruses that cause
    these symptoms is similar unless the infection is caught quickly."
    
  ),
  
  tabPanel(
    "How are these forecasts made?",
    "This page is currently under construction - check back later!"
  ),

  tabPanel(
    "How accurate are the forecasts?",
    tags$div(
      tags$p(
        "Forecast accuracy is evaluated in two different ways, which each measure slightly different aspects of 
        the forecast."
      ),
      tags$ul(
        tags$li(tags$b("Mean Absolute Error (MAE)"), " - Mean absolute error examines the accuracy of the point 
                forecasts at future weeks, and represents the average amount that those point forecasts differ
                from the observed values. An MAE of 0.2 means that those forecasts differ from the observed value
                by 0.2%, on average. A ", tags$b("lower"), " MAE is a better score."), 
        tags$li(tags$b("Geometric Mean Probability"), " - This score is the geometric mean probability assigned 
                to the observed value. It is related to the log score, which is calculated by taking the natural
                logarithm of the probability the model assigns to the correct outcome. The average of these log 
                is exponentiated, with the final result representing the geometric mean of the probability
                assigned to the observed outcome. A ", tags$b('higher'), " geometric mean probability is a better 
                score.")
      ),
      tags$p(
        "Use the drop down menus below to explore how forecast accuracy varies by location, season, and model type."
      )
    ),
    fluidRow(
      column(4,
             selectInput("scoreLocation", "Location",
                         choices = list(`All locations` = "", 
                                        `USA` = "US National",
                                        `HHS Regions` = c("HHS Region 1", "HHS Region 2",
                                                             "HHS Region 3", "HHS Region 4", 
                                                             "HHS Region 5", "HHS Region 6",
                                                             "HHS Region 7", "HHS Region 8", 
                                                             "HHS Region 9", "HHS Region 10"),
                                        `States` = c(state.name[state.name != "Florida"])),
                         multiple = TRUE)
             ),
      column(4,
             selectInput("scoreSeason", label = "Season",
                         choices = c("All seasons" = "", "2014-2015", "2015-2016", "2016-2017",
                                     "2017-2018", "2018-2019"),
                         multiple = TRUE)
             ),
      column(4,
             selectInput("scoreModel", label = "Model",
                         choices = c("All models" = "", "Ensemble" = "Ensemble",
                                     "Harmonic Regression" = "Dynamic Harmonic Model",
                                     "Subtype-weighted Historical Average" =
                                       "Subtype Historical Average",
                                     "Unweighted Historical Average" = "Historical Average"),
                         multiple = TRUE)
             )
      ),
    fluidRow(
      column(4,
             selectInput("scoreType", label = "Scoring Metric",
                         choices = c("Mean Absolute Error" = "mae",
                                     "Geometric Mean Probability" = "log"),
                         selected = "mae")
      ),
      column(4,
             checkboxInput("groupTarget", label = "Separate Scores by Target",
                           value = TRUE)
      )
    ),
    hr(),
    dataTableOutput("scoreTable")
  ),
  
  tabPanel(
    "FAQs",
    "This page is currently under construction - check back later!"
  ),
  
  tabPanel(
    "Contact",
    "This page is currently under construction - check back later!"
  )
)
