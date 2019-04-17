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
                    width = 500, height = "auto",
                    
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
                   
                    actionButton("run", label = "Update"),
                    
                   
                    # Plot
                    plotOutput("time_plot", height = 200),
                    
                    
                    sliderTextInput(inputId = "week", label = "Calendar Week",
                                    choices = c(as.character(40:52), as.character(1:18)),
                                    selected = as.character(this_week), grid = TRUE,
                                    hide_min_max = TRUE)
      )
      
      # tags$div(id="cite",
      #          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      # )
    )
  )
)
  # # Application title
  # titlePanel("Influenza Forecasts"),
  # 
  # # Sidebar with inputs for resolution, location, season 
  # fluidRow(
  #   
  #   column(
  #     6,
  #     leafletOutput("map_plot")
  #   ),
  #   
  #   column(
  #     6,
  #     plotOutput("time_plot"),
  #     
      # sliderTextInput(inputId = "week", label = "Calendar Week",
      #                 choices = c(as.character(40:52), as.character(1:18)),
      #                 selected = as.character(this_week), grid = TRUE,
      #                 hide_min_max = TRUE)
  #   )
  #   
  # ),
  # 
  # fluidRow(
  #   
  #   column(
  #     4,
  #     selectInput("res", label = "Forecast Resolution",
  #                 choices = c("National" = "nat",
  #                             "HHS Regions" = "reg",
  #                             "States" = "state"),
  #                 selected = "nat"),
  #     selectInput("loc", label = "Location",
  #                 choices = c("USA" = "US National", "HHS Region 1", "HHS Region 2", 
  #                             "HHS Region 3", "HHS Region 4", "HHS Region 5",
  #                             "HHS Region 6", "HHS Region 7", "HHS Region 8",
  #                             "HHS Region 9", "HHS Region 10", state.name))
  #   ),
  #   
  #   column(
  #     4,
      # selectInput("season", label = "Season",
      #             choices = c("2014-2015", "2015-2016", "2016-2017",
      #                         "2017-2018", "2018-2019"),
      #             selected = "2018-2019"),
      # selectInput("model", label = "Model",
      #             choices = c("Ensemble" = "ens-month-target-type-based-weights",
      #                         "Harmonic Regression" = "Dynamic Harmonic Model",
      #                         "Subtype-weighted Historical Average" =
      #                           "Subtype Historical Average",
      #                         "Unweighted Historical Average" = "Historical Average"),
      #             selected = "ens-month-target-type-based-weights")
  #   ),
  #   
  #   column(
  #     4,
      # actionButton("run", label = "Update")
  #   )
  #   
  #   
  # )
  # 
  # sidebarLayout(
  #    sidebarPanel(
  #       selectInput("res", label = "Forecast Resolution",
  #                   choices = c("National" = "nat",
  #                               "HHS Regions" = "reg",
  #                               "States" = "state"),
  #                   selected = "nat"),
  #       selectInput("loc", label = "Location",
  #                   choices = c("USA" = "US National", "HHS Region 1", "HHS Region 2", 
  #                               "HHS Region 3", "HHS Region 4", "HHS Region 5",
  #                               "HHS Region 6", "HHS Region 7", "HHS Region 8",
  #                               "HHS Region 9", "HHS Region 10", state.name)),
  #       selectInput("season", label = "Season",
  #                   choices = c("2014-2015", "2015-2016", "2016-2017",
  #                               "2017-2018", "2018-2019"),
  #                   selected = "2018-2019"),
  #       selectInput("model", label = "Model",
  #                   choices = c("Ensemble" = "ens-month-target-type-based-weights",
  #                               "Harmonic Regression" = "Dynamic Harmonic Model",
  #                               "Subtype-weighted Historical Average" = 
  #                                 "Subtype Historical Average",
  #                               "Unweighted Historical Average" = "Historical Average"),
  #                   selected = "ens-month-target-type-based-weights"),
  #       actionButton("run", label = "Update")
  #    ),
  #    
  #    # Show a plot of the generated distribution
  #    mainPanel(
  #      plotOutput("time_plot"),
  #      
  #      sliderTextInput(inputId = "week", label = "Calendar Week", 
  #                      choices = c(as.character(40:52), as.character(1:18)),
  #                      selected = as.character(this_week), grid = TRUE,
  #                      hide_min_max = TRUE)
  #      
  #    )
  # )
# )
