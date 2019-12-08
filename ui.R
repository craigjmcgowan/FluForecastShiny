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

options(shiny.sanitize.errors = FALSE)

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
                                            "2017-2018", "2018-2019", "2019-2020"),
                                selected = "2019-2020"),
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
    tags$h3("Outpatient Influenza-like Illness"),
    tags$div(
      tags$p(
        "Influenza is a difficult disease to track, since the symptoms that it causes are also caused by many 
        pathogens, such as coronaviruses and rhinoviruses. While a laboratory test exists to confirm influenza
        infection, it is not widely used in outpatient visits as the course of treatment for all of the viruses 
        that cause these symptoms is similar unless the infection is caught quickly. As such, surveillance of 
        influenza, especially outpatient influenza, often focuses on influenza-like illness as a proxy."
      ),
      tags$p(
        "In the United States, surveillance of influenza activity is coordinated by the ",
        tags$a(
          href = "https://www.cdc.gov/flu/weekly/overview.htm", "Centers for Disease Control and Prevention."
        ),
        " Levels of outpatient influenza are reported from the US Outpatient Influenza-like Illness Surveillance 
        Network (ILINet), a network of 2,200 outpatient providers that report weekly data on the total number of 
        patients seen that week, along with the number of patients with influenza-like illness (ILI), defined as 
        a fever and either cough or sore throat, without a known cause other than influenza. The number of patients
        with ILI is divided by the total number of patients to create a percentage of outpatient visits due to ILI.
        The raw percentages are reported for individual states, while for Health and Human Services Regions or 
        national estimates the state percentages are weighted by state population before being combined."
      )
    )
  ),
  
  tabPanel(
    "How are these forecasts made?",
    tags$div(
      tags$h2("Forecast Construction"),
      tags$p(
        "Weekly forecasts are created separately for each location (US, each HHS Region, each state) based on prior 
        weeks' levels of influenza-like illness (ILI), current cumulative percentages of infections due to each 
        of H1N1 and H3N2 influenza viruses, and Google Trends data based on searches for 'flu'. Four related models
        are fit for each location - a weighted ensemble model, a dynamic harmonic regression model, a 
        static historical average model, and a historical average model weighted by influenza virus type prevalence."
      ),
      tags$h4("Ensemble Model"),
      tags$p(
        "The ensemble model is a combination of the forecasts from the three other models. Ensembles have
        a ", 
        tags$a(
          href = "https://www.analyticsvidhya.com/blog/2015/08/introduction-ensemble-learning/", "strong track record"
        ), 
        "of performance in machine learning and forecasting. The idea is to combine predictions from several less
        powerful models to improve the final predictions. Protea's ensemble model combines predictions from the 
        other three models using a weighted average. Models are assigned different weights depending on the location,
        target, and time of year, with the values of the weights determined using predictions from training seasons."
      ),
      tags$h4("Dynamic Harmonic Model"),
      tags$p(
        ""
      )
    )
  ),

  tabPanel(
    "How accurate are the forecasts?",
    tags$div(
      tags$h2("Forecast Accuracy"),
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
                by the model to the true, observed value. It is related to the log score, which is calculated by 
                taking the natural logarithm of the probability the model assigns to the correct outcome. The 
                average of these log is exponentiated, with the final result representing the geometric mean of 
                the probability assigned to the observed outcome. A ", tags$b('higher'), " geometric mean 
                probability is a better score.")
      ),
      tags$p(
        "Use the drop down menus below to explore how forecast accuracy varies by location, season, and model type."
      )
    ),
    fluidRow(
      column(3,
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
      column(3,
             selectInput("scoreSeason", label = "Season",
                         choices = c("All seasons" = "", "2014-2015", "2015-2016", "2016-2017",
                                     "2017-2018", "2018-2019", "2019-2020"),
                         multiple = TRUE)
             ),
      column(3,
             selectInput("scoreModel", label = "Model",
                         choices = c("All models" = "", "Ensemble" = "Ensemble",
                                     "Harmonic Regression" = "Dynamic Harmonic Model",
                                     "Subtype-weighted Historical Average" =
                                       "Subtype Historical Average",
                                     "Unweighted Historical Average" = "Historical Average"),
                         multiple = TRUE)
             ),
      column(3,
             checkboxInput("groupTarget", label = "Separate Scores by Target",
                           value = TRUE)
      )
    ),
    hr(),
    DT::dataTableOutput("scoreTable")
  ),
  
  tabPanel(
    "FAQs",
    "This page is currently under construction - check back later!"
  ),
  
  tabPanel(
    "Contact",
    tags$div(
      tags$p(
        "If you have any additionals questions or comments, please feel free to reach out to ",
        tags$a(href = "mailto:craig.mcgowan@proteaanalytics.com", "craig.mcgowan@proteaanalytics.com")
      )
    )
  )
)
