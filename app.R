library(shiny)
library(dplyr)
library(ggplot2)

# Define UI plotting application
ui <- fluidPage(
   
   # Application title
   titlePanel("Influenza Forecasts"),
   
   # Sidebar with inputs for resolution, location, season 
   sidebarLayout(
      sidebarPanel(
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
         actionButton("run", label = "Update")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("time_plot"),
        
        sliderInput("week", label = "Calendar Week", min = 40, max = 73,
                    value = 56, step = 1, round = TRUE)
        
      )
   )
)

# Define server logic required to plot flu trends
server <- function(input, output, session) {
  
  # Update the location dropdown box based on user input for resolution
  observe({
    if (input$res == "nat") choices <- c("USA" = "US National")
    if (input$res == "reg") choices <- c("HHS Region 1", "HHS Region 2", 
                                         "HHS Region 3", "HHS Region 4", 
                                         "HHS Region 5", "HHS Region 6", 
                                         "HHS Region 7", "HHS Region 8",
                                         "HHS Region 9", "HHS Region 10")
    if (input$res == "state") choices <- state.name
    
    # Can also set the label and select items
    updateSelectInput(session, "loc",
                      label = "Location",
                      choices = choices,
                      selected = head(choices, 1)
    )
  })
  
  # Create datasets of forecasted values and truth -----
  # Update location and season only on clicks
  truth_1 <- eventReactive(input$run, {
    filter(all_observed, season == input$season, location == input$loc,
           lag == 0) 
  },
  ignoreNULL = FALSE)
  
  forecasts_1 <- eventReactive(input$run, {
    filter(all_forecasts, season == input$season, location == input$loc,
           model == input$model)
  },
  ignoreNULL = FALSE)
  
  # Update week automatically as slider is moved
  point_observed <- reactive({
    filter(truth_1(), order_week <= input$week) %>%
      select(order_week, ILI)
  })
  
  point_forecasts <- reactive({
    filter(forecasts_1(), order_week == input$week, type == "Point") %>%
      select(target, value, model) %>%
      # Note week is the week BEING forecast, NOT the week forecast received
      mutate(order_week = input$week + as.numeric(gsub(" wk ahead", "", target))) %>%
      select(-target) %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = input$week,
                       # model = factor(unique(forecasts_1()$model)),
                       value = last(point_observed()$ILI)))
  })
  
  wk_label <- reactive({
    wk_label[ifelse(as.numeric(wk_label) >= 40, as.numeric(wk_label),
                    as.numeric(wk_label) + 52) <= max(point_forecasts()$week)]
  })
  
  bounds <- reactive({
    filter(forecasts_1(), order_week == input$week, type == "Bin") %>%
      # Determine upper and lower bounds for each target
      group_by(model, target) %>%
      # Calculate cumulative probability for each bin
      mutate(cumprob = cumsum(value)) %>%
      # Only keep the rows within the 80% confidence range
      filter(row_number() %in% 
               c(max(which(cumprob < 0.1)), min(which(cumprob > 0.9)))) %>%
      # Create lower and upper bounds as min and max of the remaining probabilities
      mutate(lower = min(as.numeric(bin_start_incl)),
             upper = max(as.numeric(bin_start_incl)),
             order_week = input$week + as.numeric(gsub(" wk ahead", "", target))) %>%
      ungroup() %>%
      # Only keep one copy of each week's upper and lower bound
      select(model, order_week, lower, upper) %>%
      distinct() %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = input$week,
                       # model = factor(unique(forecasts_1()$model)),
                       lower = last(point_observed()$ILI),
                       upper = last(point_observed()$ILI)))
    
  })
  
  
  output$time_plot <- renderPlot({
    
    ggplot(point_forecasts()) +
      # Add shading for 80% CI for average
      geom_ribbon(data = bounds(), aes(x = order_week, ymin = lower, ymax = upper),
                  alpha = 0.3, fill = "tomato") +
      # Add point prediction lines for each team
      geom_line(aes(order_week, value), color = "tomato") +
      # Add observed values
      geom_line(data = point_observed(), aes(order_week, ILI), color = "black") +
      # Add horizontal line of baseline
      # geom_segment(aes(x = 40, xend = last_plot_week + 4,
      #                  y = 2.2, yend = 2.2),
      #              linetype = 3)  +
      theme_minimal() +
      labs(y = "Weighted ILI %") #+
      # scale_x_continuous(name = "MMWR Week",
      #                    breaks = seq(40, max(point_forecasts()$order_week), 2),
      #                    labels = wk_label()) +
      # scale_y_continuous(breaks = seq(0, ceiling(max(3, max(plot_points$value, na.rm=T),
      #                                                max(bounds$upper, na.rm=T))), 1),
      #                    limits = c(0, ceiling(max(3, max(plot_points$value, na.rm=T), 
      #                                              max(bounds$upper, na.rm=T),
      #                                              bounds$upper[bounds$week == last_week + 2] + 0.75)))) +

    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

