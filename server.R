library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)
library(USAboundaries)
library(htmltools)

##### SERVER #####
server <- function(input, output, session) {
  
  ### Update user inputs -----
  
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
  
  # Update week slider if a season with 53 weeks is selected
  observeEvent(input$run, {
    
    # Update slider based on season for 52/53 weeks
    if (input$season == "2014-2015") {
      week_choices <- c(as.character(40:53), as.character(1:18))
    } else {
      week_choices <- c(as.character(40:52), as.character(1:18))
    }
    
    # Can also set the label and select items
    updateSliderTextInput(session, inputId = "week",
                          label = "Calendar Week",
                          choices = week_choices,
                          selected = as.character(this_week)
    )
    
  })
  
  # Create datasets of forecasted values and truth -----
  # Update location and season only on clicks
  truth_1 <- eventReactive(input$run, {
    filter(all_observed, season == input$season, location == input$loc)
  },
  ignoreNULL = FALSE)
  
  forecasts_1 <- eventReactive(input$run, {
    filter(all_forecasts, season == input$season, location == input$loc,
           model == input$model)
  },
  ignoreNULL = FALSE)
  
  wk_label <- eventReactive(input$run, {
    if (input$season == "2014-2015") {
      c(as.character(seq(40, 52, 2)), as.character(seq(1, 21, 2)))
    } else {
      c(as.character(seq(40, 52, 2)), as.character(seq(2, 22, 2)))
    }
  },
  ignoreNULL = FALSE)
  
  max_week <- eventReactive(input$run, {
    week_inorder(22, input$season)
  },
  ignoreNULL = FALSE)
  
  # Create datasets of final observed truth and truth as known at week displayed
  final_truth <- reactive({
    filter(truth_1(), issue == min(max(issue), paste0(substr(season, 6, 9), 28))) %>%
      select(order_week, ILI)
  }) 
  
  current_truth <- reactive({
    
    if (as.numeric(input$week) < 40) {
      this_issue <- as.integer(paste0(substr(head(truth_1()$season, 1), 6, 9), 
                                      str_pad(input$week, 2, "left", "0")))
    } else {
      this_issue <- as.integer(paste0(substr(head(truth_1()$season, 1), 1, 4), input$week))
    }
    
    filter(truth_1(), issue == this_issue) %>%
      select(order_week, ILI, season, issue)
  })
  
  # Update week automatically as slider is moved
  order_week <- reactive({
    week_inorder(input$week, head(current_truth()$season, 1))
  })
  
  point_forecasts <- reactive({
    filter(forecasts_1(), order_week == order_week(), type == "Point") %>%
      select(target, value, model) %>%
      # Note week is the week BEING forecast, NOT the week forecast received
      mutate(order_week = order_week() + as.numeric(gsub(" wk ahead", "", target))) %>%
      select(-target) %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = order_week(),
                       # model = factor(unique(forecasts_1()$model)),
                       value = last(current_truth()$ILI)))
  })
  
  
  bounds <- reactive({
    filter(forecasts_1(), order_week == order_week(), type == "Bin") %>%
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
             order_week = order_week() + as.numeric(gsub(" wk ahead", "", target))) %>%
      ungroup() %>%
      # Only keep one copy of each week's upper and lower bound
      select(model, order_week, lower, upper) %>%
      distinct() %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = order_week(),
                       # model = factor(unique(forecasts_1()$model)),
                       lower = last(current_truth()$ILI),
                       upper = last(current_truth()$ILI)))
    
  })
  
  output$time_plot <- renderPlot({
    
    ggplot(point_forecasts()) +
      # Add shading for 80% CI for average
      geom_ribbon(data = bounds(), aes(x = order_week, ymin = lower, ymax = upper),
                  alpha = 0.3, fill = "tomato") +
      # Add full season observed values
      geom_line(data = final_truth(), aes(order_week, ILI), color = "darkgray", alpha = 1) +
      # Add observed values
      geom_line(data = current_truth(), aes(order_week, ILI), color = "black") +
      # Add point prediction lines for each team
      geom_line(aes(order_week, value), color = "tomato") +
      # Add horizontal line of baseline
      # geom_segment(aes(x = 40, xend = last_plot_week + 4,
      #                  y = 2.2, yend = 2.2),
      #              linetype = 3)  +
      labs(y = "Weighted ILI %") +
      scale_x_continuous(name = "MMWR Week",
                         limits = c(40, max_week()),
                         breaks = seq(40, max_week(), 2),
                         labels = wk_label()) +
      # scale_y_continuous(breaks = seq(0, ceiling(max(3, max(plot_points$value, na.rm=T),
      #                                                max(bounds$upper, na.rm=T))), 1),
      #                    limits = c(0, ceiling(max(3, max(plot_points$value, na.rm=T), 
      #                                              max(bounds$upper, na.rm=T),
      #                                              bounds$upper[bounds$week == last_week + 2] + 0.75)))) +
      theme_classic()
    
    
  })
  
  # Create leaflet map
  output$map_plot <- renderLeaflet({
    leaflet(state_shapes2) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 3) %>%
      addTiles() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", ILI)(ILI),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~paste0("<b>", name, "</b><br>",  round(ILI, 3))) 
  })
}
?navbarPage
