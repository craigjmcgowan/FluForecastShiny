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

##### SERVER #####
server <- function(input, output, session) {
  
  # Show modal that forecasting has concluded for the year
  forecasting_pause <- modalDialog(
    "Influenza forecasting has concluded for the 2018-2019 influenza season. 
     You can continue to see forecasts for prior weeks in this season and earlier seasons, as well as 
     explore accuracy measures for forecasts. Check back in late October 2019 for the first forecasts
     for the 2019-2020 season!",
    title = "Forecasting Concluded for 2018-2019",
    easyClose = TRUE
  )
  
  showModal(forecasting_pause)
  
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
    
    click <- input$mapPlot_shape_click
    
    # Save state clicked on - this will be NULL if clicked outside of a state area
    if (!is.null(click)) {
      clickState <- state_revgeocode(click$lng, click$lat)
    } else {
      clickState <- NULL
    }
    
    # If a state was clicked on, update the selected variable accordingly
    if (!is.null(clickState)) {
      if (input$res == "nat") clickLocation <- head(choices, 1)
      if (input$res == "reg") clickLocation <- state_region_mapper(clickState)
      if (input$res == "state") clickLocation <- clickState
    }
    
    # Check for click input
    if (!is.null(clickState) && clickLocation %in% choices) {
      selected <- clickLocation
    } else {
      selected <- head(choices, 1)
    }
    
    # Can also set the label and select items
    updateSelectInput(session, "loc",
                      label = "Location",
                      choices = choices,
                      selected = selected
    )
  })
  
  # Update week slider if a season with 53 weeks is selected
  observe({
    
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
  
  # Update week automatically as slider is moved
  order_week <- reactive({
    week_inorder(input$week, input$season)
  })
  
  # Create datasets of forecasted values and truth -----
  
  # Create datasets of final observed truth and truth as known at week displayed
  final_truth <- reactive({
    filter(final_ili, location == input$loc, season == input$season) %>%
      select(order_week, ILI, season)
  }) 
  
  current_truth <- reactive({
    
    if (input$res == 'state' & input$season %in% c("2014-2015", "2015-2016", "2016-2017")) {
      
      filter(final_ili, season == input$season, order_week <= order_week()) %>%
        select(season, location, order_week, ILI)
    
    } else {
    
      this_issue <- ifelse(
        as.numeric(input$week) < 40,
        as.integer(paste0(substr(head(final_truth()$season, 1), 6, 9), 
                          str_pad(input$week, 2, "left", "0"))),
        as.integer(paste0(substr(head(final_truth()$season, 1), 1, 4), input$week))
      )
      
      filter(rolling_ili, season == input$season, issue == this_issue) %>%
        select(season, location, order_week, ILI)
    }
  })

  current_truth_location <- reactive({
    filter(current_truth(), location == input$loc) %>%
      select(order_week, ILI, season)
  })
  
  # Update location and season only on clicks
  forecasts_1 <- reactive({
    filter(all_forecasts, season == input$season, location == input$loc,
           model == input$model)
  })
  
  wk_label <- reactive({
    if (input$season == "2014-2015") {
      c(as.character(seq(40, 52, 2)), as.character(seq(1, 21, 2)))
    } else {
      c(as.character(seq(40, 52, 2)), as.character(seq(2, 22, 2)))
    }
  })
  
  max_week <- reactive({
    week_inorder(22, input$season)
  })
  
  
  
  point_forecasts <- reactive({
    filter(forecasts_1(), order_week == order_week()) %>%
      select(target, value, model) %>%
      # Note week is the week BEING forecast, NOT the week forecast received
      mutate(order_week = order_week() + as.numeric(gsub(" wk ahead", "", target)),
             value = as.numeric(value)) %>%
      select(-target) %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = order_week(),
                       # model = factor(unique(forecasts_1()$model)),
                       value = last(current_truth_location()$ILI)))
  })
  
  
  bounds <- reactive({
    filter(forecasts_1(), order_week == order_week()) %>%
      # Select appropriate CI bounds based on user input
      when(input$pred_int == "80%" ~ 
             select(., model, target, order_week, lower = low_80, upper = high_80),
           input$pred_int == "50%" ~ 
             select(., model, target, order_week, lower = low_50, upper = high_50),
           input$pred_int == "None" ~
             select(., model, target, order_week, lower = value) %>%
                mutate(upper = lower)) %>%
      # Note week is the week BEING forecast, NOT the week forecast received
      mutate(order_week = order_week() + as.numeric(gsub(" wk ahead", "", target)),
             lower = as.numeric(lower),
             upper = as.numeric(upper)) %>%
      # Attach forecast bins to point prediction of last observed value
      bind_rows(tibble(order_week = order_week(),
                       # model = factor(unique(forecasts_1()$model)),
                       lower = last(current_truth_location()$ILI),
                       upper = last(current_truth_location()$ILI)))
    
  })
  
  output$time_plot <- renderPlot({
    
    ggplot(point_forecasts()) +
      # Add shading for prediction interval
      geom_ribbon(data = bounds(), aes(x = order_week, ymin = lower, ymax = upper),
                  alpha = 0.3, fill = "tomato") +
      # Add full season observed values
      geom_line(data = final_truth(), aes(order_week, ILI), color = "darkgray", alpha = 1) +
      # Add observed values
      geom_line(data = current_truth_location(), aes(order_week, ILI), color = "black") +
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
  
  ##### Create leaflet map #####
  
  # Join appropriate ILI measures onto shapefiles
  map_data <- reactive({
    if (input$res == "nat") {
      
      left_join(
        nat_shape,
        filter(current_truth(), order_week == order_week()) %>%
          mutate(print_ILI = ILI,
                 ILI = ifelse(ILI > 10, 10, ILI)) %>%
          select(location, print_ILI, ILI),
        by = c('name' = 'location')
      )
      
    } else if (input$res == "reg") {
      
      left_join(
        reg_shapes,
        filter(current_truth(), order_week == order_week()) %>%
          mutate(print_ILI = ILI,
                 ILI = ifelse(ILI > 10, 10, ILI)) %>%
          select(location, print_ILI, ILI),
        by = c('name' = 'location')
      )
      
    } else if(input$res == "state") {
      
      left_join(
        state_shapes,
        filter(current_truth(), order_week == order_week()) %>%
          mutate(print_ILI = ILI,
                 ILI = ifelse(ILI > 10, 10, ILI)) %>%
          select(location, print_ILI, ILI),
        by = c('name' = 'location')
      )
      
    }
    
  })
  
  # Set palette options
  palData <- 0:10
  pal <- colorNumeric("YlOrRd", palData)
  
  output$mapPlot <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
      addLegend('bottomleft', pal = pal, values = palData)
  })
  
  observe({
    
    leafletProxy("mapPlot", data = map_data()) %>%
      clearShapes() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(ILI),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~paste0("<b>", name, "</b><br>",  round(print_ILI, 3))) 
  })
  
  ##### Scores #####
  # Create data frame for score output to display
  scores <- reactive({
    filter(all_scores, is.null(input$scoreLocation) | location %in% input$scoreLocation,
           is.null(input$scoreSeason) | season %in% input$scoreSeason, 
           is.null(input$scoreModel) | model %in% input$scoreModel) %>%
      rename(Location = location, Season = season, Model = model, Target = target) %>%
      when(input$groupTarget ~ group_by(., Location, Season, Model, Target),
           TRUE ~ group_by(., Location, Season, Model)) %>%
      summarize(MAE = round(mean(mae_score), 2),
                `Geom. Mean Prob.` = round(exp(mean(log_score)), 2)) 
  })
  
  output$scoreTable <- renderDataTable({
    scores()
  })
  
  
  
}