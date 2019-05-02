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

source("utils.R")

# Load in forecast data
all_forecasts <- readRDS("Data/all_forecasts.Rds")

# Load in observed flu data
rolling_ili <- readRDS("Data/rolling_ili.Rds")
final_ili <- readRDS("Data/final_ili.Rds")

# Current MMWR week
this_week <- tail(final_ili$week, 1)

# Mappings between states/regions/national
state_region <- tibble(
  state_name = state.name,
  region = c("HHS Region 4", "HHS Region 10", "HHS Region 9", "HHS Region 6",
             "HHS Region 9", "HHS Region 8", "HHS Region 1", "HHS Region 3",
             "HHS Region 4", "HHS Region 4", "HHS Region 9", "HHS Region 10",
             "HHS Region 5", "HHS Region 5", "HHS Region 7", "HHS Region 7",
             "HHS Region 4", "HHS Region 6", "HHS Region 1", "HHS Region 3",
             "HHS Region 1", "HHS Region 5", "HHS Region 5", "HHS Region 4",
             "HHS Region 7", "HHS Region 8", "HHS Region 7", "HHS Region 9",
             "HHS Region 1", "HHS Region 2", "HHS Region 6", "HHS Region 2",
             "HHS Region 4", "HHS Region 8", "HHS Region 5", "HHS Region 6",
             "HHS Region 10", "HHS Region 3", "HHS Region 1", "HHS Region 4",
             "HHS Region 8", "HHS Region 4", "HHS Region 6", "HHS Region 8",
             "HHS Region 1", "HHS Region 3", "HHS Region 10", "HHS Region 3",
             "HHS Region 5", "HHS Region 8"),
  nation = rep("US National", 50)
)

# Load mapping data -----
state_shapes <- USAboundaries::us_states()  %>%
  filter(state_abbr %in% state.abb)# %>%
  # left_join(recent_flu, by = c("name" = "location"))


# National shape file
nat_shape <- rgdal::readOGR("Data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp",
                            GDAL1_integer64_policy = TRUE)

nat_shape <- subset(nat_shape, nat_shape$NAME == "United States")

names(nat_shape) <- tolower(names(nat_shape))

# nat_shape$ILI <- recent_flu$ILI[recent_flu$location == "United States"]

# Create list of shape files to use in plot
# plot_shapes <- list("nat" = nat_shape,
#                     "reg" = nat_shape,
#                     "state" = state_shapes)


# Load scores -----
