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
  filter(state_abbr %in% state.abb) %>%
  mutate(region = state_region_mapper(name))

reg_shapes <- state_shapes %>%
  group_by(region) %>%
  summarise() %>%
  rename(name = region)

nat_shape <- state_shapes %>%
  mutate(country = "US National") %>%
  group_by(country) %>%
  summarise() %>%
  rename(name = country)

# Load scores -----
nat_reg_scores <- readRDS("Data/nat_reg_scores.Rds")




