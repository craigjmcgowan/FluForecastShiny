library(dplyr)
library(purrr)
library(FluSight)
library(MMWRweek)
library(USAboundaries)

source("utils.R")

# Load in forecast data
all_forecasts <- readRDS("Data/nat_reg_forecasts.Rds")  #%>%
  # bind_rows(readRDS("Data/state_forecasts.Rds")) %>%


# Load in observed flu data
all_observed <- readRDS("Data/observed_ili.Rds")

# Current MMWR week
if (MMWRweek(Sys.Date())[[3]] == 1) {
  this_week <- MMWRweek(Sys.Date())[[2]] - 3
} else {
  this_week <- MMWRweek(Sys.Date())[[2]] - 2
}

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

# Load mapping data
state_shapes <- USAboundaries::us_states() 

state_shapes <- subset(state_shapes, state_shapes$state_abbr %in% state.abb)

recent_flu <- all_observed %>%
  filter(season == "2018-2019", week == 14, lag == 0, location %in% state.name) %>%
  select(location, ILI)

state_shapes2 <- left_join(state_shapes, recent_flu, by = c("name" = "location"))


