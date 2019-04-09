library(dplyr)
library(purrr)
library(FluSight)
library(MMWRweek)

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

