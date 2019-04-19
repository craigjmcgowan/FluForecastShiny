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
rolling_ili <- readRDS("Data/observed_ili.Rds")
final_ili <- readRDS("Data/final_ili.Rds")

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
