library(dplyr)
library(purrr)
library(FluSight)

source("utils.R")

# Load in forecast data
all_forecasts <- readRDS("Data/nat_reg_forecasts.Rds")  #%>%
  # bind_rows(readRDS("Data/state_forecasts.Rds")) %>%


# Load in observed flu data
all_observed <- readRDS("Data/observed_ili.Rds")


# Vector of labels for the time-series plots
wk_label <- c("40", "42", "44", "46", "48", "50", "52", "2", "4", "6",
              "8", "10", "12", "14", "16", "18", "20", "22", "24")