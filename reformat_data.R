library(dplyr)
library(purrr)
library(FluSight)
library(stringr)

# Read in forecasts from raw CSVs and save as smaller RDS files for use in Shiny app
source("utils.R")

# Read in forecasts
forecasts_1415 <- read_forecasts("RawData/2014-2015/NatReg")
forecasts_1516 <- read_forecasts("RawData/2015-2016/NatReg")
forecasts_1617 <- read_forecasts("RawData/2016-2017/NatReg")
forecasts_1718 <- read_forecasts("RawData/2017-2018/NatReg")
forecasts_1819 <- read_forecasts("RawData/2018-2019/NatReg")

# Collapse forecasts into single data file
nat_reg_forecasts <- bind_rows(
  bind_rows(modify_depth(forecasts_1415, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2014-2015"),
  bind_rows(modify_depth(forecasts_1516, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2015-2016"),
  bind_rows(modify_depth(forecasts_1617, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2016-2017"),
  bind_rows(modify_depth(forecasts_1718, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2017-2018"),
  bind_rows(modify_depth(forecasts_1819, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2018-2019")
) %>%
  mutate(order_week = week_inorder(forecast_week, season)) %>%
  select(-bin_end_notincl, -unit) %>%
  filter(str_detect(target, "wk ahead")) %>%
  mutate(model = factor(model))

# Save RDS output for use in Shiny app
saveRDS(nat_reg_forecasts, file = "Data/nat_reg_forecasts.Rds")


# Observed flu data
load('../FluForecast/Data/ili.Rdata')

# Collapse observed ILI into single data file
all_observed <- bind_rows(ili_init_pub_list) %>%
  mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 6, 9))) %>%
  filter(season %in% c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019")) %>%
  select(season, location, week, year, release_date, epiweek, issue, lag, ILI) %>%
  mutate(order_week = week_inorder(week, season))

# Save RDS output for use in Shiny
saveRDS(all_observed, file = "Data/observed_ili.Rds")
