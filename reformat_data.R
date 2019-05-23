library(tidyverse)
library(FluSight)
library(USAboundaries)
library(sf)

# Read in forecasts from raw CSVs and save as smaller RDS files for use in Shiny app
source("utils.R")

# Read in forecasts
forecasts_1415 <- read_forecasts("RawData/2014-2015/NatReg")
forecasts_1516 <- read_forecasts("RawData/2015-2016/NatReg")
forecasts_1617 <- read_forecasts("RawData/2016-2017/NatReg")
forecasts_1718 <- read_forecasts("RawData/2017-2018/NatReg")
forecasts_1819 <- read_forecasts("RawData/2018-2019/NatReg")

state_forecasts_1415 <- read_forecasts("RawData/2014-2015/States", challenge = 'state_ili')
state_forecasts_1516 <- read_forecasts("RawData/2015-2016/States", challenge = 'state_ili')
state_forecasts_1617 <- read_forecasts("RawData/2016-2017/States", challenge = 'state_ili')
state_forecasts_1718 <- read_forecasts("RawData/2017-2018/States", challenge = 'state_ili')
state_forecasts_1819 <- read_forecasts("RawData/2018-2019/States", challenge = 'state_ili')

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
  mutate(model = case_when(model == "ens-month-target-type-based-weights" ~ "Ensemble",
                           TRUE ~ model),
         model = factor(model))

state_forecasts <- bind_rows(
  bind_rows(modify_depth(state_forecasts_1415, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2014-2015"),
  bind_rows(modify_depth(state_forecasts_1516, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2015-2016"),
  bind_rows(modify_depth(state_forecasts_1617, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2016-2017"),
  bind_rows(modify_depth(state_forecasts_1718, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2017-2018"),
  bind_rows(modify_depth(state_forecasts_1819, 1, bind_rows), .id = 'model') %>%
    mutate(season = "2018-2019")
) %>%
  mutate(order_week = week_inorder(forecast_week, season)) %>%
  select(-bin_end_notincl, -unit) %>%
  filter(str_detect(target, "wk ahead")) %>%
  mutate(model = case_when(model == "ens-optimal-state" ~ "Ensemble",
                           TRUE ~ model),
         model = factor(model))

# Create dataset of point forecasts
point_forecasts <- bind_rows(
  filter(state_forecasts, type == "Point"),
  filter(nat_reg_forecasts, type == "Point")
 ) %>%
  select(-type, -bin_start_incl)

# Create dataset of bounds
bounds <- bind_rows(
  filter(state_forecasts, type == "Bin"),
  filter(nat_reg_forecasts, type == "Bin")
  ) %>%
  # Determine upper and lower bounds for each target
  group_by(season, location, model, target, order_week) %>%
  # Calculate cumulative probability for each bin
  mutate(cumprob = cumsum(value),
         low_50 = as.numeric(bin_start_incl[max(which(cumprob < 0.25))]),
         high_50 = as.numeric(bin_start_incl[min(which(cumprob > 0.75))]),
         low_80 = as.numeric(bin_start_incl[max(which(cumprob < 0.1))]),
         high_80 = as.numeric(bin_start_incl[min(which(cumprob > 0.9))])) %>%
  ungroup() %>%
  # Only keep one copy of each week's upper and lower bound
  select(season, location, model, target, order_week, low_50, high_50, low_80, high_80) %>%
  distinct() 

all_forecasts <- full_join(
  point_forecasts,
  bounds,
  by = c("season", "location", "model", "target", "order_week")
)

# Save RDS output for use in Shiny app
saveRDS(all_forecasts, file = "Data/all_forecasts.Rds")


# Observed flu data
load('../FluForecast/Data/ili.Rdata')
load('../FluForecast/Data/ili_state.Rdata')


# Collapse observed ILI into single data file
rolling_observed <- bind_rows(ili_init_pub_list) %>%
  mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 6, 9))) %>%
  filter(season %in% c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"),
         week %in% c(1:18, 40:53)) %>%
  select(season, location, week, year, release_date, epiweek, issue, lag, ILI) %>%
  mutate(order_week = week_inorder(week, season),
         issue_week = as.numeric(substr(issue, 5, 6)),
         release_date = as.Date(release_date)) %>%
  bind_rows(bind_rows(state_ili_init_pub_list) %>%
    unique() %>%
    mutate(season = paste0(substr(season, 1, 4), "-", substr(season, 6, 9)),
           pub_season = case_when(as.numeric(substr(issue, 5, 6)) >= 40 ~ 
                                    paste0(substr(issue, 1, 4), "-", as.numeric(substr(issue, 1, 4)) + 1),
                                  TRUE ~ paste0(as.numeric(substr(issue, 1, 4)) - 1, "-", substr(issue, 1, 4)))) %>%
    filter(season %in% c("2014-2015", "2015-2016", "2016-2017") |
             season == pub_season, week %in% c(1:18, 40:53)) %>%
    select(season, location, week, year, release_date, epiweek, issue, lag, ILI) %>%
    mutate(order_week = week_inorder(week, season),
           issue_week = as.numeric(substr(issue, 5, 6)))
  ) %>%
  arrange(season, location, order_week, issue_week)

# Final truth
final_observed <- mutate(ili_current, season = paste0(substr(season, 1, 4), "-", substr(season, 6, 9))) %>%
  filter(year > 2014 | season == "2014-2015") %>%
  select(season, location, week, ILI) %>%
  bind_rows(
    mutate(state_current, season = paste0(substr(season, 1, 4), "-", substr(season, 6, 9))) %>%
      filter(year > 2014 | season == "2014-2015") %>%
      select(season, location, week, ILI)
  ) %>%
  mutate(order_week = week_inorder(week, season))

# Save RDS output for use in Shiny
saveRDS(rolling_observed, file = "Data/rolling_ili.Rds")
saveRDS(final_observed, file = "Data/final_ili.Rds")


# Score forecasts
nat_reg_truth <- final_observed %>%
  filter(!location %in% state.name) %>%
  select(-order_week) %>%
  mutate(year = as.numeric(substr(season, 1, 4))) %>%
  nest(-season, -year) %>%
  mutate(truth = map2(data, year,
                      ~ create_truth(fluview = FALSE, year = .y, weekILI = .x,
                                     challenge = 'ilinet')),
         weeks_53 = (year == 2014),
         exp_truth = map2(truth, weeks_53,
                          ~ expand_truth(.x, week53 = weeks_53))) %>%
  select(-data, -weeks_53) 

state_truth <- final_observed %>%
  filter(location %in% state.name) %>%
  select(-order_week) %>%
  mutate(year = as.numeric(substr(season, 1, 4))) %>%
  nest(-season, -year) %>%
  mutate(truth = map2(data, year,
                      ~ create_truth(fluview = FALSE, year = .y, weekILI = .x,
                                     challenge = 'state_ili')),
         weeks_53 = (year == 2014),
         exp_truth = map2(truth, weeks_53,
                          ~ expand_truth(.x, week53 = weeks_53))) %>%
  select(-data, -weeks_53) 

nat_reg_scores <- full_join(
  # Log scores
  nat_reg_forecasts %>%
    nest(-season, -model, -order_week) %>%
    left_join(select(nat_reg_truth, season, exp_truth),
              by = "season") %>%
    mutate(score = map2(data, exp_truth,
                        ~ score_entry(.x, .y))) %>%
    select(-data, -exp_truth) %>%
    unnest() %>%
    rename(log_score = score) %>%
    filter(str_detect(target, "ahead")),
  # MAE score
  nat_reg_truth %>%
    select(season, truth) %>%
    unnest() %>%
    filter(str_detect(target, "ahead")) %>%
    left_join(point_forecasts,
              by = c("location", "season", "target", "forecast_week")) %>%
    mutate(mae_score = abs(value - as.numeric(bin_start_incl))) %>%
    select(-value, -bin_start_incl),
  by = c("model", "season", "order_week", "location", "target", "forecast_week")
)

state_scores <- full_join(
  # Log scores
  state_forecasts %>%
    nest(-season, -model, -order_week) %>%
    left_join(select(state_truth, season, exp_truth),
              by = "season") %>%
    mutate(score = map2(data, exp_truth,
                        ~ score_entry(.x, .y))) %>%
    select(-data, -exp_truth) %>%
    unnest() %>%
    rename(log_score = score) %>%
    filter(str_detect(target, "ahead")),
  # MAE score
  state_truth %>%
    select(season, truth) %>%
    unnest() %>%
    filter(str_detect(target, "ahead")) %>%
    inner_join(point_forecasts,
              by = c("location", "season", "target", "forecast_week")) %>%
    mutate(mae_score = abs(value - as.numeric(bin_start_incl))) %>%
    select(-value, -bin_start_incl),
  by = c("model", "season", "order_week", "location", "target", "forecast_week")
)

all_scores <- bind_rows(
  nat_reg_scores,
  state_scores
)

# Save scores
saveRDS(all_scores, file = "Data/all_scores.Rds")
