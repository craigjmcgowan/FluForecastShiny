library(dplyr)
library(purrr)
library(FluSight)
library(stringr)

# Read in forecasts from raw CSVs and save as smaller RDS files for use in Shiny app
read_forecasts <- function(dir, challenge = 'ilinet', these_weeks = c(),
                           these_teams = c()) {
  
  teams <- list.dirs(path = dir, recursive = F)
  
  # Only read forecasts for certain teams if called
  if(!is.null(these_teams))
    teams <- teams[str_extract(teams, "[^/]+$") %in% these_teams]
  
  subs <- list()
  
  for (this_team in teams) {
    
    # Extract team name from folder
    team_name <- str_extract(this_team, "[^/]+$")
    # Vector of submission files
    files <- list.files(path = this_team, pattern = "*.csv")
    # Only keep submission files from weeks specified
    if (!is.null(these_weeks)) {
      files <- files[str_extract(files, "EW[0-9]{2}") %in% these_weeks]
    }
    
    for (this_file in files) {
      
      week <- str_extract(this_file, "EW[0-9]{1,2}")
      
      tryCatch(
        {
          subs[[team_name]][[week]] <- read_entry(paste0(this_team,"/", this_file))
        },
        error = function(cond) {
          message(paste("Errors reading in submission for", team_name))
          message(cond)
          return(NA)
        },
        warning = function(cond) {
          message(paste("Warnings reading in submission for", team_name))
          message(cond)
          return(NA)
        },
        finally = message(paste("Submission read in for", team_name))
      )
      
      tryCatch(
        {
          verify_entry(subs[[team_name]][[week]], challenge = challenge)
        },
        error = function(cond) {
          message(paste("Errors in verifying submission for", team_name))
          message(cond)
          return(NA)
        },
        warning = function(cond) {
          message(paste("Warnings in verifying submission for", team_name))
          message(cond)
          return(NA)
        },
        finally = message(paste(week, "submission processed for", team_name))
      )
      
      # Remove invalid probabilities and normalize other probabilities.
      tryCatch(
        {
          subs[[team_name]][[week]] <- 
            remove_invalid(subs[[team_name]][[week]])
          
          subs[[team_name]][[week]] <- 
            normalize_probs(subs[[team_name]][[week]], ignore_invalid = TRUE)
        },
        error = function(cond) {
          message(paste("Errors in normalizing submission for", team_name))
          message(cond)
          return(NA)
        },
        warning = function(cond) {
          message(paste("Warnings in normalizing submission for", team_name))
          message(cond)
          return(NA)
        },
        finally = message(paste(week, "Probabilities normalized for", team_name))
      )
    }
  }
  
  
  return(subs)
}

# Read in forecasts
forecasts_1415 <- read_forecasts("RawData/2014-2015/NatReg")
forecasts_1516 <- read_forecasts("RawData/2015-2016/NatReg")
forecasts_1617 <- read_forecasts("RawData/2016-2017/NatReg")
forecasts_1718 <- read_forecasts("RawData/2017-2018/NatReg")
forecasts_1819 <- read_forecasts("RawData/2018-2019/NatReg")

# Collapse forecasts into single data file
nat_reg_forecasts <- bind_rows(
  bind_rows(modify_depth(forecasts_1415, 1, bind_rows), .id = 'model'),
  bind_rows(modify_depth(forecasts_1516, 1, bind_rows), .id = 'model'),
  bind_rows(modify_depth(forecasts_1617, 1, bind_rows), .id = 'model'),
  bind_rows(modify_depth(forecasts_1718, 1, bind_rows), .id = 'model'),
  bind_rows(modify_depth(forecasts_1819, 1, bind_rows), .id = 'model')
) %>%
  select(-bin_end_notincl)

# Save RDS output for use in Shiny app
saveRDS(nat_reg_forecasts, file = "Data/nat_reg_forecasts.Rds")

