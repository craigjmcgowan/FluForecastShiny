require(FluSight)
require(dplyr)

# Function to pull legend out of ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Functions to keep weeks in order and reset them --------
week_inorder <- function(week, season){
  case_when(as.numeric(week) < 40 & 
              season %in% c("1997-1998", "2003-2004", "2008-2009", "2014-2015") ~ 
              as.numeric(week) + 53,
            as.numeric(week) < 40 ~ as.numeric(week) + 52,
            TRUE ~ as.numeric(week))
}

week_reset <- function(week, season) {
  case_when(as.numeric(week) > 53 & 
              season %in% c("1997-1998", "2003-2004", "2008-2009", "2014-2015") ~ 
              as.numeric(week) - 53,
            as.numeric(week) > 52 & 
              !season %in% c("1997-1998", "2003-2004", "2008-2009", "2014-2015") ~
              as.numeric(week) - 52,
            TRUE ~ as.numeric(week))
}

# Functions to read forecasts from directory into a list --------
read_forecasts <- function(dir, challenge = 'ilinet', these_weeks = c(),
                           these_teams = c()) {
  require(stringr)
  
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

# State to region mapper
state_region_mapper <- function(state) {
  case_when(state %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts",
                         "Rhode Island", "Connecticut") ~ "HHS Region 1",
            state %in% c("New York", "New Jersey") ~ "HHS Region 2",
            state %in% c("Pennsylvania", "Maryland", "Delaware", "West Virginia",
                         "Virginia") ~ "HHS Region 3",
            state %in% c("Kentucky", "North Carolina", "South Carolina", "Tennessee",
                         "Mississippi", "Alabama", "Georgia", "Florida") ~ "HHS Region 4",
            state %in% c("Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin",
                         "Minnesota") ~ "HHS Region 5",
            state %in% c("Louisiana", "Arkansas", "Texas", "Oklahoma", 
                         "New Mexico") ~ "HHS Region 6",
            state %in% c("Missouri", "Iowa", "Kansas", "Nebraska") ~ "HHS Region 7",
            state %in% c("North Dakota", "South Dakota", "Montana", "Wyoming", 
                         "Colorado", "Utah") ~ "HHS Region 8",
            state %in% c("Arizona", "Nevada", "California", "Hawaii") ~ "HHS Region 9",
            state %in% c("Idaho", "Oregon", "Washington", "Alaska") ~ "HHS Region 10",
            TRUE ~ NA_character_)
}

# Function to match long and lat coordinates to a state name
state_revgeocode <- function(long, lat) {
  state_shapes_pl <- st_transform(state_shapes, 2163)  
  pnt_sf <- st_transform(st_sfc(st_point(c(long, lat)),crs = 4326), 2163)
  state_shapes_pl[which(st_intersects(pnt_sf, state_shapes_pl, sparse = FALSE)), ]$name 
}