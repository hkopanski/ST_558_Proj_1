library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(RSQLite)
library(bigrquery)
library(DBI)

## Create data functions

get_db_stats <- function(endpoint = NULL, modifier = NULL){
  base <- "https://statsapi.web.nhl.com/api/v1"
  
  URL <- paste0(base, "/" , endpoint, modifier)
  temp_con <- GET(URL)
  temp_text <- content(temp_con, "text")
  temp_JSON <- fromJSON(temp_text, flatten = TRUE)
  return(as_tibble(temp_JSON))
}

get_db_records <- function(endpoint = NULL, modifier = NULL){
  base <- "https://records.nhl.com/site/api"
  
  URL <- paste0(base, "/" , endpoint, modifier)
  temp_con <- GET(URL)
  temp_text <- content(temp_con, "text")
  temp_JSON <- fromJSON(temp_text, flatten = TRUE)
  return(as_tibble(temp_JSON))
}

# This is to pull team specific information
get_records <- function(team = NULL, ID = NULL){
  df_franchise_det <- get_db_records('franchise-detail')
  df_franchise_det <- df_franchise_det$data %>% rowwise() %>%
    mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))
  if(is.null(team) & !is.null(ID)){
    temp_string <- paste0('franchise-season-records?cayenneExp=franchiseId=', as.character(ID))
    get_db_records(temp_string)}
  else if(!is.null(team)){
    team_alt = tail(strsplit(team, " ")[[1]], n = 1)
    temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team_alt)) %>% select(id)
    temp_string <- paste0('franchise-season-records?cayenneExp=franchiseId=', as.character(temp_ID))
    get_db_records(temp_string)
  }
}

# This pulls goalie information
get_goalie_data <- function(team = NULL, ID = NULL){
  df_franchise_det <- get_db_records('franchise-detail')
  df_franchise_det <- df_franchise_det$data %>% rowwise() %>%
    mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))
  if(is.null(team) & !is.null(ID)){
    temp_string <- paste0('franchise-goalie-records?cayenneExp=franchiseId=', as.character(ID))
    get_db_records(temp_string)}
  else if(!is.null(team)){
    team_alt = tail(strsplit(team, " ")[[1]], n = 1)
    temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team_alt)) %>% select(id)
    temp_string <- paste0('franchise-goalie-records?cayenneExp=franchiseId=', as.character(temp_ID))
    get_db_records(temp_string)
  }
}

# This pulls skater information
get_skater_data <- function(team = NULL, ID = NULL){
  df_franchise_det <- get_db_records('franchise-detail')
  df_franchise_det <- df_franchise_det$data %>% rowwise() %>%
    mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))
  if(is.null(team) & !is.null(ID)){
    temp_string <- paste0('franchise-skater-records?cayenneExp=franchiseId=', as.character(ID))
    get_db_records(temp_string)}
  else if(!is.null(team)){
    team_alt = tail(strsplit(team, " ")[[1]], n = 1)
    temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team_alt)) %>% select(id)
    temp_string <- paste0('franchise-skater-records?cayenneExp=franchiseId=', as.character(temp_ID))
    get_db_records(temp_string)
  }
}

# This pulls information for a specific team
get_team_stats <- function(team = NULL, ID = NULL){
  df_franchise_det <- get_db_records('franchise-detail')
  df_franchise_det <- df_franchise_det$data %>% rowwise() %>%
    mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))
  
  if(is.null(team) & !is.null(ID)){
    temp_string <- paste0('teams/', as.character(ID))
    get_db_stats(temp_string, '?expand=team.stats')}
  
  else if(!is.null(team)){
    team_alt = tail(strsplit(team, " ")[[1]], n = 1)
    temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team_alt)) %>% select(id)
    temp_string <- paste0('teams/', as.character(temp_ID))
    get_db_stats(temp_string, '?expand=team.stats')}
}

get_team_stats(ID = 16)

#####################################################################################
get_team_stats2 <- function(team = NULL){
  df_franchise_det <- get_db_records('franchise-detail')
  df_franchise_det <- df_franchise_det$data %>% rowwise() %>%
    mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))
  
  if(is.numeric(team)){
    temp_string <- paste0('teams/', as.character(team))
    return(get_db_stats(temp_string, '?expand=team.stats'))}
  
  else if(is.character(team)){
    team_alt = tail(strsplit(team, " ")[[1]], n = 1)
    temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team_alt)) %>% select(id)
    temp_string <- paste0('teams/', as.character(temp_ID))
    temp_df <- get_db_stats(temp_string, '?expand=team.stats')
    temp_df <- temp_df$teams$teamStats[[1]]
    temp_df <- temp_df$splits[[1]]
    return(temp_df)}
  
  else {
    return(get_db_stats('teams'))
  }
}

#This will pull in all the teams
get_team_stats2()

# Exploratory analysis of the data sets
#########################################################################################
#(Returns id, firstSeasonId and lastSeasonId and name of every team in the history of the NHL)
df_franchise <- get_db_records('franchise') 

#(Returns Total stats for every franchise (ex roadTies, roadWins, etc))
df_fran_team_tot <- get_db_records('franchise-team-totals') 

#(Drill-down into season records for a specific franchise)
df_records <- get_db_records('franchise-season-records', '?cayenneExp=franchiseId=16') 

#(Goalie records for the specified franchise)
df_goalie_bruins <- get_db_records('franchise-goalie-records?cayenneExp=franchiseId=6')

#(Skater records, same interaction as goalie endpoint)
df_skater_bruins <- get_db_records('franchise-skater-records?cayenneExp=franchiseId=6')

# (Franchise details, websites, etc)
df_franchise_det <- get_db_records('franchise-detail')

# Team stats
df_teams <- get_team_stats2()

# Bruin stats
df_bruins <- get_team_stats2('bruins')

# Column names of each dataset
names(df_franchise$data)
names(df_fran_team_tot$data)
names(df_franchise_det$data)
names(df_records$data) #Data for the Flyers
names(df_goalie_bruins$data) #goalie data for the Bruins :(
names(df_skater_bruins$data) #skater data for the Bruins
names(df_teams$teams)
names(df_bruins$teams$teamStats)

#########################################################################################
# There are 32 teams, but one is a new expansion team and not active. 
df_teams$teams %>% select(id, name) %>% nrow()
df_teams$teams %>% filter(active == TRUE) %>% select(id, name) %>% nrow()

df_teams$teams %>% filter(active == TRUE) %>% 
  ggplot(. ,aes(conference.name, fill = as.factor(division.id))) + geom_bar() + 
  labs(x = "Conference ID", y = "Count") + 
  scale_fill_discrete(name = "Divisions", 
                      labels = c("MassMutual East", 
                                 "Discover Central",
                                 "Honda West",
                                 "Scotia North"))

df_teams$teams %>% filter(active == TRUE) %>% 
  ggplot(. ,aes(firstYearOfPlay)) + geom_bar(aes(fill = conference.name)) + 
  labs(x = "First Year of Play", y = "Count") +
  scale_fill_discrete(name = "Conference")

a <- df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, fullName)

b <- df_fran_team_tot$data %>% filter(is.na(lastSeasonId) & gameTypeId == 2) %>% select(teamName)

# c is the extra team that is not in the franchise data
c <- a$fullName[!(a$fullName %in% b$teamName)]

# Some data visualizations from combining stats and franchise totals
df_fil_fran_tot <- df_fran_team_tot$data %>% filter(is.na(lastSeasonId) & gameTypeId == 2) %>% 
  rename(abbreviation = triCode)

df_fil_team <- df_teams$teams %>% filter(active == TRUE)

df_fil_fran_tot$abbreviation %in% df_fil_team$abbreviation

df_com <- df_fil_team %>% inner_join(., df_fil_fran_tot, by = 'abbreviation')

df_com %>% ggplot(., aes(x = division.name, y = wins)) + geom_boxplot(fill = "gray") +
  labs(title = "Distribution of Wins Betwen the Conferences", 
       x = "Conference Name", y = "Number of Wins") + facet_grid(cols = vars(conference.name)) 

df_com %>% ggplot(., aes(x = abbreviation, y = pointPctg)) + geom_point() +
  labs(title = "Percentage Points Versus NHL Team", x = "NHL Teams (abbr)", y = "Point Pctg")

df_com %>% ggplot(., aes(x = division.name, y = wins)) + geom_boxplot()

df_com %>% mutate(win_per_tot = wins / gamesPlayed, win_per_home = homeWins / wins) %>% 
  ggplot(., aes(x = win_per_tot, y = win_per_home)) + geom_point()

#########################################################################################
# Quick Test of skater and goalie functions
df_temp_goalie <- get_goalie_data("Maple Leafs")
df_temp_skater <- get_skater_data("Maple Leafs")
df_temp <- get_records("Maple Leafs")
df_temp$data %>% select(id, franchiseName, lossStreak, winStreak, mostGoals, fewestGoals) %>% knitr::kable()

df_temp_goalie <- get_goalie_data("Maple Leafs")
df_temp_skater <- get_skater_data("Maple Leafs")

#names(df_temp_goalie$data)
#names(df_temp_skater$data)

df_temp_goalie$data %>% select(firstName, lastName, mostSavesOneGame, gamesPlayed) %>% 
  arrange(desc(gamesPlayed)) %>% head() %>% knitr::kable()

df_temp_skater$data %>% select(positionCode, mostGoalsOneGame) %>% table() %>% knitr::kable()

######################################################################################
rmarkdown::render(df_bruins, output_file = 'df_bruins.md')


######################################################################################

grab_all <- function(all = FALSE, team = NULL, ID = NULL, goalie = FALSE, skater = FALSE){
  if(all == TRUE){
    df_franchise <- get_db_records('franchise') 
    df_fran_team_tot <- get_db_records('franchise-team-totals')
  } 
 
  if(!is.null(team)){
    df_team_data <- get_records(team)
    df_team_records <- 
    df_team_stats <- get_team_stats2(team)
  }
  if(goalie == TRUE){
    if_temp_goalie <- get_goalie_data(team)
  }
  if(skater == TRUE){
    df_temp_skater <- get_skater_data(team)
  }
  
  df_list <- list(df_franchise, df_fran_team_tot, df_temp_goalie, df_temp_skater, df_team_data, df_team_stats)
  
  return(df_list)
}

df_test <- grab_all(all = TRUE, team = 'stars')
#########################################################################################

# https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=16

db_records_flyers <- get_db_records('franchise-season-records', '?cayenneExp=franchiseId=16')