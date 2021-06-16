library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(RSQLite)
library(bigrquery)
library(DBI)


get_db_stats <- function(endpoint = NULL, modifier = NULL){
  base <- "https://statsapi.web.nhl.com/api/v1"
  
  URL <- paste0(base, "/" , endpoint, modifier)
  temp_con <- GET(URL)
  temp_text <- content(temp_con, "text")
  temp_JSON <- fromJSON(temp_text, flatten = TRUE)
  return(tbl_df(temp_JSON))
}

get_db_records <- function(endpoint = NULL, modifier = NULL){
  base <- "https://records.nhl.com/site/api"
  
  URL <- paste0(base, "/" , endpoint, modifier)
  temp_con <- GET(URL)
  temp_text <- content(temp_con, "text")
  temp_JSON <- fromJSON(temp_text, flatten = TRUE)
  return(tbl_df(temp_JSON))
}

df_franchise <- get_db_records('franchise')    #(Returns id, firstSeasonId and lastSeasonId and name of every team in the history of the NHL)
df_fran_team_tot <- get_db_records('franchise-team-totals') #(Returns Total stats for every franchise (ex roadTies, roadWins, etc))
df_records <- get_db_records('franchise-season-records?cayenneExp=franchiseId=16') #(Drill-down into season records for a specific franchise)
df_goalie_briuns <- get_db_records('franchise-goalie-records?cayenneExp=franchiseId=6') #(Goalie records for the specified franchise)
get_db_records('franchise-skater-records?cayenneExp=franchiseId=6') #(Skater records, same interaction as goalie endpoint)
df_franchise_det <- get_db_records('franchise-detail') #?cayenneExp=mostRecentTeamId=6') #(Admin history and retired numbers)

df_franchise_det <- df_franchise_det$data %>% rowwise() %>% 
  mutate(short_name = tail(strsplit(teamFullName, " ")[[1]], n = 1))

df_teams <- get_db_stats('teams')

df_franchise2 <- get_db_stats('franchises')

df_teams %>% inner_join(df_franchise, by = abbreviation)

df_goalie_briuns$data %>% select(lastName, wins, ties, shutouts) %>% filter(lastName == 'Casey')

ggplot(data = df_goalie_briuns) + geom_bar(aes(x = data$wins))

# This is the syntax for team stats and the modifier
df_stats <- get_db_stats('teams/16', '?expand=team.stats')



# This is to pull team specific information
get_records <- function(team = NULL, ID = NULL){
  team_list <- df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, teamCommonName)
  tryCatch(
  if(is.null(team) & !is.null(ID)){
    temp_string <- paste0('franchise-season-records?cayenneExp=franchiseId=', as.character(ID))
    get_db_records(temp_string)}
    else if(!is.null(team)){
      temp_ID <- df_franchise_det %>% filter(toupper(short_name) == toupper(team)) %>% select(id)
      temp_string <- paste0('franchise-season-records?cayenneExp=franchiseId=', as.character(temp_ID))
      get_db_records(temp_string)
    }, warning = print("Choose ID from list"), warning = print(knitr::kable(team_list)))
}

df_temp <- get_records("1000")
print(df_temp$data[['franchiseName']])

df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, teamCommonName) %>% print()