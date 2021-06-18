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

#########################################################################################

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

########################################################################################
df_teams <- get_db_stats('teams')

df_teams$teams %>% filter(active == TRUE) %>% ggplot(. ,aes(conference.id)) + geom_bar()
df_teams$teams %>% filter(active == TRUE) %>% ggplot(. ,aes(firstYearOfPlay)) + geom_bar()
df_teams$teams %>% ggplot(. ,aes(firstYearOfPlay, fill = active)) + geom_bar()
df_teams$teams %>% filter(active == TRUE) %>% select(id, name) %>% nrow()


df_franchise2 <- get_db_stats('franchises')

df_teams %>% inner_join(df_franchise, by = abbreviation)

df_goalie_briuns$data %>% select(lastName, wins, ties, shutouts) %>% filter(lastName == 'Casey')

ggplot(data = df_goalie_briuns) + geom_bar(aes(x = data$wins))

# This is the syntax for team stats and the modifier
df_stats <- get_db_stats('teams/16', '?expand=team.stats')

# This is to pull team specific information
df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, teamCommonName) %>% knitr::kable()
df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, teamCommonName) %>% nrow()

get_records <- function(team = NULL, ID = NULL){
  
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

df_temp <- get_records("Maple Leafs")

names(df_temp_goalie$data)
df_temp_goalie <- get_goalie_data("Maple Leafs")
df_temp_skater <- get_skater_data("Maple Leafs")

df_temp_goalie$data %>% select(firstName, lastName, mostSavesOneGame, gamesPlayed) %>% arrange(desc(gamesPlayed)) %>% print()

names(df_temp_skater$data)
df_temp_skater$data %>% select(positionCode, mostGoalsOneGame) %>% table()

print(df_temp$data[['franchiseName']])
print(df_temp)



a <- df_franchise$data %>% filter(is.na(lastSeasonId)) %>% select(id, fullName)

b <- df_fran_team_tot$data %>% filter(is.na(lastSeasonId) & gameTypeId == 2) %>% select(teamName)

a$fullName[!(a$fullName %in% b$fullName)]

df_fran_team_tot$data %>% ggplot(., aes(HomeWins)) + geom_histogram()

df_fran_team_tot$data %>% filter(activeFranchise == 1) %>% 
  ggplot(., aes(x = as.factor(gameTypeId), y = roadWins)) + geom_boxplot()

ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot() + geom_jitter(aes(colour = Species)) + 
  labs(title = "Boxplot for Sepal Length")

df_fil_fran_tot <- df_fran_team_tot$data %>% filter(is.na(lastSeasonId) & gameTypeId == 2) %>% 
  rename(abbreviation = triCode)

df_fil_team <- df_teams$teams %>% filter(active == TRUE)

df_fil_fran_tot$abbreviation %in% df_fil_team$abbreviation

df_com <- df_fil_team %>% inner_join(., df_fil_fran_tot, by = 'abbreviation')

df_fil_fran_tot$abbreviation
df_fil_team$abbreviation 

df_com %>% ggplot(., aes(x = conference.name, y = wins)) + geom_boxplot()
df_com %>% ggplot(., aes(x = division.name, y = wins)) + geom_boxplot()
df_com %>% ggplot(., aes(x = division.name, y = points)) + geom_boxplot()

df_com %>% mutate(win_per_tot = wins / gamesPlayed, win_per_home = homeWins / wins) %>% 
  ggplot(., aes(x = win_per_tot, y = win_per_home)) + geom_point()

df_com %>% table()

df_goalie_bruins <- get_db_records('franchise-goalie-records?cayenneExp=franchiseId=6')