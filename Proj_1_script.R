library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(RSQLite)
library(bigrquery)
library(DBI)


get_db_stats <- function(endpoint = NULL, modifier = NULL){
  base <- "https://statsapi.web.nhl.com/api/v1"
  
  URL <- paste0(base, "/" , endpoint, "/" , modifier)
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
get_db_records('franchise-season-records?cayenneExp=franchiseId=16') #(Drill-down into season records for a specific franchise)
get_db_records('franchise-goalie-records?cayenneExp=franchiseId=6') #(Goalie records for the specified franchise)
get_db_records('franchise-skater-records?cayenneExp=franchiseId=6') #(Skater records, same interaction as goalie endpoint)
df_franchise_det <- get_db_records('franchise-detail') #?cayenneExp=mostRecentTeamId=6') #(Admin history and retired numbers)

df_franchise2 <- get_db_stats('franchises')


as_tibble(fromJSON(content(GET('https://statsapi.web.nhl.com/api/v1/standings?season=20052006'), 'text'), flatten = TRUE))

as_tibble(fromJSON(content(GET('https://records.nhl.com/site/api/franchise-team-totals'), 'text'), flatten = TRUE))
as_tibble(fromJSON(content(GET('https://records.nhl.com/site/api/franchise-team-totals'), 'text'), flatten = TRUE))

as_tibble(fromJSON(content(GET('https://statsapi.web.nhl.com/api/v1/teams/?expand=team.roster'), 'text'), flatten = TRUE))

as_tibble(fromJSON(content(GET('https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=6'), 'text'), flatten = TRUE))

get_db('standings?season=20102011')

#?expand=team.stats modifier
#?stats=statsSingleSeason&season=19801981


base <- "https://statsapi.web.nhl.com/api/v1/"
endpoint <- "teams"
value_x <- ""

URL <- paste0(base, "/" , endpoint, "/" ,value_x)

temp_con <- GET(URL)
temp_text <- content(temp_con, "text")
temp_JSON <- fromJSON(temp_text, flatten = TRUE)
temp_tbl <- tbl_df(temp_JSON)
print(temp_tbl)

pikachu_text <- content(pikachu, "text")

pikachu_JSON <- fromJSON(pikachu_text, flatten = TRUE)

print(length(pikachu_JSON))

pikachu_names <- names(pikachu_JSON)

move_count <- nrow(pikachu_JSON$moves)

game_ver <- nrow(pikachu_JSON$game_indices)