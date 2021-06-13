library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(RSQLite)
library(bigrquery)
library(DBI)


get_db <- function(endpoint = NULL, modifier = NULL){
  base <- "https://statsapi.web.nhl.com/api/v1"
  
  URL <- paste0(base, "/" , endpoint, "/" , modifier)
  temp_con <- GET(URL)
  temp_text <- content(temp_con, "text")
  temp_JSON <- fromJSON(temp_text, flatten = TRUE)
  return(tbl_df(temp_JSON))
}

df_teams <- get_db("teams")
df_franchise <- get_db("franchises")
df_people <- get_db('people', '8477474')
df_people
#df_team_roster <- get_db("teams?expand=team.roster")
df_temp <- get_db('teams', '?stats=statsSingleSeason&season=19801981')
df_temp
print(df_teams)
print(df_franchise)


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