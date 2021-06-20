Readme
================

### ST 558 Project 1

[Link to Repo Pages](https://github.com/hkopanski/ST_558_Proj_1)

### Hockey data API

**Description**

Functions that connect to and pull data from NHL franchise records. The
functions output the data into tibbles with the expection of the
grab\_all function, which output mutiple tibbles in the form of a named
list. Data can then be used for various forms of analysis.

**Usage**

get\_db\_records(endpoint = NULL, modifier = NULL)
get\_db\_stats(endpoint = NULL, modifier = NULL) get\_goalie\_data(team
= NULL, ID = NULL) get\_skater\_data(team = NULL, ID = NULL)
get\_records(team = NULL, ID = NULL) get\_team\_stats2(team = NULL)
grab\_all(all = FALSE, team = NULL, ID = NULL, goalie = FALSE, skater =
FALSE)

**Arguments**

`endpoint` API endpoint, such as /record-detail or /franchise.

`modifier` Endpoint modifier, used to parse data from API. Examples are
?cayenneExp=gameTypeId=2 or ?cayenneExp=teamFranchiseId=ID.

`team` String to identify team for data pull. Examples are “Coyotes”,
“Boston Bruins”, or “Dallas Stars”. Not case sensitive. In the case of
get\_team\_stats2, team can also be the franchise ID.

`ID` Franchise ID number for particular team. Lookup table provided in
vignette.

`all` Argument to pull general NHL data.

`team` and `ID` Same as team and ID for previously described. Will pull
in team stats when using the grab\_all function.

`goalie` Argument to pull in team specific goalie data.

`skater` Argument to pull team specific skater data.

**EXAMPLES**

grab\_all(all = TRUE, team = ‘stars’)

grab\_all(team = ‘Boston Bruins’, goalie = TRUE)

grab\_all(ID = 15, skater = TRUE)
