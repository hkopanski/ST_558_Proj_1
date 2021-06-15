Project 1
================
Halid Kopanski
6/14/2021

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter()  masks stats::filter()
    ## x purrr::flatten() masks jsonlite::flatten()
    ## x dplyr::lag()     masks stats::lag()

## Create data functions

## Setup ID Data

    ## No encoding supplied: defaulting to UTF-8.

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## Please use `tibble::as_tibble()` instead.

    ## No encoding supplied: defaulting to UTF-8.
    ## No encoding supplied: defaulting to UTF-8.

## Display Data

``` r
df_franchise
```

    ## # A tibble: 39 x 2
    ##    data$id $firstSeasonId $fullName   $lastSeasonId $mostRecentTeam… $teamAbbrev
    ##      <int>          <int> <chr>               <int>            <int> <chr>      
    ##  1       1       19171918 Montréal C…            NA                8 MTL        
    ##  2       2       19171918 Montreal W…      19171918               41 MWN        
    ##  3       3       19171918 St. Louis …      19341935               45 SLE        
    ##  4       4       19191920 Hamilton T…      19241925               37 HAM        
    ##  5       5       19171918 Toronto Ma…            NA               10 TOR        
    ##  6       6       19241925 Boston Bru…            NA                6 BOS        
    ##  7       7       19241925 Montreal M…      19371938               43 MMR        
    ##  8       8       19251926 Brooklyn A…      19411942               51 BRK        
    ##  9       9       19251926 Philadelph…      19301931               39 QUA        
    ## 10      10       19261927 New York R…            NA                3 NYR        
    ## # … with 29 more rows, and 1 more variable: total <int>

``` r
df_teams
```

    ## # A tibble: 32 x 2
    ##    copyright        teams$id $name  $link  $abbreviation $teamName $locationName
    ##    <chr>               <int> <chr>  <chr>  <chr>         <chr>     <chr>        
    ##  1 NHL and the NHL…        1 New J… /api/… NJD           Devils    New Jersey   
    ##  2 NHL and the NHL…        2 New Y… /api/… NYI           Islanders New York     
    ##  3 NHL and the NHL…        3 New Y… /api/… NYR           Rangers   New York     
    ##  4 NHL and the NHL…        4 Phila… /api/… PHI           Flyers    Philadelphia 
    ##  5 NHL and the NHL…        5 Pitts… /api/… PIT           Penguins  Pittsburgh   
    ##  6 NHL and the NHL…        6 Bosto… /api/… BOS           Bruins    Boston       
    ##  7 NHL and the NHL…        7 Buffa… /api/… BUF           Sabres    Buffalo      
    ##  8 NHL and the NHL…        8 Montr… /api/… MTL           Canadiens Montréal     
    ##  9 NHL and the NHL…        9 Ottaw… /api/… OTT           Senators  Ottawa       
    ## 10 NHL and the NHL…       10 Toron… /api/… TOR           Maple Le… Toronto      
    ## # … with 22 more rows
