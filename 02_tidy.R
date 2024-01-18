tidy <- function(odds_tbl){
# tidy the odds_tbl returned from the odds api website.
# assumes you have already run 01_request_odds.R

# requirements ------------------------------------------------------------

library(tidyverse)
library(jsonlite)
source('american_to_decimal.R')

# parameters --------------------------------------------------------------

# list of legal NY sportsbooks
ny.sportsbooks <- c("BetMGM", "BetRivers", "Caesars", "DraftKings", "FanDuel", "PointsBet", "WynnBet")

# routine -----------------------------------------------------------------
# unnest the dataframe, filter only NY sports books

odds_tbl %>% 
  unnest(cols = c(bookmakers)) %>% 
  unnest(cols = c(markets), names_sep = '_') %>% 
  unnest(cols = c(markets_outcomes)) %>%
  mutate(price.decimal = map_dbl(.f = american_to_decimal, .x = price)) %>%
  filter(title %in% ny.sportsbooks) -> odds_tbl

return(odds_tbl)

}