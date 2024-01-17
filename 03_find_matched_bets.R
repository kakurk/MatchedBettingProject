# find matched betting opportunities

# requirements ------------------------------------------------------------

source('american_to_decimal.R')
source('decimal_to_american.R')
source('optimal_realmoney.R')
source('optimal_bonus.R')

# parameters --------------------------------------------------------------

# how much (in $) to bet on the principal ("bookie") bet?
bookiebet   <- 150

# what type of matched bet is this?
matched.bet.type <- 'bonus' # realmoney | bonus

# routine -----------------------------------------------------------------
# for each bet, find the best priced lay bet

# a tibble collecting all of the best lay bet options for each bet
lay_collection <- tibble()

# for each possible bet...
for(i in 1:nrow(odds_tbl)){
  
  # this bets...
  thisID        <- odds_tbl$id[[i]]
  thisTeams     <- c(odds_tbl$home_team[[i]], odds_tbl$away_team[[i]])
  thisBetTeam   <- odds_tbl$name[[i]]
  thisOtherTeam <- thisTeams[!thisTeams %in% thisBetTeam]
  
  # all possible laybets
  odds_tbl %>%
    filter(id == thisID) %>%
    filter(name == thisOtherTeam) -> allLayBets
  
  # the best priced laybet
  allLayBets %>%
    slice_max(price, with_ties = FALSE) -> bestLayBet
  
  # what is the optimal laybet size?
  bookie_odds <- odds_tbl$price[[i]]
  laybet_odds <- bestLayBet$price
  
  # calculate the optimal amount to bet on the laybet along with the expected profit,
  # given the odds offered by the bookie, the best laybet odds, and the amount desired to
  # bet with the bookie.
  if(matched.bet.type == "realmoney"){
    output      <- optimal.realmoney(bookie_odds, laybet_odds, bookiebet)
  } else if(matched.bet.type == "bonus"){
    output      <- optimal.bonus(bookie_odds, laybet_odds, bookiebet)
  } else {
    simpleError("Incorrect Matched Bet Type. Must be realmoney | bonus", call = NULL)
  }

  # add bookiebet to the output
  output %>%
    add_column(bookiebet) -> output
  
  # tack information to the end of the best laybet; tidy up a bit
  bind_cols(bestLayBet, output) %>%
    select(key, title, name, price, bookiebet, laybet_size, bet_profit) -> bestLayBet
  
  # add best laybet to the collection
  if(nrow(lay_collection) == 0){
    lay_collection <- bestLayBet
  } else {
    lay_collection <- bind_rows(lay_collection, bestLayBet) 
  }
  
}

# tidy up the lay_collection
lay_collection %>%
  rename(best_lay_key = key,
         best_lay_title = title,
         best_lay_name = name,
         best_lay_price = price) -> lay_collection

# add lay collection to the end of the odds_tbl;
# rearrange putting the most profitable matched bets
# at the top
bind_cols(odds_tbl, lay_collection) %>%
  arrange(desc(bet_profit)) -> finalTbl

# tidy up the finalTbl
finalTbl %>%
  mutate(price.american = map_dbl(price, decimal_to_american),
         best_lay_price.american = map_dbl(best_lay_price, decimal_to_american)) %>%
  mutate(bet_profit.percentage = bet_profit/bookiebet * 100) %>%
  mutate(across(.cols = all_of(c('bet_profit', 'bet_profit.percentage', 'laybet_size', 'price.american', 'best_lay_price.american')), round, digits = 2)) -> finalTbl

view(finalTbl)
