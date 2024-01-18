find_matched_bets <- function(odds_tbl, bookie.stake, matched.bet.type, boost = NA){
# find matched betting opportunities

# requirements ------------------------------------------------------------

source('american_to_decimal.R')
source('decimal_to_american.R')
source('optimal_realmoney.R')
source('optimal_bonus.R')
source('calc.boosted.odds.R')

# routine -----------------------------------------------------------------
# for each bet, find the best priced lay bet

if(!is.na(boost)){
  odds_tbl %>%
    mutate(price.boosted = map(price.decimal, \(x) calc.boosted.odds(x, boost))) %>%
    add_column(boost = boost) -> odds_tbl
}

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
  if(is.na(boost)){
    bookie.odds <- odds_tbl$price.decimal[[i]]
  } else {
    bookie.odds <- odds_tbl$price.boosted[[i]] 
  }
  lay.odds    <- bestLayBet$price.decimal

  # calculate the optimal amount to bet on the laybet along with the expected profit,
  # given the odds offered by the bookie, the best laybet odds, and the amount desired to
  # bet with the bookie.

  if(matched.bet.type == "realmoney"){
    output      <- optimal.realmoney(bookie.stake, bookie.odds, lay.odds)
  } else if(matched.bet.type == "bonus"){
    output      <- optimal.bonus(bookie.stake, bookie.odds, lay.odds)
  }
  else {
    simpleError("Incorrect Matched Bet Type. Must be realmoney | bonus", call = NULL)
  }

  # add bookiebet to the output
  output %>%
    add_column(bookie.stake) -> output

  # tack information to the end of the best laybet; tidy up a bit
  bind_cols(bestLayBet, output) %>%
    select(key, title, name, price, bookie.stake, lay.stake, bookie.profit, lay.profit) -> bestLayBet
  
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
  arrange(desc(bookie.profit)) -> finalTbl

# tidy up the finalTbl
finalTbl %>%
  mutate(best_lay_price.decimal = map_dbl(best_lay_price, american_to_decimal)) %>%
  mutate(bookie.profit.percentage = bookie.profit/bookie.stake * 100) %>%
  mutate(across(.cols = all_of(c('bookie.profit','lay.profit', 'bookie.profit.percentage', 'lay.stake')), \(x) round(x, digits = 2))) %>%
  select(key, title, name, price, price.decimal, bookie.stake, bookie.profit, bookie.profit.percentage, best_lay_key, best_lay_title, best_lay_name, best_lay_price, best_lay_price.decimal,lay.stake, lay.profit, everything())-> finalTbl

return(finalTbl)

}