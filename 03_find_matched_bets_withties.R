find_matched_bets_withties <- function(odds_tbl, bookie.stake, matched.bet.type){
# find matched betting opportunities

# requirements ------------------------------------------------------------

source('american_to_decimal.R')
source('decimal_to_american.R')
source('optimal_realmoney.R')
source('optimal_bonus.R')

# routine -----------------------------------------------------------------
# for each bet, find the best priced lay bet

# a tibble collecting all of the best lay bet options for each bet
lay_collection <- tibble()

# for each possible bet...
for(i in 1:nrow(odds_tbl)){

  # this bets...
  thisID        <- odds_tbl$id[[i]]
  thisGameBets  <- c(odds_tbl$home_team[[i]], odds_tbl$away_team[[i]], 'Draw')
  thisBet       <- odds_tbl$name[[i]]
  thisOtherBets <- thisGameBets[!thisGameBets %in% thisBet]

  bestLayBet <- tibble()
  
  for(r in thisOtherBets){

    # all possible laybets
    odds_tbl %>%
      filter(id == thisID) %>%
      filter(name == r) -> allLayBets

    # the best priced laybet
    allLayBets %>%
      slice_max(price, with_ties = FALSE) -> thisBestLayBet

    # what is the optimal laybet size?
    bookie.odds <- odds_tbl$price.decimal[[i]]
    lay.odds <- thisBestLayBet$price.decimal

    # calculate the optimal amount to bet on the laybet along with the expected profit,
    # given the odds offered by the bookie, the best laybet odds, and the amount desired to
    # bet with the bookie.
    if(matched.bet.type == "realmoney"){
      output      <- optimal.realmoney(bookie.stake, bookie.odds, lay.odds)
    } else if(matched.bet.type == "bonus"){
      output      <- optimal.bonus(bookie.stake, bookie.odds, lay.odds)
    } else {
      simpleError("Incorrect Matched Bet Type. Must be realmoney | bonus", call = NULL)
    }

    # add bookiebet to the output
    output %>%
      add_column(bookie.stake, lay.odds) -> output

    # tack information to the end of the best laybet; tidy up a bit
    bind_cols(thisBestLayBet, output) %>%
      select(key, title, name, price, bookie.stake, lay.stake, lay.odds) -> thisBestLayBet

    if(nrow(bestLayBet) == 0){
      bestLayBet <- thisBestLayBet
    } else{
      bestLayBet <- left_join(bestLayBet, thisBestLayBet, by = 'bookie.stake', suffix = c('.lay1', '.lay2'))
    }

  }

  # add best laybet to the collection
  if(nrow(lay_collection) == 0){
    lay_collection <- bestLayBet
  } else {
    lay_collection <- bind_rows(lay_collection, bestLayBet) 
  }      
}

# add lay collection to the end of the odds_tbl;
# rearrange putting the most profitable matched bets
# at the top
calc.bookie.profit <- function(x, matched.bet.type){
  if(matched.bet.type == 'realmoney'){
    (x$bookie.stake * x$price.decimal) - x$bookie.stake - x$lay.stake.lay1 - x$lay.stake.lay2
  } else if(matched.bet.type == 'bonus'){
    (x$bookie.stake * x$price.decimal) - x$bookie.stake - x$lay.stake.lay1 - x$lay.stake.lay2
  }
}

calc.lay.profit.lay1 <- function(x, matched.bet.type){
  if(matched.bet.type == 'realmoney'){
    (x$lay.stake.lay1 * x$lay.odds.lay1) - x$bookie.stake - x$lay.stake.lay1 - x$lay.stake.lay2
  } else if(matched.bet.type == 'bonus'){
    (x$lay.stake.lay1 * x$lay.odds.lay1) - x$lay.stake.lay1 - x$lay.stake.lay2
  }
}

calc.lay.profit.lay2 <- function(x, matched.bet.type){
  if(matched.bet.type == 'realmoney'){
    (x$lay.stake.lay2 * x$lay.odds.lay2) - x$bookie.stake - x$lay.stake.lay1 - x$lay.stake.lay2
  } else if(matched.bet.type == 'bonus'){
    (x$lay.stake.lay2 * x$lay.odds.lay2) - x$lay.stake.lay1 - x$lay.stake.lay2
  }
}

bind_cols(odds_tbl, lay_collection) %>%
  nest(info = c(bookie.stake, price.decimal, lay.stake.lay1, lay.odds.lay1, lay.stake.lay2, lay.odds.lay2)) %>%
  mutate(bookie.profit = map_dbl(.x = info, .f = calc.bookie.profit, matched.bet.type = matched.bet.type),
         lay.profit.lay1 = map_dbl(.x = info, .f = calc.lay.profit.lay1, matched.bet.type = matched.bet.type),
         lay.profit.lay2 = map_dbl(.x = info, .f = calc.lay.profit.lay2, matched.bet.type = matched.bet.type)) %>%
  unnest(cols = info) %>%
  mutate(profit.percentage = bookie.profit / bookie.stake * 100) %>%
  select(key, title, name, price, price.decimal, bookie.stake, bookie.profit, profit.percentage,
         ends_with('.lay1'),
         ends_with('.lay2'), everything()) %>%
  arrange(desc(bookie.profit)) %>%
  mutate(across(all_of(c('bookie.profit', 'profit.percentage', 'lay.stake.lay1', 'lay.stake.lay2')), \(x) round(x, digits = 2))) -> finalTbl

return(finalTbl)

}