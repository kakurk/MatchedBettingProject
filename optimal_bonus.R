optimal.bonus <- function(bookie_odds, laybet_odds, bookiebet){
  # estimate optimal bonus bet wager.
  # in other words, at what laybet would the net profit be the same for the
  # laybet and the bookie bet?
  # The difference between a real money bet and a bonus bet is that the
  # bonus bet stake is essentially free

# functions ---------------------------------------------------------------

  bookiebet_profit <- function(lay_wager, lay_odds, bookie_wager, bookie_odds){
    # what would the profit be if the bookiebet wins?
    (bookie_wager*bookie_odds)-bookie_wager-lay_wager
  }
  
  laybet_profit <- function(lay_wager, lay_odds, bookie_wager, bookie_odds){
    # what would the profit be if the laybet wins?
    (lay_wager*lay_odds)-lay_wager
  }
  
  bookiebet_minus_laybet <- function(lay_wager, lay_odds, bookie_wager, bookie_odds){
    bookiebet_profit(lay_wager, lay_odds, bookie_wager, bookie_odds) - laybet_profit(lay_wager, lay_odds, bookie_wager, bookie_odds)
  }
  
# routine -----------------------------------------------------------------
  
  laybet_size_root <- uniroot(bookiebet_minus_laybet, 
                              lower        = 0, 
                              upper        = 100, 
                              extendInt    = 'yes', 
                              lay_odds     = laybet_odds, 
                              bookie_wager = bookiebet, 
                              bookie_odds  = bookie_odds)
  laybet_size      <- laybet_size_root$root
  bet_profit       <- bookiebet_profit(lay_wager    = laybet_size, 
                                       lay_odds     = laybet_odds, 
                                       bookie_wager = bookiebet, 
                                       bookie_odds  = bookie_odds)

  return(tibble(laybet_size, bet_profit))
  
}