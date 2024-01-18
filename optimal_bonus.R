optimal.bonus <- function(bookie.stake, bookie.odds, lay.odds){
  # estimate optimal bonus bet wager.
  # in other words, at what laybet would the net profit be the same for the
  # laybet and the bookie bet?
  # The difference between a real money bet and a bonus bet is that the
  # bonus bet stake is essentially free
  
# routine -----------------------------------------------------------------

  # how much would be returned if the bookie bet were to win?
  bookie.return  <- bookie.stake * bookie.odds
  
  # which lay bet stake size would match this return?
  lay.stake      <- (bookie.return - bookie.stake)/lay.odds
  
  # what would the return be if the lay bet were to win?
  lay.return     <- lay.stake * lay.odds
  
  # what would be the profit for the lay bet? for the bookie bet?
  lay.profit    <- lay.return - lay.stake
  bookie.profit <- bookie.return - bookie.stake - lay.stake
  
  return(tibble(lay.stake, bookie.profit, lay.profit))
  
}