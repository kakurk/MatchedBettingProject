calc.boosted.odds <- function(bookie.odds, boost){
  (bookie.odds*boost)+bookie.odds-boost
}