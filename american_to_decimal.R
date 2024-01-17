american_to_decimal <- function(american_odds){
  # convert american odds to decimal odds
  if(american_odds > 0) {
    (american_odds / 100) + 1
  } else {
    (-100 / american_odds) + 1  
  }
}