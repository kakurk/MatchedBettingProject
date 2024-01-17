decimal_to_american <- function(decimal){
  # convert decimal odds to american odds
  if(decimal >= 2){
    (decimal - 1) * 100 
  } else {
    -100 / (decimal - 1)
  }
}