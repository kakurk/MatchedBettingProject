# request multiple sports

source('01_request_odds.R')
source('02_tidy.R')
source('03_find_matched_bets.R')
source('03_find_matched_bets_withties.R')

# request_odds ------------------------------------------------------------

request_odds('basketball_nba', 'h2h') -> odds_tbl.nba
request_odds('americanfootball_nfl', 'h2h') -> odds_tbl.nfl
request_odds('soccer_epl', 'h2h') -> odds_tbl.epl

# nba ---------------------------------------------------------------------

tidy(odds_tbl.nba) -> odds_tbl
find_matched_bets(odds_tbl, 25, 'bonus') %>%
  mutate(title = factor(title)) -> finalTbl.nba

# nfl ---------------------------------------------------------------------

tidy(odds_tbl.nfl) -> odds_tbl
find_matched_bets(odds_tbl, 25, 'bonus') %>%
  mutate(title = factor(title)) -> finalTbl.nfl

# epl soccer --------------------------------------------------------------

tidy(odds_tbl.epl) -> odds_tbl
find_matched_bets_withties(odds_tbl, 25, 'bonus') %>%
  mutate(title = factor(title)) -> finalTbl.epl

# view --------------------------------------------------------------------

View(finalTbl.nba)
View(finalTbl.nfl)
View(finalTbl.epl)
