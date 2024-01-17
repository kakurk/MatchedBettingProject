# request odds using the odds api https://the-odds-api.com/

# requirements ------------------------------------------------------------
# packages or external function this script relies on
library(httr2)

# parameters --------------------------------------------------------------
# see https://the-odds-api.com/liveapi/guides/v4/#get-odds for more 
# information on parameters

apiKey <- 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' # randomized key; obtain from odd-api website
sport  <- 'americanfootball_nfl' # basketball_nba | soccer_epl | americanfootball_nfl
region <- 'us' # uk | us | eu | au.
market <- 'h2h' # h2h | spreads | totals

# routine -----------------------------------------------------------------
# the main body of the script.

request("https://api.the-odds-api.com") -> req

req |>
  req_template("GET /v4/sports/{sport}/odds/?apiKey={apiKey}&regions={region}&markets={market}") %>%
  req_perform() -> resp

resp |> 
  resp_body_json(simplifyVector = TRUE) -> odds_tbl
