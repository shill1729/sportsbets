#' Convert team name string to team endpoint for ESPN url
#'
#' @param teamName the full team name e.g. "Arizona Cardinals" etc.
#'
#' @description {Backend function for converting team name from string to url endpoint.}
#' @details {The statistics page url for an individual team on ESPN ends with a three letter string
#' resembling the city or team name of a given team. For example "Arizona Cardinals" has the endpoint
#' "ari". Exceptions include
#' \itemize{
#' \item Green Bay Packers is "gb"
#' \item JacksonVille Jaguars is "jax"
#' \item Kansas City Chiefs is "kc", etc.}}
#'
#' @return string
#' @export nfl_endpoints
nfl_endpoints <- function(teamName)
{
  base_url <- "https://www.espn.com/nfl/team/stats/_/type/team/name/"
  teams <- c("Arizona Cardinals", # ari
             "Atlanta Falcons", # atl
             "Baltimore Ravens", #bal
             "Buffalo Bills", #buf
             "Carolina Panthers", #car
             "Chicago Bears", #chi
             "Cincinnati Bengals", #cin
             "Cleveland Browns", #cle
             "Dallas Cowboys", #dal
             "Denver Broncos", #den
             "Detroit Lions", #det
             "Green Bay Packers", #gb 12
             "Houston Texans", #hou
             "Indianapolis Colts", #ind
             "Jacksonville Jaguars", #jax #15
             "Kansas City Chiefs", #kc 16
             "Las Vegas Raiders", #lv 17
             "Los Angeles Chargers", #lac 18
             "Los Angeles Rams", #lar 19
             "Miami Dolphins", #mia
             "Minnesota Vikings", #min
             "New England Patriots", #ne 22
             "New Orleans Saints", #no 23
             "New York Giants", #nyg 24
             "New York Jets", #nyj 25
             "Philadelphia Eagles", #phi
             "Pittsburgh Steelers", #pit
             "San Francisco 49ERS", #sf 28
             "Seattle Seahawks", #sea
             "Tampa Bay Buccaneers", #tb 30
             "Tennessee Titans", #ten
             "Washington Commanders" #wsh 32
  )
  teamIndex <- which(teams == teamName)
  if(length(teamIndex) == 0)
  {
    stop(paste("teamName", teamName, "not found in data-base"))
  }
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(12, 15, 16, 17, 18, 19, 22, 23, 24, 25, 28, 30, 32)] <- c("gb", "jax", "kc", "lv", "lac", "lar",
                                                                     "ne", "no", "nyg", "nyj", "sf", "tb", "wsh")
  return(paste(base_url, first3[teamIndex], sep = ""))
}


#' Convert team name string to team endpoint for ESPN url
#'
#' @param teamName the full team name e.g. "Boston Bruins" etc.
#'
#' @description {Backend function for converting team name from string to url endpoint.}
#' @details {The statistics page url for an individual team on ESPN ends with a three letter string
#' resembling the city or team name of a given team. For example "Arizona Cardinals" has the endpoint
#' "ari". Exceptions include
#' \itemize{
#' \item Green Bay Packers is "gb"
#' \item JacksonVille Jaguars is "jax"
#' \item Kansas City Chiefs is "kc", etc.}}
#'
#' @return string
#' @export nhl_endpoints
nhl_endpoints <- function(teamName)
{
  base_url <- "https://www.espn.com/nhl/team/stats/_/name/"
  teams <- c("Boston Bruins",
             "Buffalo Sabres",
             "Detroit Red Wings",
             "Florida Panthers", # 4 fla
             "Montreal Canadiens", # 5 mtl
             "Ottawa Senators",
             "Tampa Bay Lightning", # 7 tb
             "Toronto Maple Leafs",
             "Carolina Hurricanes",
             "Columbus Blue Jacket", # 10 cbj
             "New Jersey Devils", # 11 nj
             "New York Islanders", # 12 nyi
             "New York Rangers", # 13 nyr
             "Philadelphia Flyers",
             "Pittsburgh Penguins",
             "Washington Capitals", # 16 wsh
             "Arizona Coyotes",
             "Chicago Blackhawks",
             "Colorado Avalanche",
             "Dallas Stars",
             "Minnesota Wild",
             "Nashville Predators", # 22 nsh
             "St. Louis Blues", # 23 stl
             "Winnipeg Jets", # 24 wpg
             "Anaheim Ducks",
             "Calgary Flames", #cgy
             "Edmonton Oilers",
             "Los Angelos Kings", #la
             "San Jose Sharks", # sj
             "Seattle Kraken",
             "Vancouver Canucks",
             "Vegas Golden Knights" # vgk
  )
  teamIndex <- which(teams == teamName)
  if(length(teamIndex) == 0)
  {
    stop(paste("teamName", teamName, "not found in data-base"))
  }
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(4, 5, 7, 10, 11, 12, 13, 16, 22, 23, 24, 26, 28, 29, 32)] <- c("fla", "mtl", "tb", "cbj", "nj", "nyi", "nyr", "wsh",
                                                                          "nsh", "stl", "wpg", "cgy", "la", "sj", "vgk")
  endpoint <- tolower(gsub(" ", "-", teams))
  endpoints <- paste(first3, "/", endpoint, sep = "")
  return(paste(base_url, endpoints[teamIndex], sep = ""))
}


#' Convert team name string to team endpoint for ESPN url
#'
#' @param teamName the full team name e.g. "Boston Bruins" etc.
#'
#' @description {Backend function for converting team name from string to url endpoint.}
#' @details {The statistics page url for an individual team on ESPN ends with a three letter string
#' resembling the city or team name of a given team. For example "Arizona Cardinals" has the endpoint
#' "ari". Exceptions include
#' \itemize{
#' \item Green Bay Packers is "gb"
#' \item JacksonVille Jaguars is "jax"
#' \item Kansas City Chiefs is "kc", etc.}}
#'
#' @return string
#' @export nba_endpoints
nba_endpoints <- function(teamName)
{
  base_url <- "https://www.espn.com/nba/team/stats/_/name/"
  teams <- c("Atlanta Hawks",
             "Boston Celtics",
             "Brooklyn Nets", # 3 bkn
             "Charlotte Hornets",
             "Chicago Bulls",
             "Cleveland Cavaliers",
             "Dallas Mavericks",
             "Denver Nuggets",
             "Detroit Pistons",
             "Golden State Warriors", # 10 gs
             "Houston Rockets",
             "Indiana Pacers",
             "LA Clippers", # 13 lac
             "Los Angeles Lakers", # 14 lal
             "Memphis Grizzlies",
             "Miami Heat",
             "Milwaukee Bucks",
             "Minnesota Timberwolves",
             "New Orleans Pelicans", # 19 no
             "New York Knicks", # 20 ny
             "Oklahoma City Thunder", # 21 okc
             "Orlando Magic",
             "Philadelphia 76ers",
             "Phoenix Suns", # 24 phx
             "Portland Trail Blazers",
             "Sacramento Kings",
             "San Antonio Spurs", # 27 sa
             "Toronto Raptors",
             "Utah Jazz", # 29 utah
             "Washington Wizards" #30 wsh

  )
  teamIndex <- which(teams == teamName)
  if(length(teamIndex) == 0)
  {
    stop(paste("teamName", teamName, "not found in data-base"))
  }
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(3, 10, 13, 14, 19, 20, 21, 24, 27, 29, 30)] <- c("bkn", "gs", "lac", "lal", "no", "ny", "okc", "phx",
                                                    "sa", "utah", "wsh")
  endpoint <- tolower(gsub(" ", "-", teams))
  endpoints <- paste(first3, "/", endpoint, sep = "")
  return(paste(base_url, endpoints[teamIndex], sep = ""))
}



