#' Endpoints for NHL teams on ESPN.com
#'
#' @param teamName the full team name, e.g. "Boston Bruins"
#'
#' @description {Returns the appropriate endpoint for a NHL team. Internal use only.}
#' @return string, the url endpoint for the team's statistics.
#' @export nhl_stats_endpoint
nhl_stats_endpoint <- function(teamName)
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


#' Obtain the season stats for NHL teams by team name
#'
#' @param teamName string of team name, in full, e.g. "Boston Bruins"
#'
#' @description {Pull the ESPN data for NHL team's statistics.}
#' @return data.frame of the team's statistics
#' @export nhl_stats
nhl_stats <- function(teamName)
{
  url <- nhl_stats_endpoint(teamName)
  resp <- httr::GET(url = url)
  resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")
  tbs <- xml2::xml_find_all(resp_cont, "//td")
  lis <- xml2::xml_find_all(resp_cont, "//li")
  # Last row from below.
  gp <- tbs[[length(tbs)-14]]
  pts <- tbs[[length(tbs)-13]]

  gp <- as.numeric(unlist(xml2::as_list(gp)))
  pts <- as.numeric(unlist(xml2::as_list(pts)))
  team_stats <- data.frame(gp = gp, goals = pts, ppg = pts/gp)
  return(team_stats)
}


