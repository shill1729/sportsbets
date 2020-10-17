#' Convert team name to team endpoint for ESPN url
#'
#' @param team_name team name e.g. "Arizona Cardinals" etc.
#'
#' @description {Backend function for converting team name from string to url endpoint.}
#' @return string
#' @export team_endpoint
team_endpoint <- function(team_name)
{
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
             "Kansas City Chiefs", #kc 15
             "Las Vegas Raiders", #lv 16
             "Los Angeles Chargers", #lac 17
             "Los Angeles Rams", #lar 18
             "Miami Dolphins", #mia
             "Minnesota Vikings", #min
             "New England Patriots", #ne 21
             "New Orleans Saints", #no 22
             "New York Giants", #nyg 23
             "New York Jets", #nyj 24
             "Philadelphia Eagles", #phi
             "Pittsburgh Steelers", #pit
             "San Francisco 49ERS", #sf 27
             "Seattle Seahawks", #sea
             "Tampa Bay Buccaneers", #tb 29
             "Tennessee Titans", #ten
             "Washington" #wsh 31
  )
  team_index <- which(teams == team_name)
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(12, 15,16, 17, 18, 21, 22, 23, 24, 27, 29, 31)] <- c("gb", "kc", "lv", "lac", "lar",
                                                             "ne", "no", "nyg", "nyj", "sf", "tb", "wsh")
  return(first3[team_index])
}
