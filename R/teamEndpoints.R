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
#' @export teamEndpointNFL
teamEndpointNFL <- function(teamName)
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
             "Washington" #wsh 32
  )
  teamIndex <- which(teams == teamName)
  if(length(teamIndex) == 0)
  {
    stop(paste("teamName", teamName, "not found in data-base"))
  }
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(12, 15, 16, 17, 18, 19, 22, 23, 24, 25, 28, 30, 32)] <- c("gb", "jax", "kc", "lv", "lac", "lar",
                                                                     "ne", "no", "nyg", "nyj", "sf", "tb", "wsh")
  return(first3[teamIndex])
}



