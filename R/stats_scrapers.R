#' ESPN stats scraper
#'
#' @param team team name, the full name
#'
#' @description {Get mean number of touchdowns and (good) field goals per game for the
#' current season}
#' @return a named list containing
#' \itemize{
#' \item \code{scraped} a list containing the scraped data of
#' \code{record}, \code{total_tds}, and \code{fg_stats}
#' \item \code{means} }
#' @export nfl_stats
nfl_stats <- function(team)
{
  # url <- "https://www.espn.com/nfl/team/stats/_/type/team/name/"
  # url <- paste(url, team, sep = "")
  url <- nfl_endpoints(team)
  resp <- httr::GET(url = url)
  resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")
  tbs <- xml2::xml_find_all(resp_cont, "//td")
  lis <- xml2::xml_find_all(resp_cont, "//li")
  # The teams record
  record <- unlist(xml2::as_list(lis[15]))
  record <- as.numeric(unlist(strsplit(record, "-")))
  # Total points
  total_points <- as.numeric(unlist(xml2::as_list(tbs[length(tbs)-88])))
  # The teams number of tds
  total_tds <- as.numeric(unlist(xml2::as_list(tbs[length(tbs)-86])))
  # Field-goal good--attempts
  fg_stats <- unlist(xml2::as_list(tbs[length(tbs)-16]))
  fg_stats <- as.numeric(unlist(strsplit(fg_stats, "-")))
  rm(resp_cont)
  rm(tbs)
  rm(lis)
  scraped <- list(record = record, total_tds = total_tds, fg_stats = fg_stats)
  mtds <- total_tds/sum(record)
  mfgs <- fg_stats[1]/sum(record)
  mextras <- (total_points-total_tds*6-3*fg_stats[1])/sum(record)
  means <- c(tds = mtds, fgs = mfgs, extras = mextras, expected_points = total_points/sum(record))
  return(list(scraped = scraped, means = means))
}


#' ESPN stats scraper for NHL data
#'
#' @param teamName team name, the full name
#'
#' @description {Get mean number of goals per game for a team.}
#' @return a named list containing
#' \itemize{
#' \item \code{scraped} a list containing the scraped data of
#' \code{record}, \code{total_tds}, and \code{fg_stats}
#' \item \code{means} }
#' @export nhl_stats
nhl_stats <- function(teamName)
{
  url <- nhl_endpoints(teamName)
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

#' ESPN stats scraper for NBA data
#'
#' @param team team name, the full name
#'
#' @description {Get mean number of goals per game for a team.}
#' @return Returns a vector of mean 2pters, 3pters, and free-throws as well as number of games played
#' @export nba_stats
nba_stats <- function(team)
{
  url <- nba_endpoints(team)
  resp <- httr::GET(url = url)
  resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")
  tbs <- xml2::xml_find_all(resp_cont, "//td")
  lis <- xml2::xml_find_all(resp_cont, "//li")
  # Games played and average number of goals
  # 15 columns.
  # This should work for all teams
  num_rows <- floor(length(tbs)/15)/2
  gp_index <- num_rows*15-13
  # if(team == "Utah Jazz")
  # {
  #   gp_index <- 13*14
  # } else if(team == "Milwaukee Bucks")
  # {
  #   gp_index <- 13*16+4
  # } else if(team == "Boston Celtics")
  # {
  #   gp_index <- 13*10+7
  # } else if(team == "Golden State Warriors")
  # {
  #   gp_index <- 13*10+7
  # } else if(team == "Phoenix Suns")
  # {
  #   gp_index <- 13*10+7
  # }
  gp <- tbs[[gp_index]] # Hopefully every team has the same number of players!
  gp <- as.numeric(unlist(xml2::as_list(gp)))
  mean_fgs <- tbs[[length(tbs)-13]]
  mean_3pts <- tbs[[length(tbs)-10]]
  mean_fts <- tbs[[length(tbs)-7]]

  mean_fgs <- as.numeric(unlist(xml2::as_list(mean_fgs)))
  mean_3pts <- as.numeric(unlist(xml2::as_list(mean_3pts)))
  mean_fts <- as.numeric(unlist(xml2::as_list(mean_fts)))
  means <- c(mean_fgs, mean_3pts, mean_fts, gp)
  return(means)
}




