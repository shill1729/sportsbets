#' ESPN stats scraper
#'
#' @param team team name, usually first three letters of city or team-name.
#'
#' @description {Get mean number of touchdowns and (good) field goals per game for the
#' current season}
#' @return list
#' @export espn_nfl_scrape
espn_nfl_scrape <- function(team)
{
  print(paste("Getting", team, "data"))
  url <- "https://www.espn.com/nfl/team/stats/_/type/team/name/"
  url <- paste(url, team, sep = "")
  # url <- "https://www.pro-football-reference.com/teams/buf/2020.htm"
  resp <- httr::GET(url = url)
  resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")
  # pl <- xml2::as_list(x = resp_cont)
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
  mextras <- (total_points-total_tds*6-2*fg_stats[1])/sum(record)
  means <- c(tds = mtds, fgs = mfgs, extras = mextras, expected_points = total_points/sum(record))

  # print(means)
  return(list(scraped = scraped, means = means))
}
