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
  team_index <- which(teams == team_name)
  first3 <- substring(tolower(teams), 1, 3)
  first3[c(12, 15, 16, 17, 18, 19, 22, 23, 24, 25, 28, 30, 32)] <- c("gb", "jax", "kc", "lv", "lac", "lar",
                                                                     "ne", "no", "nyg", "nyj", "sf", "tb", "wsh")
  return(first3[team_index])
}


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
  # print(paste("Getting", team, "data"))
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
  mextras <- (total_points-total_tds*6-3*fg_stats[1])/sum(record)
  means <- c(tds = mtds, fgs = mfgs, extras = mextras, expected_points = total_points/sum(record))

  # print(means)
  return(list(scraped = scraped, means = means))
}

#' Scrape NFL line data from ESPN
#'
#' @description {Pulls and formats data from ESPN for current schedules line.}
#' @return data.frame
#' @export espn_nfl_line
espn_nfl_line <- function()
{
  url <- "https://www.espn.com/nfl/lines"
  resp <- httr::GET(url = url)
  resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")


  # Team names alternate every other index from 30 onward
  a <- xml2::xml_find_all(resp_cont, "//a")
  length(a)
  a[30] # the teams
  a[32] #

  # Could get the href too for the /nyj/, etc
  # a_attr <- xml2::xml_attrs(a[30])
  # a_attr[[1]][3]
  # a_attr
  # a[30]

  # Lines/etc are in td
  tbs <- xml2::xml_find_all(resp_cont, "//td")
  tbs # Cycles in patterns of index 6: div class, 4-0 (record), line, open, ml, fpi
  tbs[1] # div - don't need
  unlist(xml2::as_list(tbs[2])) # record
  unlist(xml2::as_list(tbs[3])) # line
  unlist(xml2::as_list(tbs[4])) # open
  unlist(xml2::as_list(tbs[5])) # ML
  unlist(xml2::as_list(tbs[6])) # fpi


  # Goal is to get data.frame of favorites, underdogs, line, open, ml, fpi
  # team-names cycle every two from 30 onward
  # stats cycle 1-6
  # The favorite is not always the first.
  records <- list()
  live_lines <- list()
  live_opens <- list()
  live_ml <- list()
  live_fpi <- list()
  for(i in 1:length(tbs))
  {
    if(i%%6==2)
    {
      records <- append(records, unlist(xml2::as_list(tbs[i])))
    }
    if(i%%6==3)
    {
      live_lines <- append(live_lines, unlist(xml2::as_list(tbs[i])))
    }
    if(i%%6==4)
    {
      live_opens <- append(live_opens, unlist(xml2::as_list(tbs[i])))
    }
    if(i%%6==5)
    {
      live_ml <- append(live_ml, unlist(xml2::as_list(tbs[i])))
    }
    if(i%%6==0)
    {
      live_fpi <- append(live_fpi, unlist(xml2::as_list(tbs[i])))
    }
  }
  records <- unlist(records)
  live_lines <- unlist(live_lines)
  live_opens <- unlist(live_opens)
  live_ml <- unlist(live_ml)
  live_fpi <- unlist(live_fpi)

  # Get all of the teams scheduled
  teamsScheduled <- list()
  # index arithmetic is so trivial, there is little point in commenting on it...
  for(i in 30+2*0:((length(tbs)-1)/6))
  {

    w <- xml2::as_list(a[i])
    team_name <- attr(w[[1]]$img, "title")
    # Sometimes the above does not work? for example Colts
    if(is.null(team_name))
    {
      # It's stored as this now? API sucks.
      team_name <- w[[1]][[1]]
    }
    if(team_name == "San Francisco 49ers")
    {
      team_name <- "San Francisco 49ERS"
    }
    teamEnd <- sportsbets::team_endpoint(team_name)
    # print(team_name)
    # print(teamEnd)
    teamsScheduled[(i-30)/2+1] <- team_name

  }
  teamsScheduled <- unlist(teamsScheduled)
  # Open lines can often be blank, which NA's on as.numeric.
  # dat <- data.frame(teamsScheduled, records, live_lines, live_opens, live_ml, live_fpi)
  dat <- data.frame(teamsScheduled, records, live_lines, live_ml, live_fpi)
  # Data-formatting
  dat$live_fpi <- gsub("%", "", dat$live_fpi)
  # Convert to probabilities
  dat$live_fpi <- as.numeric(dat$live_fpi)/100
  # Convert all to numeric
  suppressWarnings(dat[, -c(1, 2)] <- apply(dat[,-c(1, 2)], 2, as.numeric))

  # If there is an NA on an odd-row index, remove that index and index+1
  # if there is an NA on an even-row index, remove that index and index-1
  toDrop <- list()
  for(i in 1:nrow(dat))
  {
    if(is.na(dat$live_lines[i]))
    {
      if(i %% 2 == 1)
      {
        toDrop[[i]] <- c(i, i+1)
      } else if(i %% 2 == 0)
      {
        toDrop[[i]] <- c(i-1, i)
      }
    }
  }
  toDrop <- unlist(toDrop)
  if(!is.null(toDrop))
  {
    dat <- dat[-toDrop, ]

  }

  dat$sentiment <- ifelse(dat$live_lines <= 0, "favorite", "underdog")
  # print(dat)
  # Now we are going to transform the data-set game-wise to be suitable to our log optimal functions
  # Since we do not want to lose order of the matchups, best to approach iteratively unless
  # you are sure about a more clever way.
  favs <- list()
  underdogs <- list()
  game_line <- list()
  game_spread <- list()
  game_fav_ml <- list()
  game_und_ml <- list()
  game_fav_fpi <- list()
  game_und_fpi <- list()
  for(i in 1:nrow(dat))
  {
    if(dat[i, ]$sentiment == "favorite")
    {
      favs[[i]] <- dat[i, ]$teamsScheduled
      game_spread[[i]] <- dat[i, ]$live_lines
      game_fav_ml[[i]] <- dat[i, ]$live_ml
      game_fav_fpi[[i]] <- dat[i, ]$live_fpi
    } else if(dat[i, ]$sentiment == "underdog")
    {
      underdogs[[i]] <- dat[i, ]$teamsScheduled
      game_line[[i]] <- dat[i, ]$live_lines
      if(dat[i, ]$live_ml < 0)
      {
        game_und_ml[[i]] <- 100
      } else{
        game_und_ml[[i]] <- dat[i, ]$live_ml
      }
      game_und_fpi[[i]] <- dat[i, ]$live_fpi
    }

  }
  favs <- unlist(favs)
  underdogs <- unlist(underdogs)
  game_line <- unlist(game_line)
  game_spread <- unlist(game_spread)
  game_fav_ml <- unlist(game_fav_ml)
  game_und_ml <- unlist(game_und_ml)
  game_fav_fpi <- unlist(game_fav_fpi)
  game_und_fpi <- unlist(game_und_fpi)
  tdat <- data.frame(favs, underdogs, line = game_line, spread = game_spread, fav = game_fav_ml, underdog = game_und_ml)
  tdat$posted_fav <- (abs(tdat$fav)/(100+abs(tdat$fav)))
  tdat$posted_und <- (100/(abs(tdat$underdog)+100))
  return(tdat)

}
