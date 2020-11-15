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
