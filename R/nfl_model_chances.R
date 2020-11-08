#' Estimate model-chances of moneyline, point-spread, and o/u events
#'
#' @param tdat data returned from \code{espn_nfl_line()}
#' @param n number of simulations
#'
#' @description {Using a linear combination of Poisson RVs to model the points of a given
#' team, independently, this function computes estimates of chances of moneyline, point-spread,
#' and o/u events via Monte-Carlo averaging.}
#' @return list of model-estimated chances for outcomes in moneyline, point-spread, and o/u
#' @export nfl_model_chances
nfl_model_chances <- function(tdat = NULL, n = 5*10^4)
{
  if(is.null(tdat))
  {
    print("Scraping ESPN live line")
    tdat <- espn_nfl_line()
  }
  ml <- matrix(0, nrow = nrow(tdat), ncol = 2)
  pts <- matrix(0, nrow = nrow(tdat), ncol = 2)
  ou <- matrix(0, nrow = nrow(tdat), ncol = 3)
  print("Estimating chances for all wagers over the week")
  for(i in 1:nrow(tdat))
  {

    matchup <- c(team_endpoint(tdat$favs[i]),  team_endpoint(tdat$underdogs[i]))
    print(matchup)
    fav_stat <- espn_nfl_scrape(matchup[1])
    und_stat <- espn_nfl_scrape(matchup[2])

    ml[i, ] <- as.matrix(nfl_moneyline_cdf(fav_stat$means, und_stat$means, n))
    pts[i, ] <- as.matrix(nfl_spread_cdf(fav_stat$means, und_stat$means, abs(tdat$spread[i]), n))
    ou[i, ] <- as.matrix(nfl_total_cdf(line = tdat$line[i], means = fav_stat$means+und_stat$means, n))
  }

  ml <- as.data.frame(ml)
  pts <- as.data.frame(pts)
  ou <- as.data.frame(ou)
  names(ml) <- c("fave", "underdog")
  names(pts) <- c("fave", "underdog")
  names(ou) <- c("over", "under", "push")

  dat <- data.frame(favorites = tdat$favs, underdogs = tdat$underdogs,
                    fav_risk = tdat$fav, und_win = tdat$underdog,
                    mlf = ml$fave, mlu = ml$underdog,
                    psf = pts$fave, psu = pts$underdog,
                    over = ou$over, under = ou$under, push = ou$push)
  return(dat)
}

