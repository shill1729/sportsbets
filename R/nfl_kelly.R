#' Compute log-optimal bets for NFL wagers
#'
#' @param tdat data returned from \code{espn_nfl_line()}
#' @param n number of simulations
#'
#' @description {Compute bets that maximize log wealth for NFL wagers from ESPN
#' schedule.}
#' @return list of the ESPN data data.frame, optimal allocations under Kelly-Criterion, and growth-rates
#' @export nfl_kelly
nfl_kelly <- function(tdat = NULL, n = 5*10^4)
{
  if(is.null(tdat))
  {
    tdat <- espn_nfl_line()
  }
  optimal_bets <- list()
  growth_rates <- list()
  for(i in 1:nrow(tdat))
  {

    matchup <- c(team_endpoint(tdat$favs[i]),  team_endpoint(tdat$underdogs[i]))
    # print(matchup)
    fav_stat <- espn_nfl_scrape(matchup[1])
    und_stat <- espn_nfl_scrape(matchup[2])
    ml_est <- nfl_moneyline_cdf(fav_stat$means, und_stat$means, n)
    spread_est <- nfl_spread_cdf(fav_stat$means, und_stat$means, abs(tdat$spread[i]), n)
    ou_est <- nfl_total_cdf(line = tdat$line[i], means = fav_stat$means+und_stat$means, n)
    # print(list(fav_stat, und_stat, ml = ml_est, spread = spread_est, ou = ou_est))
    bets <- data.frame(mlf = kelly_binary(ml_est$fav, 100/abs(tdat$fav[i]), b = 1),
                       mlu = kelly_binary(ml_est$underdog, abs(tdat$underdog[i])/100, b = 1),
                       spreadf = kelly_binary(spread_est$fav, a = 100/110, 1),
                       spreadu = kelly_binary(spread_est$und, a = 100/110, 1),
                       ov = kelly_binary(ou_est$over, a = 100/110, 1),
                       un = kelly_binary(ou_est$under, a = 100/110, 1)
    )
    rates <- data.frame(mlf = entropy_binary(100/abs(tdat$fav[i]), b = 1, ml_est$fav),
                        mlu = entropy_binary(abs(tdat$underdog[i])/100, b = 1, ml_est$underdog),
                        spreadf = entropy_binary(a = 100/110, 1, spread_est$fav),
                        spreadu = entropy_binary(a = 100/110, 1, spread_est$und),
                        ov = entropy_binary(a = 100/110, 1, ou_est$over),
                        un = entropy_binary(a = 100/110, 1, ou_est$under)
    )
    optimal_bets[[i]] <- bets
    growth_rates[[i]] <- rates
    Sys.sleep(0.2)
  }
  optimal_bets <- as.data.frame(do.call(rbind, optimal_bets))
  optimal_bets <- data.frame(favs = tdat$favs, underdogs = tdat$underdogs, optimal_bets)
  growth_rates <- as.data.frame(do.call(rbind, growth_rates))
  growth_rates <- data.frame(favs = tdat$favs, underdogs = tdat$underdogs, growth_rates)
  return(list(data = tdat, model = optimal_bets, growth = growth_rates))
}

