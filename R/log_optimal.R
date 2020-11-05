

#' Kelly-criterion for Total points bets
#'
#' @param teams vector of team names
#' @param line the line for betting over/under
#' @param total_wager wager risked (-b)
#' @param n number of variates to use in Monte-Carlo estimations
#'
#' @description {Log optimal wager strategy for total points betting. Based on
#' linear combination of Poisson RVs model for points.}
#' @return list
#' @export log_optimal_total_bet
log_optimal_total_bet <- function(teams, line, total_wager = 110, n = 10^5)
{
  dat1 <- espn_nfl_scrape(teams[1])
  dat2 <- espn_nfl_scrape(teams[2])
  total_means <- dat1$means+dat2$means
  total_estimates <- nfl_total_cdf(line, total_means, n = n)
  t_over <- KellyCriterion::kelly_binary(total_estimates$over, a = 100/total_wager, 1)
  t_under <- KellyCriterion::kelly_binary(total_estimates$under, a = 100/total_wager, 1)
  optimal_bets <- list(total = c(over = t_over, under = t_under))
  return(list(estimates = total_estimates, bets = optimal_bets))
}

#' Kelly-criterion for moneyline bets
#'
#' @param teams vector of team names
#' @param fav_minus risk for betting on favorite (-b)
#' @param underdog_plus profit for betting on underdog (+a)
#' @param n number of variates to use in Monte-Carlo estimations
#'
#' @description {Log optimal wager strategy for moneyline betting. Based on
#' linear combination of Poisson RVs model for points.}
#' @return list
#' @export log_optimal_moneyline_bet
log_optimal_moneyline_bet <- function(teams, fav_minus, underdog_plus, n = 10^5)
{
  dat1 <- espn_nfl_scrape(teams[1])
  dat2 <- espn_nfl_scrape(teams[2])
  moneyline_estimates <- nfl_moneyline_cdf(dat1$means, dat2$means, n = n)
  m_und <- KellyCriterion::kelly_binary(moneyline_estimates$underdog, a = underdog_plus/100, 1)
  m_fav <- KellyCriterion::kelly_binary(moneyline_estimates$fav, a = 100/fav_minus, 1)
  optimal_bets <- list(moneyline = c(fav = m_fav, underdog = m_und))
  return(list(estimates = moneyline_estimates, bets = optimal_bets))

}

#' Kelly-criterion for moneyline bets
#'
#' @param teams vector of team names
#' @param spread the point-spread
#' @param n number of variates to use in Monte-Carlo estimations
#'
#' @description {Log optimal wager strategy for moneyline betting. Based on
#' linear combination of Poisson RVs model for points.}
#' @return list
#' @export log_optimal_spread_bet
log_optimal_spread_bet <- function(teams, spread, n = 10^5)
{
  dat1 <- espn_nfl_scrape(teams[1])
  dat2 <- espn_nfl_scrape(teams[2])
  spread_estimates <- nfl_spread_cdf(dat1$means, dat2$means, spread, n = n)
  m_und <- KellyCriterion::kelly_binary(spread_estimates$underdog, a = 100/110, 1)
  m_fav <- KellyCriterion::kelly_binary(spread_estimates$fav, a = 100/110, 1)
  optimal_bets <- list(spread = c(fav = m_fav, underdog = m_und))
  return(list(estimates = spread_estimates, bets = optimal_bets))

}

#' Log optimal bet for an independent but not identically distributed sequence
#'
#' @param chances sequence of chances of winning the bet
#' @param a payout, default 100
#' @param b risk, default 110
#'
#' @description {Log optimal growth for a finite sequence of bets with different
#' probabilities.}
#' @return numeric
#' @export logOptimalWeekBet
logOptimalWeekBet <- function(chances, a = 100/110, b = 1)
{
  N <- length(chances)
  p <- sum(chances)
  return(((a+b)*p-b*N)/(N*a*b))
}

#' Maximize log-growth of a sequence of bets with different chances
#'
#' @param tdat data returned from \code{espn_nfl_line}
#' @param n number of variates
#'
#' @description {Work in progress model for non-identical chance bets.}
#' @return list
#' @export logOptWeekTotal
logOptWeekTotal <- function(tdat = NULL, n = 5*10^4)
{
  if(is.null(tdat))
  {
    tdat <- espn_nfl_line()
  }
  over_chances <- matrix(0, nrow = nrow(tdat))
  under_chances <- matrix(0, nrow = nrow(tdat))
  for(i in 1:nrow(tdat))
  {
    matchup <- c(team_endpoint(tdat$favs[i]),  team_endpoint(tdat$underdogs[i]))
    # print(matchup)
    fav_stat <- espn_nfl_scrape(matchup[1])
    und_stat <- espn_nfl_scrape(matchup[2])
    ou_est <- nfl_total_cdf(line = tdat$line[i], means = fav_stat$means+und_stat$means, n)
    over_chances[i] <- ou_est$over
    under_chances[i] <- ou_est$under
    Sys.sleep(0.2)
  }
  output <- data.frame(overOptimal = logOptimalWeekBet(over_chances), underOptimal = logOptimalWeekBet(under_chances))
  return(list(data = tdat, model_over = over_chances, model_under = under_chances, bet = output))

}
