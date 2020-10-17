

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

