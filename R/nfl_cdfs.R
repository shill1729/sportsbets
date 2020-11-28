#'  Monte-Carlo estimates of Total points distribution
#'
#' @param line the line
#' @param means the vector of mean number of touchdowns and (good) field goals
#' of both teams cumulatively
#'
#' @description {Compute via Monte-Carlo probabilities of being over or under a given line.}
#' @details {We model the point process of a football team by a linear combination of Poisson RVs,
#' where the coefficients are the points earned for each type of goal. Since the data we use for the
#' mean number of touchdowns includes all touchdowns not just ones that have no conversion, the coefficients for the
#' the conversion points will be 1 and 2 rather than 7 and 8. I am not a football expert, so contact me if you have better ideas.}
#' @param n number of variates to use in Monte-Carlo estimation.
#'
#' @return data.frame of over, under, and push chances.
#' @export nfl_total_cdf
nfl_total_cdf <- function(line, means, n = 10^6)
{
  # mean touchdowns and (good) field-goals
  mtd <- means[1]
  mfg <- means[2]
  mextras <- means[3]
  # tds, fgs, safeties and everything else lumped into 1
  point_coef <- c(6, 3, 1)
  r1 <- rpois(n, mtd)
  r2 <- rpois(n, mfg)
  r3 <- rpois(n, mextras)
  r <- cbind(r1, r2, r3)
  # Linear combinations of Poisson RVs
  ps <- apply(r, 1, function(x) t(point_coef)%*%x)
  # Probabilities of events for Total
  over <- mean((ps > line))
  under <- mean((ps < line))
  push <- mean((ps == line))
  return(data.frame(over = over, under = under, push = push))
}

#' Monte-Carlo estimates of moneyline distribution
#'
#' @param fav_means vector of mean number of touchdowns and (good) field goals of favorite team
#' @param underdog_means vector of mean number of touchdowns and (good) field goals of underdog team
#' @param n number of variates to use in Monte-Carlo estimate
#'
#' @description {Compute via Monte-Carlo simulations the probability of money-line events
#' for a given favorite and underdog team. See the details documentation for the total cdf for
#' more details on the model.}
#' @return data.frame of favorite win chance and underdog win(+tie) chance.
#' @export nfl_moneyline_cdf
nfl_moneyline_cdf <- function(fav_means, underdog_means, n = 10^6)
{
  # tds, fgs, safeties, 1 point, 2 point conversions
  point_coef <- c(6, 3, 1)
  r1 <- rpois(n, fav_means[1])
  r2 <- rpois(n, fav_means[2])
  r3 <- rpois(n, fav_means[3])
  r_fav <- cbind(r1, r2, r3)
  ps_fav <- apply(r_fav, 1, function(x) t(point_coef)%*%x)

  # Underdog team
  r1 <- rpois(n, underdog_means[1])
  r2 <- rpois(n, underdog_means[2])
  r3 <- rpois(n, underdog_means[3]) # assume 1 safety on average per game (per both teams)
  r_und <- cbind(r1, r2, r3)
  ps_und <- apply(r_und, 1, function(x) t(point_coef)%*%x)
  fav_win <- mean((ps_fav > ps_und))
  und_win <- mean((ps_fav <= ps_und))
  return(data.frame(fav = fav_win, underdog = und_win))

}

#' Monte-Carlo estimates of point-spread distribution
#'
#' @param fav_means vector of mean number of touchdowns and (good) field goals of favorite team
#' @param underdog_means vector of mean number of touchdowns and (good) field goals of underdog team
#' @param spread the point spread to exceed
#' @param n number of variates to use in Monte-Carlo estimate
#'
#' @description {Compute via Monte-Carlo simulations the probability of point-spread events
#' for a given favorite and underdog team. See the details documentation for the total cdf for
#' more details on the model.}
#' @return data.frame of favorite win chance and underdog win(+tie) chance.
#' @export nfl_spread_cdf
nfl_spread_cdf <- function(fav_means, underdog_means, spread = 1.5, n = 10^6)
{
  # tds, fgs, safeties, 1 point, 2 point conversions
  point_coef <- c(6, 3, 1)
  r1 <- rpois(n, fav_means[1])
  r2 <- rpois(n, fav_means[2])
  r3 <- rpois(n, fav_means[3])
  r_fav <- cbind(r1, r2, r3)
  ps_fav <- apply(r_fav, 1, function(x) t(point_coef)%*%x)

  # Underdog team
  r1 <- rpois(n, underdog_means[1])
  r2 <- rpois(n, underdog_means[2])
  r3 <- rpois(n, underdog_means[3]) # assume 1 safety on average per game (per both teams)
  r_und <- cbind(r1, r2, r3)
  ps_und <- apply(r_und, 1, function(x) t(point_coef)%*%x)
  fav_win <- mean((ps_fav - ps_und > spread))
  und_win <- mean((ps_und - ps_fav > -spread ))
  return(data.frame(fav = fav_win, underdog = und_win))

}
