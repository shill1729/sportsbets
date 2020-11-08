

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
  t_over <- kelly_binary(total_estimates$over, a = 100/total_wager, 1)
  t_under <- kelly_binary(total_estimates$under, a = 100/total_wager, 1)
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
  m_und <- kelly_binary(moneyline_estimates$underdog, a = underdog_plus/100, 1)
  m_fav <- kelly_binary(moneyline_estimates$fav, a = 100/fav_minus, 1)
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
  m_und <- kelly_binary(spread_estimates$underdog, a = 100/110, 1)
  m_fav <- kelly_binary(spread_estimates$fav, a = 100/110, 1)
  optimal_bets <- list(spread = c(fav = m_fav, underdog = m_und))
  return(list(estimates = spread_estimates, bets = optimal_bets))

}

#' Optimal allocations among many independent bets
#'
#' @param ps vector of chances
#' @param a vector of win payouts
#' @param b vector of risk payins
#' @param restraint percentage of wealth to restrict to
#'
#' @description {Given independent alternatives to wager on on the same trial, going up to
#' a percentage of one's bankroll, optimal allocations are computed via
#' log-maximization of terminal wealth.}
#' @return vector
#' @export kelly_totals
kelly_totals <- function(ps, a, b, restraint = 1)
{
  m <- length(ps)
  l <- list()
  print("Computing coefficients")
  for(i in 1:m)
  {
    l[[i]] <- c(ps[i], 1-ps[i])
  }
  chance_coefs <- expand.grid(l)
  chance_coefs <- apply(chance_coefs, 1, prod)


  print("Computing log arguments")
  ll <- rep(list(0:1), m)
  outcomes <- 1-expand.grid(ll) # order is reversed
  logargs <- 2*outcomes-1
  for(i in 1:(2^m))
  {
    for(j in 1:m)
    {
      if(logargs[i, j] < 0)
      {
        logargs[i, j] <- -b[j]
      } else if(logargs[i, j] > 0)
      {
        logargs[i, j] <- a[j]
      }
    }
  }
  # print(logargs)
  # constrOptim *minimizes* functions, so we use negatives
  g <- function(x)
  {
    r <- apply(as.matrix(logargs), 1, function(y) y%*%x)
    return(-sum(chance_coefs*log(1+r)))
  }

  # -restraint for budget constraint (to get u cdot 1^T <= restraint), >0, -u>-1 for 0<u<1
  bvec <- c(-restraint, rep(0, m), rep(-1, m))
  # for row is 1s for budget equality constraint, then diag matrices
  Amat <- cbind(matrix(rep(-1, m), nrow = m), diag(x = 1, m, m), diag(x = -1, m, m))
  print("Optimizing")
  kf <- stats::constrOptim(theta = rep(1/(m*3), m),
                           f = g, grad = NULL, ui = t(Amat), ci = bvec)
  print(-kf$value)
  return(kf$par)
}

#' Compute chances of over/under for a given live line on NFL game week
#'
#' @param tdat data returned from \code{espn_nfl_line}
#' @param n number of variates
#'
#' @description {A linear combination of Poisson RVs is used to model the points of each team.}
#' @return list
#' @export weekTotalChances
weekTotalChances <- function(tdat = NULL, n = 5*10^4)
{
  if(is.null(tdat))
  {
    print("Getting data from ESPN")
    tdat <- espn_nfl_line()

  }
  over_chances <- matrix(0, nrow = nrow(tdat))
  under_chances <- matrix(0, nrow = nrow(tdat))
  for(i in 1:nrow(tdat))
  {
    matchup <- c(team_endpoint(tdat$favs[i]),  team_endpoint(tdat$underdogs[i]))
    print(matchup)
    fav_stat <- espn_nfl_scrape(matchup[1])
    und_stat <- espn_nfl_scrape(matchup[2])
    ou_est <- nfl_total_cdf(line = tdat$line[i], means = fav_stat$means+und_stat$means, n)
    over_chances[i] <- ou_est$over
    under_chances[i] <- ou_est$under
    Sys.sleep(0.2)
  }
  dat <- tdat[, c("favs", "underdogs", "line")]
  dat$over <- as.numeric(over_chances)
  dat$under <- as.numeric(under_chances)
  return(dat)

}

#' Log optimal allocations among independent bets
#'
#' @param bankroll wealth to bet with
#' @param tdat scraped data from \code{espn_live_line}
#' @param n number of trials in MC estimations
#' @param restraint percentage of wealth to use
#' @param wager wager to risk
#' @param top how many games to bet on
#'
#' @description {Kelly-criterion applied to multiple independent bets per
#' trial.}
#' @return list of two data.frames \code{over} and \code{under}
#' both containing the same column names except the last one.
#' Contained is the model-estimate for the outcome together with the
#' optimal fraction of wealth to bet.
#' @export logOptimalTotals
logOptimalTotals <- function(bankroll = 444, tdat = NULL, n = 5*10^4, restraint = 1, wager = 110, top = 2)
{
  w <- weekTotalChances(tdat, n)
  a <- rep(100/wager, length(w$over))
  b <- rep(1, length(w$over))

  overEdge <- w[order(w$over, decreasing = TRUE), ]
  underEdge <- w[order(w$under, decreasing = TRUE), ]
  if(length(top) > length(w$over))
  {
    stop("Use a smaller top-games input")
  }
  overEdge <- overEdge[1:top,]
  underEdge <- underEdge[1:top, ]

  okf <- kelly_totals(ps = overEdge$over, a = a, b = b, restraint)
  ukf <- kelly_totals(ps = underEdge$under, a = a, b = b, restraint)
  overEdge$kellyOver <- okf
  underEdge$kellyUnder <- ukf
  overEdge$bet <- okf*bankroll
  underEdge$bet <- ukf*bankroll
  decision <- list(over = overEdge, under = underEdge)
  return(decision)
}

#' Log optimal allocations among independent bets for NFL
#'
#' @param bankroll wealth to bet with
#' @param tdat scraped data from \code{espn_live_line}
#' @param n number of trials in MC estimations
#' @param restraint percentage of wealth to use
#' @param wager wager to risk for total over/under (110 default)
#' @param top how many games to bet on
#'
#' @description {Kelly-criterion applied to multiple independent bets per
#' trial.}
#' @return list of four data.frames \code{over} and \code{under} and \code{fave} and \code{underdog}
#' both containing the same column names except the last one.
#' Contained is the model-estimate for the outcome together with the
#' optimal fraction of wealth to bet.
#' @export logOptimalNFL
logOptimalNFL <- function(bankroll = 444, tdat = NULL, n = 5*10^4, restraint = 1, wager = 110, top = 2)
{
  estimates <- nfl_model_chances(tdat, n)
  m <- nrow(estimates)
  # Now we must take only the top-games and compute optimal bets

  mlf_a <- rep(100, m)/-estimates$fav_risk
  mlf_b <- rep(1, m)
  mlu_a <- estimates$und_win/100
  mlu_b <- rep(1, m)

  ou_a <- rep(100/wager, m)
  ou_b <- rep(1, m)

  overEdge <- estimates[order(estimates$over, decreasing = TRUE), ]
  underEdge <- estimates[order(estimates$under, decreasing = TRUE), ]
  mlfEdge <- estimates[order(estimates$mlf, decreasing = TRUE), ]
  mluEdge <- estimates[order(estimates$mlu, decreasing = TRUE), ]
  psfEdge <- estimates[order(estimates$psf, decreasing = TRUE), ]
  psuEdge <- estimates[order(estimates$psu, decreasing = TRUE), ]
  if(length(top) > m)
  {
    stop("Use a smaller top-games input")
  }
  overEdge <- overEdge[1:top,]
  underEdge <- underEdge[1:top, ]
  mlfEdge <- mlfEdge[1:top, ]
  mluEdge <- mluEdge[1:top, ]
  psfEdge <- psfEdge[1:top, ]
  psuEdge <- psuEdge[1:top, ]

  okf <- kelly_totals(ps = overEdge$over, a = ou_a, b = ou_b, restraint)
  ukf <- kelly_totals(ps = underEdge$under, a = ou_a, b = ou_b, restraint)
  mlfkf <- kelly_totals(ps = mlfEdge$mlf, a = mlf_a, b = mlf_b, restraint)
  mlukf <- kelly_totals(ps = mluEdge$mlu, a = mlu_a, b = mlu_b, restraint)
  psfkf <- kelly_totals(ps = psfEdge$psf, a = ou_a, b = ou_b, restraint)
  psukf <- kelly_totals(ps = psuEdge$psu, a = ou_a, b = ou_b, restraint)

  overEdge$kellyOver <- okf
  underEdge$kellyUnder <- ukf
  mlfEdge$kellyFave <- mlfkf
  mluEdge$kellyUnder <- mlukf
  psfEdge$kellyFave <- psfkf
  psuEdge$kellyUnder <- psukf

  overEdge$bet <- okf*bankroll
  underEdge$bet <- ukf*bankroll
  mlfEdge$bet <- mlfkf*bankroll
  mluEdge$bet <- mlukf*bankroll
  psfEdge$bet <- psfkf*bankroll
  psuEdge$bet <- psukf*bankroll

  # Remove unnecessary values
  overEdge <- overEdge[, -c(3:8)]
  underEdge <- underEdge[, -c(3:8)]
  mlfEdge <- mlfEdge[, -c(7:11)]
  mluEdge <- mluEdge[, -c(7:11)]
  psfEdge <- psfEdge[, -c(3:6, 9:11)]
  psuEdge <- psuEdge[, -c(3:6, 9:11)]

  decision <- list(over = overEdge,
                   under = underEdge,
                   mlf = mlfEdge,
                   mlu = mluEdge,
                   psf = psfEdge,
                   psu = psuEdge
                   )
  # print(decision)
  return(decision)
}

