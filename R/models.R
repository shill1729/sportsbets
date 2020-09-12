
#' Entropy-growth for log-optimal moneyline bets
#'
#' @param p true probability of favorite winning
#' @param a winnings if favorite wins
#' @param b wager to throw down for favorite \eqn{-b}
#' @param u winnings if underdog wins \eqn{+u}
#' @param v wager to throw down for underdog
#'
#' @description {The entropy-growth for the log-optimal bet on two wagers for favorite
#' vs underdog team to win.}
#' @return numeric
#' @export entropy_moneyline
entropy_moneyline <- function(p, a, b, u, v)
{
  x <- kelly_moneyline(p, a, b, u, v)
  p*log(1+(a+v)*x-v)+(1-p)*log(1+u-(b+u)*x)
}

#' Kelly fraction for moneyline bets
#'
#' @param p true probability of favorite winning
#' @param a winnings if favorite wins
#' @param b wager to throw down for favorite \eqn{-b}
#' @param u winnings if underdog wins \eqn{+u}
#' @param v wager to throw down for underdog
#'
#' @description {A Kelly-criterion for a bet on two wagers for favorite
#' vs underdog team to win.}
#' @return numeric
#' @export kelly_moneyline
kelly_moneyline <- function(p, a, b, u, v)
{
  region_check <- (v-1)*(u+b)/((1+u)*(a+v))
  if(region_check >= 1)
  {
    stop("Feasible region DNE")
  }
  x <- ((1+u)*(a+v)*p-(1-v)*(b+u)*(1-p))/((b+u)*(a+v))
  return(x)
}

#' Simulate a series of IID trials of moneyline under log-optimal growth
#'
#' @param bankroll initial bankroll to bet
#' @param p true chance of outcome
#' @param wagers the vector of wagers, see details
#' @param trials the number of trials to simulate
#'
#' @description {Simulate a series of moneyline trials under log-optimal growth.}
#' @details {The argument \code{wagers} must contain four wagers, the first two the odds
#' for the favorite, the latter two the odds for the underdog.}
#' @return vector
#' @importFrom stats rbinom
#' @export sim_moneyline
sim_moneyline <- function(bankroll, p, wagers, trials = 100)
{
  a <- wagers[1]
  b <- wagers[2]
  u <- wagers[3]
  v <- wagers[4]

  # Initial wealth process
  z <- matrix(data = 0, nrow = trials+1)
  z[1] <- bankroll
  # Optimal strategy
  x <- kelly_moneyline(p, a, b, u, v)
  y <- 1-x

  # Simulate the Bernoulli outcomes 0,1 (loss, win)
  outcomes <- rbinom(n = trials, size = 1, prob = p)
  # Convert to bet outcomes:
  epsilon <- a*outcomes-b*(1-outcomes) # Favorite wins
  delta <- u*(1-outcomes)-v*outcomes # Underdog wins
  for(i in 1:(trials))
  {
    z[i+1] <- z[i]+epsilon[i]*z[i]*x+delta[i]*z[i]*y
  }
  return(z)
}

#' Model analysis for a moneyline bet
#'
#' @param bankroll the amount to bet with
#' @param favorite the \eqn{-b} in \eqn{a} to \eqn{b} odds for the favorite.
#' @param underdog the \eqn{+u} in \eqn{u} to \eqn{v} odds for the underdog.
#' @param lambda mean number of points per game by favorite
#' @param mu mean number of points by per game by underdog
#' @param trials number of trials to simulate
#'
#' @description {Log optimal strategy for gambling on moneyline event
#' under a Skellam distribution for the point spread.}
#' @return list of odds and decision rule
#' @importFrom graphics abline par
#' @importFrom stats uniroot
#' @export moneyline_analysis
moneyline_analysis <- function(bankroll, favorite, underdog, lambda, mu, trials = 30)
{
  if(favorite < underdog)
  {
    stop("Wager to throw down on favorite must be more than earnings from underdog")
  }
  a <- 100/favorite
  b <- 1
  u <- 1
  v <- 100/underdog
  # Estimation of true odds via Skellam model
  p <- moneyline(lambda, mu)
  # Log-optimal bet
  x <- kelly_moneyline(p, a, b, u, v)
  # Entropy
  g <- entropy_moneyline(p, a, b, u, v)
  log_opt <- c(bet = x, growth = g)
  # Implied market odds
  fav_odds <- b/(a+b)
  und_odds <- v/(u+v)
  odds <- c(true = p, fav = fav_odds, und = und_odds)
  # Output to return
  allocation <- c(bankroll = bankroll, favorite = bankroll*x, underdog = bankroll*(1-x))
  lb <- uniroot(f = entropy_moneyline, interval = c(0.01, fav_odds), a = a, b = b, u = u, v = v)
  ub <- uniroot(f = entropy_moneyline, interval = c(fav_odds, 0.99), a = a, b = b, u = u, v = v)
  output <- list(odds = odds, decision = log_opt, allocation = allocation,
                 edge_region = c(lb$root, ub$root))


  # Simulation
  wager <- c(a, b, u, v)
  s <- sim_moneyline(bankroll, p, wager, trials = trials)
  # Grid for chances for growth-function
  pp <- seq(0, 1, length.out = 100)
  par(mfrow = c(1, 2))
  plot(s, type = "l", main = "Sample path", xlab = "trials", ylab = "wealth")
  plot(pp, entropy_moneyline(pp, a, b, u, v), xlab = "true odds", ylab = "growth rate", type= "l", main = "Growth g(p)")
  abline(h = 0, lty = "dashed")
  abline(v = p, col = "blue", lty = "dashed")
  abline(v = fav_odds, lty = "dashed")
  abline(v = und_odds, lty = "dashed")
  abline(v = lb$root, lty = "dashed", col = "green")
  abline(v = ub$root, lty = "dashed", col = "green")

  return(output)
}


