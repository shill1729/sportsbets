#' Kelly-criterion for binary IID bets
#'
#' @param p probability of success
#' @param a winnings on top of wager
#' @param b wager
#'
#' @return numeric
#' @export kellyBinary
kellyBinary <- function(p, a, b)
{
  return(p/b-(1-p)/a)
}

#' Growth rate of the Kelly-criterion for binary IID bets
#'
#' @param p probability of success
#' @param a winnings on top of wager
#' @param b wager
#'
#' @return numeric
#' @export kellyBinary
entropyBinary <- function(p, a, b)
{
  p*log(p)+(1-p)*log(1-p)+log(a+b)+p*log(a/b)-log(a)
}






#' Compute optimal over allocations for NBA games
#'
#' @param bankroll the account value to bet with
#' @param underdogs list of underdog team names
#' @param faves list of favorite team names
#' @param lines vector of lines
#' @param b vector of wagers (typically 110)
#' @param n number of steps to use in CF integration or MC trials
#' @param alpha significance level for LBs and UBs for estimates
#' @param ci boolean for using CI
#' @param method string "cf" or "mc" for characteristic function method or Monte-Carlo
#'
#' @description {Wrapper to computing over chances and allocaitons given series of games.}
#'
#' @return prints out the bets.
#' @export nba_over
nba_over <- function(bankroll, underdogs, faves, lines, b, n=1000, alpha=0.15, ci=TRUE, method="cf")
{
  market <- data.frame(underdogs, faves, b, lines)
  print(market)
  over <- matrix(0, nrow(market))
  for(i in 1:nrow(market))
  {
    print(c(market$faves[i], market$underdogs[i]))
    # Ditch game's played
    fav_stat <- nba_stats(market$faves[i])
    und_stat <- nba_stats(market$underdogs[i])
    gp_fav <- fav_stat[length(fav_stat)]
    gp_und <- und_stat[length(und_stat)]
    fav_stat <- fav_stat[1:3]
    und_stat <- und_stat[1:3]
    if(ci)
    {
      # Multiplying factor for one sided upper bound for Variance
      beta1 <- (gp_fav-1)/stats::qchisq(alpha,df=gp_fav-1)
      beta2 <- (gp_und-1)/stats::qchisq(alpha,df=gp_und-1)
      # Offset adjustment for one-sided LB of means
      # We assume sample_var=sample_mean due to assuming the counts ~ Poisson
      fav_stat <- fav_stat-stats::qt(1-alpha, df=gp_fav-1)*sqrt(fav_stat*beta1)/sqrt(gp_fav)
      und_stat <- und_stat-stats::qt(1-alpha, df=gp_und-1)*sqrt(und_stat*beta2)/sqrt(gp_und)
    }
    matchup_stats <- rbind(fav_stat, und_stat)
    colnames(matchup_stats) <- c("2pts", "3pts", "free-throws")
    print(c(gp_fav, gp_und))
    print(matchup_stats)
    print("Expected number of points")
    print(as.numeric(c(2,3,1)%*%(fav_stat+und_stat)))

    over[i] <- total_points_chances(line = market$lines[i],
                                    means = fav_stat+und_stat,
                                    points = c(2,3,1),
                                    method = method,
                                    n = n
    )$over
  }
  model <- market
  model$posted <- model$b/(100+model$b)
  model$over <- over

  bets <- kellyMultipleBets(ps=as.numeric(model$over), rep(100, nrow(market))/b, rep(1, nrow(market)))
  straight_kelly <- kellyBinary(p=as.numeric(model$over), 100/b, 1)
  g <- growth_rate(bets, as.numeric(model$over), rep(100, nrow(market))/b, rep(1, nrow(market)))
  gs <- entropyBinary(p=as.numeric(model$over), 100/b, 1)
  bets <- rbind(bets, straight_kelly, gs)
  rownames(bets) <- c("multiple_kelly", "straight_kelly", "ind_growth_rates")
  # print("Model")
  # print(model)
  # print("Allocations")
  # print(round(bets, 5))
  # print(c(growth=g))
  output <- list(model = model, bets = round(bets, 5), growth = g,
                 dollar_bets = bets[1,]*bankroll)
  return(output)
}


