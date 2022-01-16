#' Optimal allocations among many independent bets
#'
#' @param x the allocation for the bets
#' @param ps vector of chances
#' @param a vector of win payouts
#' @param b vector of risk payins
#'
#' @description {Compute the growth-rate for a given allocation under the
#' independent simultaneous binary wager model.}
#' @return vector
#' @export growth_rate
growth_rate <- function(x, ps, a, b)
{
  m <- length(ps)
  l <- list()
  #print("Computing coefficients")
  for(i in 1:m)
  {
    l[[i]] <- c(ps[i], 1-ps[i])
  }
  chance_coefs <- expand.grid(l)
  chance_coefs <- apply(chance_coefs, 1, prod)


  #print("Computing log arguments")
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
  # print("All possible payoffs:")
  # print(logargs)
  # constrOptim *minimizes* functions, so we use negatives
  r <- apply(as.matrix(logargs), 1, function(y) y%*%x)
  return(-sum(chance_coefs*log(1+r)))
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
#' @export kellyMultipleBets
kellyMultipleBets <- function(ps, a, b, restraint = 1)
{
  # TODO remove this and add extra args to constrOpt
  m <- length(ps)
  l <- list()
  #print("Computing coefficients")
  for(i in 1:m)
  {
    l[[i]] <- c(ps[i], 1-ps[i])
  }
  chance_coefs <- expand.grid(l)
  chance_coefs <- apply(chance_coefs, 1, prod)


  #print("Computing log arguments")
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
  #print("All possible payoffs:")
  #print(logargs)
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
  #print("Optimizing")
  kf <- stats::constrOptim(theta = rep(restraint/(3*m), m),
                           f = g, grad = NULL, ui = t(Amat), ci = bvec)
  #print(-kf$value)
  return(kf$par)
}
