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




#' Log optimal allocations among independent bets for NFL
#'
#' @param bankroll wealth to bet with
#' @param estimates object returned from \code{nfl_model_chances}
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
logOptimalNFL <- function(bankroll = 444, estimates = NULL, restraint = 1, wager = 110, top = 2)
{
  if(is.null(estimates))
  {
    estimates <- nfl_model_chances()
  }

  m <- nrow(estimates)
  # Now we must take only the top-games and compute optimal bets

  mlf_a <- rep(100, m)/-estimates$fav_risk
  mlf_b <- rep(1, m)
  mlf_io <- mlf_b/(mlf_a+mlf_b)

  mlu_a <- estimates$und_win/100
  mlu_b <- rep(1, m)
  mlu_io <- mlu_b/(mlu_a+mlu_b)

  ou_a <- rep(100/wager, m)
  ou_b <- rep(1, m)

  # Rank the chances; for ML must use deviation from posted odds
  # (since they differ)
  overEdge <- estimates[order(estimates$over, decreasing = TRUE), ]
  underEdge <- estimates[order(estimates$under, decreasing = TRUE), ]
  mlfEdge <- estimates[order(estimates$mlf-mlf_io, decreasing = TRUE), ]
  mluEdge <- estimates[order(estimates$mlu-mlu_io, decreasing = TRUE), ]
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

  okf <- kellyMultipleBets(ps = overEdge$over, a = ou_a, b = ou_b, restraint)
  ukf <- kellyMultipleBets(ps = underEdge$under, a = ou_a, b = ou_b, restraint)
  mlfkf <- kellyMultipleBets(ps = mlfEdge$mlf, a = mlf_a, b = mlf_b, restraint)
  mlukf <- kellyMultipleBets(ps = mluEdge$mlu, a = mlu_a, b = mlu_b, restraint)
  psfkf <- kellyMultipleBets(ps = psfEdge$psf, a = ou_a, b = ou_b, restraint)
  psukf <- kellyMultipleBets(ps = psuEdge$psu, a = ou_a, b = ou_b, restraint)

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

