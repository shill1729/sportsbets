#' Kelly fraction for arbitrary binary (non-random) payoffs
#'
#' @param p the probability of winning
#' @param a the amount won on top of the wager
#' @param b the wager lost
#'
#' @description {The classical Kelly criterion formula for Bernoulli bets of arbitrary odds.}
#' @description {The formula is well known, i.e. on wikipedia.}
#' @return numeric
#' @export kelly_binary
kelly_binary <- function(p, a, b)
{
  p/b-(1-p)/a
}

#' Growth rate for Kelly fraction for arbitrary binary (non-random) payoffs
#'
#' @param a the amount won on top of the wager
#' @param b the wager lost
#' @param p the probability of winning
#'
#' @description {The growth rate or entropy of the Kelly strategy}
#' @description {The average log wealth at the optimal strategy.}
#' @return numeric
#' @export entropy_binary
entropy_binary <- function(a, b, p)
{
  p*log(p)+(1-p)*log(1-p)+log(a+b)+p*log(a/b)-log(a)
}
