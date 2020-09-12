#' The chance of a given point spread under the Skellam model
#'
#' @param k the size of the point spread (in the direction of the favorite)
#' @param lambda mean number of points for the favorite
#' @param mu mean number of points for the underdog
#'
#' @description {The probability of the event \eqn{\{X-Y>k\}}}
#' @return numeric or vector
#' @export point_spread
point_spread <- function(k, lambda, mu)
{
  1-pskellam(k, lambda, mu)
}

#' The chance of the favorite winning
#'
#' @param lambda mean number of points for the favorite \eqn{X}
#' @param mu mean number of points for the underdog \eqn{Y}
#'
#' @description {The probability of the event \eqn{\{X-Y>0\}}}
#' @return numeric or vector
#' @export moneyline
moneyline <- function(lambda, mu)
{
  # Chance of favorite winning Z = X-Y>0 X pois lambda > mu
  point_spread(0, lambda, mu)
}



#' The chance of a given point spread under the Skellam model
#'
#' @param k the size of the point spread (in the direction of the favorite)
#' @param lambda mean number of points for the favorite
#' @param mu mean number of points for the underdog
#' @param over boolean, true for over and false for under
#'
#' @description {The probability of the event \eqn{\{X+Y>k\}} or its complement.}
#' @return numeric or vector
#' @importFrom stats ppois rpois
#' @export overunder
overunder <- function(k, lambda, mu, over = TRUE)
{
  ifelse(over, 1, 0)*(1-ppois(k, lambda = lambda+mu))+
    (1-ifelse(over, 1, 0))*ppois(k, lambda = lambda+mu)
}


