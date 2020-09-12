#' PMF of Skellam distribution i.e. difference of two independent Poisson RVs
#'
#' @param k argument of PMF
#' @param lambda mean number of points of favorite team
#' @param mu mean number of points of underdog team
#'
#' @description {The Skellam distribution for the difference of two independent
#' Poisson RVs is used to model point-spread in games with goals worth unit-points.}
#' @return numeric or vector
#' @export dskellam
dskellam <- function(k, lambda, mu)
{
  exp(-(lambda+mu))*(sqrt(lambda/mu)^k)*besselI(2*sqrt(lambda*mu), nu = k)
}

#' CDF of Skellam distribution i.e. difference of two independent Poisson RVs
#'
#' @param k argument of PMF
#' @param lambda mean number of points of favorite team
#' @param mu mean number of points of underdog team
#'
#' @description {The Skellam distribution for the difference of two independent
#' Poisson RVs is used to model point-spread in games with goals worth unit-points.}
#' @return numeric or vector
#' @export pskellam
pskellam <- function(k, lambda, mu)
{
  if(length(k) == 1)
  {
    # -100 should be a good lower bound. Higher causes precision loss complaints.
    sum(dskellam((-100):k, lambda, mu))
  } else if(length(k) > 1)
  {
    unlist(lapply(k, function(x) sum(dskellam((-100):x, lambda, mu))))
  }

}

#' Simulate variates of Skellam distribution i.e. difference of two independent Poisson RVs
#'
#' @param n number of variates to sample from the distribution
#' @param lambda mean number of points of favorite team
#' @param mu mean number of points of underdog team
#'
#' @description {The Skellam distribution for the difference of two independent
#' Poisson RVs is used to model point-spread in games with goals worth unit-points.}
#' @return numeric or vector
#' @export rskellam
rskellam <- function(n, lambda, mu)
{
  rpois(n, lambda)-rpois(n, mu)
}








