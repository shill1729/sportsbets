#' CF of a linear combination of Poisson RVs
#'
#' @param u real number argument
#' @param lambdas mean rates
#' @param points coefficients of points per goal
#'
#' @description {CF of a linear combination of Poisson variables representing
#' total points in a game.}
#' @return numeric
phi_X1 <- function(u, lambdas, points)
{
  return(exp(sum(lambdas*(exp(1i*u*points)-1))))
}

#' CF of a linear combination of Poisson RVs
#'
#' @param u real number argument
#' @param lambdas mean rates
#' @param points coefficients of points per goal
#'
#' @description {CF of a linear combination of Poisson variables representing
#' total points in a game.}
#' @return numeric
phi_X <- Vectorize(phi_X1, "u")



#' The PMF of a linear combination of Poisson RVs
#'
#' @param l integer argument, must be positive
#' @param lambdas mean rates
#' @param points coefficients representing points per goal
#' @param grid grid size for numeric integration
#' @param a interval bounds for the CF inversion integration
#'
#'
#' @description {Numeric integration of PMF of a linear combination of Poisson
#' RVs via inverting the CF using trapezoid rule.}
#'
#' @return numeric
dlcpois1 <- function(l, lambdas, points, grid = 1000, a=2)
{
  u <- seq(-a, a, length.out = grid)
  h <- diff(u)[1]
  x <- sum(c(1, rep(2, length(u)-2), 1)*exp(-1i*u*l)*phi_X(u, lambdas, points))*h/(4*pi)
  return(Re(x))
}

#' The PMF of a linear combination of Poisson RVs
#'
#' @param l integer argument, must be positive
#' @param lambdas mean rates
#' @param points coefficients representing points per goal
#' @param grid grid size for numeric integration
#' @param a interval bound for the CF integral inversion
#'
#'
#' @description {Numeric integration of PMF of a linear combination of Poisson
#' RVs via inverting the CF using trapezoid rule.}
#'
#' @return numeric
#' @export dlcpois
dlcpois <- Vectorize(dlcpois1, "l")


#' The CDF of a linear combination of Poisson RVs
#'
#' @param l integer argument, must be positive
#' @param lambdas mean rates
#' @param points coefficients representing points per goal
#' @param grid grid size for numeric integration
#' @param engine either "cf" or "mc"
#' @param a interval bound
#'
#'
#' @description {Numeric integration of CDF of a linear combination of Poisson
#' RVs via inverting the CF using trapezoid rule.}
#'
#' @return numeric
plcpois1 <- function(l, lambdas, points, grid = 300, engine = "cf", a=2)
{
  if(engine == "mc")
  {
    numGoals <- lapply(lambdas, function(lambda) stats::rpois(grid, lambda))
    r <- do.call(cbind, numGoals)
    ps <- apply(r, 1, function(x) t(points)%*%x)
    return(mean(ps <= l))
  } else if(engine == "cf")
  {
    x <- sum(dlcpois(0:l, lambdas, points, grid, a))
    return(x)
  }

}

#' The CDF of a linear combination of Poisson RVs
#'
#' @param l integer argument, must be positive
#' @param lambdas mean rates
#' @param points coefficients representing points per goal
#' @param grid grid size for numeric integration
#' @param engine either "cf" or "mc"
#' @param a interval bound for CF inversion integration
#'
#'
#' @description {Numeric integration of CDF of a linear combination of Poisson
#' RVs via inverting the CF using trapezoid rule.}
#'
#' @return numeric
#' @export plcpois
plcpois <- Vectorize(plcpois1, "l")


