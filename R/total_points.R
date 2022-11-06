#'  Model estimates of Total points distribution for two-teams
#'
#' @param line the line
#' @param means vector of the team-wise sum of the mean number of goals of each type
#' @param points the coefficients of points earned per goal, i.e. in basketball (2,3,1) (see details for orders)
#' @param method "cf" or "mc"
#' @param n number of variates to use in Monte-Carlo estimation.
#'
#' @description {Compute via Monte-Carlo probabilities of being over or under a given line.}
#' @details {We model the point process of a football team by a linear combination of Poisson RVs,
#' where the coefficients are the points earned for each type of goal. Since the data we use for the
#' mean number of touchdowns includes all touchdowns not just ones that have no conversion, the coefficients for the
#' the conversion points will be 1 and 2 rather than 7 and 8. I am not a football expert, so contact me if you have better ideas.}
#'
#' @return data.frame of over, under, and push chances.
#' @export total_points_chances
total_points_chances <- function(line, means, points, method = "cf", n = 10^3)
{
  over <- 1-plcpois(line, means, points, grid=n, engine = method)
  under <- plcpois(line-1, means, points, grid=n, engine = method)
  push <- dlcpois(line, means, points, grid=n)
  return(data.frame(over = over, under = under, push = push))
}
