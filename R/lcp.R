# l <- 49
# n <- 5000
# lambdas <- c(10, 8, 3)
# points <- c(3, 2, 1)
# print(lambdas%*%points)
# a <- 3
# u <- seq(-a, a, length.out=n)
# g <- matrix(0, nrow =n)
# for(i in 1:n)
# {
#   f <- exp(1i*points*u[i])-1
#   g[i] <- exp(as.complex(lambdas%*%f)-1i*u[i]*l)
# }
# w <- c(1, rep(2, n-2), 1)
# h <- diff(u)[1]
# mm <- w%*%g*h/(4*pi)
# mm <- Re(mm)
#
# dlcpois_mc <- function(l, lambdas, points, n=5000)
# {
#   numGoals <- lapply(lambdas, function(lambda) stats::rpois(n, lambda))
#   r <- do.call(cbind, numGoals)
#   ps <- apply(r, 1, function(x) t(points)%*%x)
#   return(mean(ps == l))
# }
#
# print(mm)
# print(dpois(l, lambdas))
# print(dlcpois_mc(l, lambdas, points))
# print(dlcpois(l, lambdas, points, grid=n))
