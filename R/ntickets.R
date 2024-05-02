#' Title
#'
#' @param N the number of seats on the flight
#' @param gamma the probability of overbooking
#' @param p the probability a passenger will show
#'
#' @return prints a named list containing nd, nc, N, p and gamma - where nd is calculated using the discrete distribution and nc is the same calculated with normal approximation and creates a plot of Objective function Vs n.
#' @export
#'
#' @examples ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets<-function(N=400,gamma = 0.02, p = 0.95) {

  # Discrete
  nD <- seq(N, floor(N + N/10), by = 1) # sequence n-values
  objectsD <- 1 - gamma - pbinom(N, size = nD, prob = p) # Complementary cumulative distribution function
  indexD <- which.min(abs(objectsD))
  nd <- nD[indexD] # assign to nd the smallest value in the sequence of the n-values in nD

  plot(nD, objectsD, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", nd, ")\n",
                    "gamma=", gamma, " N=", N, " discrete"))
  points(nD, objectsD, pch = 16, col = ifelse(nD == nd, "red", "blue")) #dots
  abline(h = objectsD[indexD], v = nd, col = "red")

  # Continuous
  nC <- seq(N, floor(N + N/10), by = 0.001) # sequence n-values
  objectsC <- 1 - gamma - pnorm(N + 0.5, mean = nC*p, sd = sqrt(nC*p*(1 - p)))
  indexC <- which.min(abs(objectsC))
  nc <- nC[indexC]


  plot(nC, objectsC, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", nc, ")\n",
                    "gamma=", gamma, " N=", N, " continuous"))
  abline(h = objectsC[indexC], v = nc, col = "blue")

  #print named list
  values <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(values)
}
