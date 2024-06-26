% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Title}
\usage{
ntickets(N = 400, gamma = 0.02, p = 0.95)
}
\arguments{
\item{N}{the number of seats on the flight}

\item{gamma}{the probability of overbooking}

\item{p}{the probability a passenger will show}
}
\value{
prints a named list containing nd, nc, N, p and gamma - where nd is calculated using the discrete distribution and nc is the same calculated with normal approximation and creates a plot of Objective function Vs n.
}
\description{
Title
}
\examples{
ntickets(N=400,gamma = 0.02, p = 0.95)
}
