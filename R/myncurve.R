#' My Normal Curve
#'
#'@param a the probability that X is lesser or equal to a
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return the probability density that the value of a variable that follows the standard normal distribution is equal to x
#' @export
#'
#' @examples myncurve(mu=10,sigma=5, a=6)
myncurve = function(a, mu, sigma){
  #DELETE IF PROBLEMATIC
  x <- seq(-3, 3, length.out = 100)
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)

  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(-1000,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(-1000,xcurve,a),c(0,ycurve,0),col="Red")

  # Put in the text with the appropriate area

  # Area
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(-1000,mean=mu,sd=sigma)
  prob=round(prob,4)
  area=prob
  result <- list(mu = mu, sigma = sigma, area = area, a = a)
  return(result)
}
