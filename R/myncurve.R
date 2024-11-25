utils::globalVariables("x")
#Create the myncurve function, creates a normal curve
#' Title
#' @importFrom stats pbinom qbinom qnorm uniroot
#'
#' @param mu - the mean of the normal distribution
#' @param sigma  - the standard deviation of the normal distribution
#' @param a - the value below which to calculate the lower-tail probability
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve polygon text
#' @importFrom stats dnorm lm pnorm
#'
#'
#' @return - a list containing relavent variables and the probabilty of left tail distribution of a
#' @export

myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xlim = c(mu-3*sigma, mu + 3*sigma)

  #Probability polygon helper function
  probPoly <- function(xmin, mean, sd, color, winMin = FALSE, winMax = FALSE){
    #Since we will have multiple situations including lower tail probabilities and
    #probabilities of a range of values, we will overload the function so it can be used
    #in both cases.
    #First, if only one value is provided for the probability, use lowerTail to determine whether
    #to calculate the lower or upper tail probability
    #Find left tail probability
    prob = pnorm(xmin, mean, sd)

    #Make xmax be inputted probability (xmin)
    xmax = xmin
    #Make xmin be the left end of the window for polygon
    xmin = winMin


    #Round the probability to 4 decimal places.
    prob = round(prob, 4)

    #create list of values to plot under the curve
    xcurve = seq(xmin, xmax, length = 1000)
    #calculate y values for the curve
    ycurve = dnorm(xcurve, mean, sd)
    #Add the polygon to the graph
    polygon(c(xmin, xcurve, xmax), c(0, ycurve, 0), col = color)

    #Place labels of probability in the shaded areas.
    middle = (xmin + xmax) / 2

    #Control label height
    if (dnorm(middle, mean, sd) > 0.1){
      label_level = 0.5*dnorm(middle, mean, sd)
    } else {
      label_level = 0.5*dnorm(middle, mean, sd) + 0.05
    }
    text(x = middle, y = label_level, paste0("Prob: ", prob))
    return(prob)
  }

  #Store the returned probability from helper method.
  prob = probPoly(a, mu, sigma, color = "lightblue", winMin = xlim[1], winMax = xlim[2])

  #Create a named list with variables used and return this list
  toReturn = list(mu = mu, sigma = sigma, a = a, prob = prob)

  return(toReturn)
}
