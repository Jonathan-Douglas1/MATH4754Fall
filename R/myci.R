#' Title
#' @importFrom stats qt sd
#' @param d - list of data
#' @param alpha - confidence interval
#'
#' @return - return the bounds
#' @export
#'
#' @examples
myci = function(d, alpha = 0.05){
  meanin = mean(d)
  std = sd(d)
  num = length(d)
  #Calculate the t value for alpha/2
  t = qt(p = 1 - alpha/2, df = num - 1)

  #Calculate the lower and upper bounds using the t value
  ci = c()
  ci[1] = meanin - t*std/sqrt(num)
  ci[2] = meanin + t*std/sqrt(num)

  #For lab visualization, print them as well
  cat("Lower Bound:", ci[1], "Upper Bound:", ci[2], "\n")

  #Return the intervals
  return(ci)
}
