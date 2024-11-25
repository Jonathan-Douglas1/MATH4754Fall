#Create the ntickets function
#' ntickets
#'
#' This function takes a number of seats N, a tolerated probability of a overbooking gamma, and
#' probability of passengers showing p, to find the discrete and continuous optimized number of
#' tickets to sell.
#'
#' @author Jonathan Douglas
#'
#' @param N - the number of seats on the flight
#' @param gamma - the accepted probability that a flight will be overbooked
#' @param p - probability of a passenger showing
#'
#' @export
#'
ntickets <- function(N, gamma, p){#N is number of seats on the flight,
  #Probability of show p, gamma is the allowed prob of an overbooking.
  #n is the number of tickets sold
  #The Discrete Method
  #Find a range of discrete values that scales with the value of N
  number_sold = N:(N + ceiling(N*(2*(1 - p))))
  vector_of_shows = qbinom(1 - gamma, number_sold, p)

  #What indicies of the number of people who show up are less than the number of seats
  indices = which(vector_of_shows <= N)

  #Find the maximum number of passengers before the seat limit would be exceeded.
  nd = number_sold[max(indices)]

  #Now for the normal approximation
  #use the uniroot function
  qnorm_function = function(n){
    #Endpoint adjustment is added here
    num = qnorm(1 - gamma, n*p, sqrt(n*p*(1 - p))) - 0.5

    return(num)
  }

  #This function helps find where the qnorm function becomes larger than the number of seats
  find_zero = function(x){
    return(qnorm_function(x) - N)
  }

  #Find the value of x where the find_zero function = 0
  nc = uniroot(find_zero, lower = N - N*0.1, upper = N + N*0.1)$root

  #Plot the discrete and continuous plots
  #For the discrete plot
  vector_of_probs = 1 - pbinom(N, number_sold, p)
  #Create graph for discrete function
  plot(number_sold, vector_of_probs - gamma, type = "b", col = "black", lwd = 2, pch = 16,
       main = paste0("Objective Vs n to find optimal tickets sold", '\n', "(", nd, ") gamma= 0.02 N=200 discrete"),
       xlab = "n", ylab = "Objective")

  #Add the lines to the graph to show x and y values
  abline(v = nd, col = "red")
  abline(h = 0, col = "red")

  #For the continuous plot
  pnorm_function = function(n){
    num = 1 - pnorm(N + 0.5, n*p, sqrt(n*p*(1 - p)))

    return(num)
  }

  #Create graph for the continuous functions
  curve((pnorm_function(x) - gamma), from = N, to = (N + N*(2*(1 - p))), col = "blue", lwd = 2,
        main = paste0("Objective Vs n to find optimal tickets sold", '\n',"(", round(nc, 5), ") gamma= 0.02 N=200 continuous"),
        xlab = "n", ylab = "Objective")

  #Add the lines to the graph to show x and y values
  abline(v = nc, col = "red")
  abline(h = 0, col = "red")

  #Create a named list to return to the console.
  return_list = list(nc = nc, nd = nd, N = N, p = p, gamma = gamma)

  #Print the list
  print(return_list)
}
