
#Create the ntickets function
ntickets <- function(N, gamma, p){#N is number of seats on the flight,
  #Probability of show p, gamma is the allowed prob of an overbooking.
  #n is the number of tickets sold
  #The Discrete Method
  number_sold = N:(N + N*(2*(1 - p)))
  vector_of_shows = qbinom(1 - gamma, number_sold, p)

  indices = which(vector_of_shows <= N)

  #Find the minimum number of passengers where the upper limit would be exceeded
  max_tickets_sold = number_sold[max(indices)]

  #Now for the normal approximation
  n = N:(N + N*(2*(1 - p)))
  quad = qnorm(1 - gamma, n*p, sqrt(n*p*(1 - p))) - 0.5

  quad.op = optimize()
  return(max_tickets_sold)
}

ntickets(200, 0.02, 0.95)
ntickets(20000, 0.02, 0.95)

#Now for the normal approximation
N = 200
gamma = 0.02
p = 0.95

#n = N:(N + N*(2*(1 - p)))

quad = qnorm(1 - gamma, n*p, sqrt(n*p*(1 - p))) - 0.5



