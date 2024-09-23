library(Intro2R)
ddt = read.csv("DDT-1.csv")


ddt[ddt$WEIGHT > 500,]

#Question 1
ddt[1,2]

#Question 2
ddt[ddt$SPECIES == "LMBASS" & ddt$LENGTH > 30,]

#Question 3
ddt[ddt$LENGTH > 49 & ddt$LENGTH < 50,]

#Question 4
ddt[ddt$LENGTH > 49 & ddt$WEIGHT < 1200,]



#Second part of class
l <-ddt$LENGTH
z <- (l-mean(l))/sd(l)
z

#Find the possible outliers
l[abs(z) >= 2 & abs(z) <= 3]

#Find statistical outliers
l[abs(z) > 3]
