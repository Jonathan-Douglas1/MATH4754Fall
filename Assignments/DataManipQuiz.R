#Here is a script for the Data Manipulation quiz.
setwd("/Users/jonathandouglas/MATH4753/FALL244753doug0080/Labs/Lab1")#This is WD because data is here
getwd()

#Read in the data
ddt <-read.csv("DDT-1.csv")

head(ddt, n=3)

ddt["LENGTH"]#Very cool! Pulls off the column

#Lets try again with the c function, pulls two columns
ddt[c("LENGTH", "WEIGHT")]

ddtcat <- ddt[ddt$SPECIES == "CCATFISH",]
ddtcat

names(ddt)

#Print the summary
summary(ddt)#BOOM

#COOL SUBSET
ddt[ddt$LENGTH > 48 & ddt$WEIGHT > 1200,]

#Another one
ddt[ddt$SPECIES == "LMBASS" | ddt$RIVER == "TRM",]
