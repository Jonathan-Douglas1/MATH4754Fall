library(Intro2R)
ddt = read.csv("../FALL244753doug0080/Labs/Lab1/DDT-1.csv")

head(ddt, 3)

#Find weight that are considered outliers
weight = ddt$WEIGHT

3*sd(weight) + mean(weight)

#What are the possible values
z = (weight - mean(weight)) / sd(weight)

weight[abs(z) >= 2 & abs(z) <= 3]

#What is SD of Z (1 Always)
sd(z)

##In class
v <- Intro2R::myreadxl("/Users/jonathandouglas/MATH4753/FALL244753doug0080/DataForClass/Excel/")#This reads in all excel files
#Uhh fizxx
tab <- table()
