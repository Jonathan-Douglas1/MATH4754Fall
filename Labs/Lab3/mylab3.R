## Task 1: Set and get the working directory
setwd("/Users/jonathandouglas/MATH4753/FALL244753doug0080/Labs/Lab3")
getwd()

## Task 2: Read in the data
library(Intro2R)

#Read in the spruce dataset
spruce = Intro2R::spruce

#Print the head of the spruce dataset
head(spruce)


## Task 3: Scatter plot of Height vs Breast Height
ggplot(spruce, aes(x = BHDiameter, y = Height)) + geom_point(pch = 21 ,bg = "blue", cex = 1.2) + xlim(0, 1.1 * max(spruce$BHDiameter)) +
  ylim(0, 1.1 * max(spruce$Height)) + xlab("Breast Height Diameter") + ylab("Tree Height") + ggtitle("Tree Height vs Breast Height Diameter")

#layout(matrix(c(1,2,3), 3, 1))
layout(matrix(c(1, 2, 3), 1, 3))
# This graph appears to have a roughly linear relationship.
trendscatter(spruce$BHDiameter, spruce$Height, f=0.5, main = "Height vs BHDDiameter, f=0.5")
trendscatter(spruce$BHDiameter, spruce$Height, f=0.6, main = "Height vs BHDDiameter, f=0.6")
trendscatter(spruce$BHDiameter, spruce$Height, f=0.7, main = "Height vs BHDDiameter, f=0.7")

#Create Linear Model Object
spruce.lm = with(spruce, lm(Height~BHDiameter))

#New Scatter plot with trend line
layout(matrix(c(1), 1, 1))
trendscatter(spruce$BHDiameter, spruce$Height, f=0.6, main = "Height vs BHDDiameter, f=0.6")
abline(spruce.lm)

