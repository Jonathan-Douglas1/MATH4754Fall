
ddt = read.csv("../../Labs/Lab1/DDT-1.csv")

table(ddt$RIVER, ddt$SPECIES)
#addmrgins(tab)

barplot(ddt$RIVER)

library(readxl)

ants <- read_excel("../../Data/data_for_course/Excel/GOBIANTS.XLS")

barplot(ants$AntSpecies)


