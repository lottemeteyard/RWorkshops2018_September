# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading


################## Chi Square ##########################
#For cases where you have categorical data
#That is, counts of how many things fall into different categories

library(MASS)
#look at data set
head(survey)
#We have Smoking frequency and Exercise level
levels(survey$Smoke)
levels(survey$Exer)

#show how many people fall into each - cross tabulate / cross table
table(survey$Smoke, survey$Exer) 

#We can look to see if there is an association between the two
temp <- table(survey$Smoke, survey$Exer) 

chisq.test(temp) 

#as there are small values (less than 5) for some categories
#we get a warning
#could collapse different categories to prevent this

#### INDEPENDENT PRACTICE ####
# Complete questions 1




