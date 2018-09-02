# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading

############ Learning statistics using R ########

# We can use R to better understand some common concepts
# For example, the normal distribution
# Go to the website below - it was created using R
# https://istats.shinyapps.io/NormalDist/

#What happens to the normal distribution when you
#Increase the mean?
#Decrease the mean?
#Increase the standard deviation?
#Decrease the standard deviation?

# Now lets create our own set of data
# I have the data from how everyone rated
# their experience with statistics (0-10):
# The numbers are:
# 3, 4, 1, 2, 5, 2, 5, 8, 6, 5, 3, 3, 2, 1, 2, 2

# We can put this into R:
exp <- c(3, 4, 1, 2, 5, 2, 5, 8, 6, 5, 3, 3, 2, 1, 2, 2)

# Then we can explore it:
plot(exp)

hist(exp)
boxplot(exp)
#Based on the histogram and boxplot, what do you expect the mean to be?
#What do you expect the standard deviation to be?
#Is the data normally distributed?
# Now check...
mean(exp)
median(exp)
sd(exp)


#Lets look at something more fun
help("ToothGrowth")
# Tooth growth in guinea pigs

str(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)
#len = tooth length
# supp = supplement given (Orange Juice or Ascorbic Acid)
# dose = dose of supplement given

boxplot(len ~ dose*supp, ToothGrowth)

plot(len ~ dose, ToothGrowth)
# Look at the boxplot
# Which of the combinations of dose and supplement 
# are more variable? Less variable?

# Check by calculating mean and standard deviation

subset(ToothGrowth, dose == 0.5)
subset(ToothGrowth, dose == 0.5 & supp == "OJ")

temp <- subset(ToothGrowth, dose == 0.5 & supp == "OJ")
mean(temp$len)
sd(temp$len)

temp <- subset(ToothGrowth, dose == 2.0 & supp == "OJ")
mean(temp$len)
sd(temp$len)

#Now - can you figure out how to change the above code
# so that you can compare the mean and standard deviation
# for the 1.0 dose with VC, to the 2.0 dose with VC?

#Is the data normally distributed?
#Take all the data
hist(ToothGrowth$len)
#or a subset
temp <- subset(ToothGrowth, dose == 2.0 & supp == "OJ")
hist(temp$len)

#Looking at the boxplot, do you think there will be an
#effect of dose on toothgrowth?
boxplot(len ~ dose, ToothGrowth)

#before we analyse the data, we need to specify participants/subject numbers
#Look at data set first
ToothGrowth
#Assume participants ordered in the data set, and number them
ToothGrowth$Subjects <- rep(1:10)

library(ez)
ezANOVA(ToothGrowth, len, wid = Subjects, within = dose)

#Note the warnings:
#We can change 'dose' to be a factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  
ezANOVA(ToothGrowth, len, wid = Subjects, within = dose)
#Can anybody see whether or not it is significant?

#tidy up
rm(temp, exp)


################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")


#Follow up/light reading:
#This website provides a nice introduction to different graphs
#https://www.shinyapps.org/apps/RGraphCompendium/index.php



