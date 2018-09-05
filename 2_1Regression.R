# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading

# Regression & modelling

# Reference reading:
# www.zoology.ubc.ca/~schluter/R/fit-model/
# personality-project.org/r/r.lm.html
# www.pitt.edu/~njc23/Lecture10.pdf
# cran.r-project.org/doc/contrib/Faraway-PRA.pdf (ANOVA & Regression ind detail)

# The following books assume a fair level of statistical knowledge already:
# Crawley, M.J. (2005) Statistics: An Introduction using R. Wiley.
# Baayen, R.H. (2008) Analyzing Linguistic Data: A Practical Introduction to Statistics using R. Cambridge UP.

################ Regression & Modelling in R ########################

# Regression is where you'll see more explicit data modelling
# It is also the 'foundation' for ANOVA, so we will work on it first

# Model fitting = finding the 'least worst model'
# and optimising the fit of the model to the data
# you want the model that makes the data most likely

# NB: I'm not really covering how to fix problems with fitted models, just how to check for them
# See this webpage for a nice summary of assumptions and possible fixes
# people.duke.edu/~rnau/testing.htm
# note that the same assumptions apply for LMMs

##########  Better coding practice ########################
# so far we have been loading packages as we need them
# much better to have them all at the top of your script...
library(car)
library(effects)



##########  Model formulae in R ########################

#go back to our Homophone Data
str(Homophone_RT_Long)
head(Homophone_RT_Long)

# read through help to get an idea of options 
# and what is returned
help(lm)

# example with DV ~ IV
# predict RT by strength of muscle twitches
lm.1<-lm(RT ~ WordCondition, data=Homophone_RT_Long)
#note here that we have just put all data points through
#as though they are all independent (they are not)
summary(lm.1)

# see what else is in the output
names(lm.1)
# get fitted values
lm.1$fit
fitted.values(lm.1)
# get residuals (these are the differences between actual and fitted values)
head(lm.1$res)
head(residuals(lm.1))
# get confidence intervals for parameters (IVs) fitted in model
confint(lm.1)

#look at how R has built the regression model
str(model.matrix(lm.1))



##########  Model checks ########################
# see also: www.pitt.edu/~njc23/Lecture10.pdf

# produce diagnostic plots for model
plot(lm.mov.1)
plot.lm(lm.mov.1)
# Histograms of residuals from model (should be normal / evenly dispersed)
hist(resid(lm.1))

# tabulate influential/problematic data points
influence.measures(lm.mov.1)
# this places an asterisk next to each influential data point

# a way of selecting out the important bits / outliers / influential data points
inf<-influence.measures(lm.mov.1)
names(inf)  # let's look at what this function gives you
inf$is.inf
head(inf$is.inf)
# select those that are influential
which(apply(inf$is.inf, 1, any))     
# compare to graphs / influence.measures full table



# We know that RT is skewed, 
hist(Homophone_RT_Long$RT)
#so let's try with log RT to normalise the distribution
hist(log(Homophone_RT_Long$RT))

# predict log RT by strength of muscle twitches
lm.1log<-lm(log(RT) ~ WordCondition, data=Homophone_RT_Long)
summary(lm.1log)
# note the esimates are now in log units
plot(lm.1log)



##########  Building more complex models ########################
# Lets work with the movies data and build a more complex model

names(movies)
head(movies)
str(movies)

#Can you work out what this model is doing?
#What are we predicting? (i.e. what is the DV)
#What are the predictors (i.e. what are the IVs)

lm.mov.1 <- lm(profit ~ budget + year, data = movies)
summary(lm.mov.1)


#### INDEPENDENT PRACTICE ####
# Complete questions 1-2


# From the questions above we now have a set of nested models
# that is, models that are related and increase (or decrease in complexity)
# we can now compare these models to see which is a better fit to the data

#Is a model with cast facebook likes a better fit?
anova(lm.mov.1,lm.mov.2)

summary(lm.mov.1)
summary(lm.mov.2)  


#### INDEPENDENT PRACTICE ####
# Complete questions 3


### CLASS DISCUSSION
# What else should we add to this model?
# Do you think there will be any interactions?
# e.g. what does the following model do?
lm(profit ~ budget*genre + year + cast_facebook_likes, data = movies))

#For help, look here: https://faculty.chicagobooth.edu/richard.hahn/teaching/FormulaNotation.pdf
#what does the * do?

#Lets choose a model together, write it and fit it.

lm.mov.4 <- lm(profit ~ budget + year + cast_facebook_likes + genre + rating,
               data=movies)




#### INDEPENDENT PRACTICE ####
# Complete question 4

#We can see that many have outliers and there is generally 
#a long tail / non-normality in the profits

hist(movies$profit)



############### Regression Diagnostics #######################

# We will look at some further regression diagnostics
# open this webpage:
https://www.statmethods.net/stats/rdiagnostics.html

outlierTest(lm.mov.1) # Bonferonni p-value for most extreme obs
qqPlot(lm.mov.1, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(lm.mov.1) # leverage 


########## Multicollinearity and auto-correlation ########################

# Multi-collinearity
# When predictor variables are correlated, the estimated regression coefficient 
# of any one variable depends on which other predictor variables are 
# included in the model. That is, you can't get a true partial effect.
# see onlinecourses.science.psu.edu/stat501/node/82

# From the car package
vif(lm.mov.1) # variance inflation factors 
sqrt(vif(lm.mov.1)) > 2  # problem?


# Temporal correlation in errors
# First question: would you expect it for THIS data set?
# What data set WOULD you expect temporal correlation?

# Autocorrelation in the residuals distorts the regression statistics, 
# (e.g. if positive, will inflate F statistics by fitting small SEs).
# It suggests the model is missing a useful predictor variable 
# or it needs a time series component.
durbinWatsonTest(lm.mov.1)

#Interpretation:
https://en.wikipedia.org/wiki/Durbin%E2%80%93Watson_statistic
# d = 2 indicates no autocorrelation. 
# The value of d always lies between 0 and 4. 
# If the Durbin–Watson statistic is substantially less than 2, 
# there is evidence of positive serial correlation. 
# As a rough rule of thumb, if Durbin–Watson is less than 1.0, there may be cause for alarm.



############### Non-linearity in effects #######################
#Like correlation, the default for regression is fit a straight line
# relationships in your data may not be straight!

#component + residual plot 
crPlots(lm.mov.1)

####### Solutions to regression problems
# transform data
# remove outliers
# reduce number of predictors - look for correlations betwen them

#### INDEPENDENT PRACTICE ####
# Complete question 6 & 7



############### Effects package #########################

# Now we'll see an example of how R becomes a real pleasure

# Using the effects package
# NB: nice intro to this package quantoid.net/IntroR/Handout6_2012.pdf
# some examples below taken from this document

eff<-allEffects(lm.mov.3)

# make figure window bigger so you can see everything..
plot(eff)

#look at the components
names(eff)
#select one to look at
plot(eff$budget)
# Look at what else you get with this package output
names(eff$budget)
plot(eff$year, ylab = "Fitted profit $",xlab = "Year",main="")

####### bar plot with ggplot2  ####
require(ggplot2)
# set up data
eff$genre$variables$genre$levels
eff$genre$fit

temp<-unlist(eff$genre$variables$genre$levels)
temp<-as.data.frame(temp)
temp[,2]<-eff$genre$fit
names(temp)<-c("Genre","Profit")

# set up plot
plot <- ggplot(temp,aes(x=Genre,y=Profit))
plot <- plot + geom_bar(position=position_dodge(),stat="identity")
plot
plot<-plot + xlab("Movie Genre")
plot<-plot + ylab("Profit (fitted $)")
plot
#change orientation of the x axis
plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#these plots are ready to save and go into a document!


#### INDEPENDENT PRACTICE ####
# Complete question 8

with(movies, interaction.plot(genre, budget, profit, fun = mean))




################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")
