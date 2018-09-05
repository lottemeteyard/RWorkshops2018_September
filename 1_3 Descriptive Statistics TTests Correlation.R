# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading


################## Descriptive Statistics ##########################
library(plyr)
# the summarise function when used with ddply can can flexibly generate summary data
# this is very useful when we want to complete further analysis

#For example, to get the average RT for each participant 
ddply(Homophone_RT_Long, "Participant", summarise,
      MeanRT = mean(RT, na.rm=TRUE))

#For example, to get the average RT for each participant for each WordCondition
ddply(Homophone_RT_Long, c("Participant","WordCondition"), summarise,
      MeanRT = mean(RT, na.rm=TRUE))

#For example, to get the average RT and sd for each participant for each WordCondition
ddply(Homophone_RT_Long, c("Participant","WordCondition"), summarise,
      MeanRT = mean(RT, na.rm=TRUE),
      SDRT = sd(RT, na.rm=TRUE)
      )

#Another way to generate summary data is to use the Psych package
# Install package 'Psych'
require(psych)
describeBy(Homophone_RT_Long$RT,list(Homophone_RT_Long$Participant,
                                     Homophone_RT_Long$WordCondition))

#### INDEPENDENT PRACTICE ####
# Complete questions 1-2


#### Psych package ####

# Let's look at Psych
help("psych")
# List all the functions
help(package="psych")

# Find the function 'describe'
help(describe)

# This function can be used to generate descriptive statistics for any grouping in our data.
# We are going to store it in an object 'temp'
temp <- describeBy(ToothGrowth,group=ToothGrowth$dose)

# notice that temp is under Values in the Environment window

head(temp)    #Check what it looks like
str(temp)     # Look at the nested structure

# Look at rownames 
# Try the following, note how sensitive R is to the double square brackets for lists
# This is because of that nested structure (found in Lists)
rownames(temp[1])
rownames(temp[[1]])
rownames(temp[[2]])

# Repeat the same function, but get a matrix, not a list as output
# We will overwrite the previous data
temp<-describeBy(ToothGrowth,group=ToothGrowth$dose,mat=TRUE)  
# we set mat=TRUE, so we get a matrix/dataframe as output, not a list

# notice that temp is now under data in the Environment window
head(temp)

# several functions have this flexibility with parameters you set to TRUE/FALSE etc.


################## T-tests ##########################
# T-tests (or their non-parametric equivalent) are used when you want to compare
# TWO conditions

#In the Homophone data set, we have the Homophone words and the Control words.
#This data was presented 'within subjects', so every participant saw 
#every item/condition

#First we need to get our summary data - i.e. average RT for each 
#participant for the two conditions in WordCondition


#### INDEPENDENT PRACTICE ####
# Complete question 3

#First lets check if the data is normally distributed

#use visual checks - often better than using formal tests, as you can see what 
#the deviation from normality looks like
hist(temp$MeanRT)
qqnorm(temp$MeanRT)
qqline(temp$MeanRT)

#plot them in the same window
par(mfrow=c(1,2))
hist(temp$MeanRT)
qqnorm(temp$MeanRT)
qqline(temp$MeanRT)

#return plotting window to single pane
par(mfrow=c(1,1))

#Can also do formal tests for normality
shapiro.test(temp$MeanRT)


#Boxplot can spot outliers
boxplot(temp$MeanRT)
#data looks ok, but there are some outliers we should be mindful of 
#(note this is typical in RT data)

#Boxplot can show us the data
boxplot(temp$MeanRT~temp$WordCondition)



#### Within subjects / paired t-test ####

#and the t-test..
t.test(temp$MeanRT~temp$WordCondition,paired=TRUE)

wilcox.test(temp$MeanRT~temp$WordCondition,paired=TRUE)

#t.test(temp$Control,temp$Homophone,paired=TRUE)

#boxplot (as above) but with labelled axes etc.
boxplot(temp$MeanRT~temp$WordCondition, main = "Homophone Vs Control RTs", 
        ylab = "Mean RT in ms",
        xlab = "Condition", names = c("Control", "Homophone"))



################## Between subject t-test ##############
# http://www.statmethods.net/stats/ttest.html

#Now we need to use some data where different participants were exposed to different things
#Look again at Mydata 
head(Mydata)
#'Task' was between subjects


#### INDEPENDENT PRACTICE ####
# Complete question 4

#Complete t-test between subjects
t.test(temp$MeanRT~temp$Task) 
#non-parametric test - not paired, must be dv~group
wilcox.test(temp$MeanRT~temp$Task)

#### INDEPENDENT PRACTICE ####
# Complete question 5

################## Correlation ##############
# http://www.statmethods.net/stats/correlations.html

#From Mydata, we are going to correlate the twitch caused by TMS
#with the participants' reaction time

#First generate some summary data for that.
names(Mydata)

#get participant average data first
temp <- ddply(Mydata, c("SubNo","Twitches"), summarise,
      MeanRT = mean(RT, na.rm=TRUE))
head(temp)
tail(temp)

#eyeball data with a scatterplot
plot(temp$MeanRT,temp$Twitches)

# get correlation coefficients
# spearman correlation coefficient for data that is not normally distributed
cor(temp$MeanRT,temp$Twitches, method="spearman", use="pairwise.complete.obs")
# pearson correlation coefficient for data that is normally distributed
cor(temp$MeanRT,temp$Twitches, method="pearson", use="pairwise.complete.obs")
# note the 'use' to specify what to do with missing data

# Get significance levels for correlations
library(Hmisc)
# for this function we need to use a numeric matrix not a dataframe
temp<-data.matrix(temp)

#NB: rcorr() uses pairwise deletion for missing values
rcorr(temp[,c(2:3)], type="spearman") 
#gives correlation coefficient, n and p values


#### INDEPENDENT PRACTICE ####
#taken from: https://www.datacamp.com/community/blog/r-correlation-tutorial
# Complete questions 6-8
# Intermediate - question 9


################## Correlation continued ##############
head(dat2)

# turn into data frame for easy management
dat2<-data.frame(dat2)
str(dat2)

#Data for age, height, shoesize & haircolour

#check normality
shapiro.test(dat2$AGE)
shapiro.test(dat2$HEIGHT)

#eyeball data and get correlation coefficient
plot(dat2$AGE,dat2$HEIGHT)

# note the 'use' to specify what to do with missing data
cor(dat2$AGE, dat2$HEIGHT, method="spearman", use="pairwise.complete.obs")

# Install package to help plot results for multiple correlations
library(corrplot)
# Asking for correlation between first three columns of data
M <- cor(dat2[,c(1:3)],method="spearman",use="pairwise.complete.obs")
M
#repeat those correlations, but plot with labelled axes
names(dat2)
rownames(M)<-names(dat2)[1:3]
colnames(M)<-names(dat2)[1:3]
corrplot(M, method = "number")

# make things a bit more jazzy
corrplot(M, order="AOE", addCoef.col="grey")

# Get significance levels for correlations
library(Hmisc)
# for this function we need to use a numeric matrix not a dataframe
temp<-data.matrix(dat2)

#NB: rcorr() uses pairwise deletion for missing values
rcorr(temp[,c(1:3)], type="spearman") 
#gives correlation coefficient, n and p values




################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")


