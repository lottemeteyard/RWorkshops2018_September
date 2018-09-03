# R teaching Workshops.
# Lotte Meteyard, 2014, University of Reading
# School of Psychology and Clinical Language Sciences

############# More ANOVA #########

#the complexity of the analysis with ANOVA depends on the experimental design
#so it can be very simple, or very complex

############# Mixed ANOVA #########


# Let's ratchet up the complexity for the ANOVA model

#Let's return to our TMS data set
library(dplyr)
print(tbl_df(Mydata.corr.a), n = 3)

names(Mydata.corr.a)
# Between: Task, Hemisphere (both were counterbalanced)
# Within: Axes, Twitches
# DV: RT


############# 2 way Between Subjects ANOVA #########
# Fit a between subjects ANOVA model with Task and Hemisphere only
# As in SPSS we are going to look at main effects AND interactions

#note how this is coded with the * for main effects and interactions
aov.1 <- aov(log(RT) ~ (Task*Hemisphere), data=Mydata.corr.a)
summary(aov.1)

#if we ONLY wanted the interaction, it would look like this:
aov.1 <- aov(log(RT) ~ (Task:Hemisphere), data=Mydata.corr.a)
summary(aov.1)

# Print condition means or effects
model.tables(aov.1,"means") 
model.tables(aov.1,"effects")


# plot interactions for two factors from the above model
with(Mydata.corr.a, interaction.plot(Task, Hemisphere, log(RT), fun = mean))


############# 2 way Within Subjects ANOVA #########
# Fit a within subjects ANOVA with Axes and Twitches only

# First check the data.
# Axes was manipulated by the experimenter (it was the direction of the TMS coil)
# Twitches was a rating provided by the participants

temp<-dcast(Mydata.corr.a, SubNo ~ Twitches, value.var = "RT",
            fun.aggregate = mean, na.rm = TRUE)

temp

#there are lots of NAs/missing data - some participants did not provide
#a rating of e.g. 7, 8, 9, 10

# Therefore, going to recode Twitches as a binary variable low vs high
library(plyr)
Mydata.corr.a$TwitchFactor<-revalue(as.character(Mydata.corr.a$Twitches), 
                                  c("0" = "Low",
                                    "1" = "Low", "2" = "Low",
                                    "3" = "Low", "4" = "Low",
                                    "5" = "Low", "6" = "High",
                                    "7" = "High", "8" = "High",
                                    "9" = "High", "10" = "High"))
str(Mydata.corr.a$TwitchFactor)
#change to factor
Mydata.corr.a$TwitchFactor<-as.factor(Mydata.corr.a$TwitchFactor)
str(Mydata.corr.a$TwitchFactor)

#run within subjects ANOVA. Note the error structure.
aov.2 <- aov(log(RT) ~ (Axes*TwitchFactor) + Error(SubNo/(Axes*Twitches)),data=Mydata.corr.a)
summary(aov.2)
# plot interactions for two factors from the above model
with(Mydata.corr.a, interaction.plot(Axes, TwitchFactor, log(RT), fun = mean))




############# Mixed ANOVA #########


# Extend the model to be a mixed ANOVA with all IVs.

aov.3 <- aov(log(RT) ~ (Axes*TwitchFactor*Task*Hemisphere)
             + Error(SubNo/(Axes*TwitchFactor)), data=Mydata.corr.a)
#you must segregate between- and within-subjects variables
#above within-subjects factors are first.



#### INDEPENDENT PRACTICE ####
# Complete questions 1-2



################## Important notes on ANOVA in R #########

# a.k.a all the things you never knew about ANOVA
# Short powerpoint presentation here to cover content

# Lets work through an example
# From www.personal.psu.edu/mar36/stat_461/unbalanced/unbalanced_two_factor_ANOVA.html


####### weighted means example #######

unb<-read.csv("unbalanced.csv")
str(unb)
# Data is rate of bone growth affected by hormone administration
# bone development is mild, moderate or severe

# plot the data
with(unb, interaction.plot(gender, boneDev, growth, fun = mean))

table(unb$gender,unb$boneDev)  #Frequency table to show cells are unbalanced
#there are more data points for Female (F) than male in Med/Mild, and less for Severe

# individual cell means
with(unb, tapply(growth, boneDev:gender, mean))

# Weighted mean calculation using tapply for levels of bone development
# It weights towards female scores when F > M, and vice versa
with(unb, tapply(growth, boneDev, mean))
#or
mean(unb$growth[unb$boneDev=="Sev"])

# Compare to unweighted means for Sev level of boneDev
#Get data for F
unb$growth[unb$gender=="F" & unb$boneDev=="Sev"]
#get mean for F (only one data point)
2.4
#Get data for M
unb$growth[unb$gender=="M" & unb$boneDev=="Sev"]
#get mean for M (three data points)
(1.4 + 2.4 + 2.2)/3

#overall unweighted mean (i.e. take mean from F and M)
(2.4 + 2)/2
#compare to weighted mean (effectively pools data)
mean(unb$growth[unb$boneDev=="Sev"])
#see that weighted mean is lower, as it has bias towards lower value for M, 
#which had more data points


#Now we will see the implications for this in ANOVA
####### implemented in aov (Type I SS)

# Test boneDev alone, and then gender after boneDev
aov.1 <- aov(growth ~ boneDev * gender, data = unb)
summary(aov.1)

# Test gender alone, and then boneDev after gender
aov.2 <- aov(growth ~ gender * boneDev, data = unb)
summary(aov.2)
# we see different results depending on the order of entry
 

####### Type II and Type III SS

# Type II is best used when there is no interaction, 
# it tests each factor after the others, but ignores any interactions
# that is, it assumes any higher order interactions are zero
# see goanna.cs.rmit.edu.au/~fscholer/anova.php

# Type III SS takes into account all other factors and interactions
# This is, it gives you the UNIQUE variance for each effect/interaction
# This approach is valid in the presence of significant interactions.
# NB: in the presence of interactions, main effects are rarely interpretable
# When interactions are not present, Type II SS is a stronger test (see refs above)

#Two packages allow you to do Type II and III
#the car package
#the ez package

###### car package #######
# install the car package
require(car)
help(Anova)

# you explicitly state the contrasts for each factor
mod <- lm(growth ~ gender * boneDev, data = unb, contrasts = list(gender = contr.sum, boneDev = contr.sum))

#for contrasts see:
help("contr.sum")
# Sum contrasts (contr.sum): compares conditions to grand mean

# then an ANOVA test on that model

# Type II suitable for main effects
Anova(mod, type = 2)
# Type III suitable for main effects and interactions
Anova(mod, type = 3)

# You can see that boneDev is significant in both
# But more so in Type II 

# So, for unbalanced designs (e.g. missing values in repeated measures)
# it may be sensible to use Type III
# see onlinestatbook.com/2/analysis_of_variance/unequal.html
# for a more detailed discussion of when to use Type II

#NB more notes and practice code for car are provided at the end



###### ez package #######
#package to complete ANOVAs with Type II and III
require(ez)
#the ez package ('easy anova') is straightforward to use
??ezANOVA


#let's repeat one from above - between subjects
unb.anova <- ezANOVA(data=unb, dv=growth, between = .(gender,boneDev))

#we get an error- it wants a subject/participant column
#let's make one
unb$subj <- c(1:length(unb$gender))

unb.anova <- ezANOVA(data=unb, dv=growth, wid = subj, between = .(gender,boneDev))
#look how it picks up the unbalanced data, and prompts for a type argument
#look it also converts the subj data to a factor

unb.anova <- ezANOVA(data=unb, dv=growth, wid = subj, between = .(gender,boneDev),
                     type=3)
unb.anova  #notice that it automatically tests the interactions



#let's repeat one from above - 1x4 within subjects
names(Mydata.corr.a)

#summary data using reshape2 melt/cast
temp<-dcast(Mydata.corr.a, SubNo ~ Axes, value.var = "RT",
            fun.aggregate = mean, na.rm = TRUE)
data.summary<-melt(temp, id.vars = "SubNo")
head(data.summary)
tail(data.summary)
names(data.summary)[2:3]<-c("Axes","meanRT")

#convert to factor for ANOVA
data.summary$Axes<-as.factor(data.summary$Axes)

with.anova <- ezANOVA(data=data.summary, dv = log(meanRT), wid = SubNo, 
                      within = Axes,
                      type=3)
with.anova

#generate summary statistics for this same ANOVA model
#this gives you mean RTs broken down by cells of the design
summary.anova <- ezStats(data=data.summary, dv = log(meanRT), wid = SubNo, 
                      within = Axes)
print(summary.anova)


#### INDEPENDENT PRACTICE ####
# Complete questions 4-5






###### Multiple comparisons, contrasts and contrast coding #######
# There are packages and defaults available for complete follow on tests
# e.g. see the 'contrasts' package for more options with lm fits
# and www.ats.ucla.edu/stat/r/modules/dummy_vars.htm

# For a good introduction to principles, 
# see: Crawley, M.J. (2005) Statistics: an introduction using R

# example data from this book:
load("ContrastData.RData")
# Computer experiment with 5 conditions and a response variable
# One condition acts as a control here.
names(compexpt)
levels(compexpt$Condition)
plot(compexpt$Condition,compexpt$Resp)

# Treatment contrasts: default in R
# compares conditions to a control NB: control = first alphabetically
options(contrasts=c("contr.treatment","contr.poly")) 
mod<-lm(Resp~Condition,data=compexpt)
summary(mod)
# First coefficient is the mean of the factor level that comes first in 
# alphabetical order (Control mean in this example). 
# The remaining parameters are all differences between means 
# (the mean in question compared with the control mean). 
# So here we see the increase in Resp for each condition > control


#package multcomp is very helpful
require(multcomp)
help("glht")

#go back to one anova above (line 288)
# ombnibus ANOVA
mod<-aov(Resp~Condition,data=compexpt)
summary(mod)

glht(mod, linfct = mcp(Condition = "Tukey"))
#notice that this gives you ALL possible comparisons
summary(glht(mod, linfct = mcp(Condition = "Tukey")))  #summary to get sig tests

#can also specify specific comparisons
#first need to know the size of the model/design
temp <- glht(mod, linfct = mcp(Condition = "Tukey"))
head(temp)
#look directly at comparisons being made
temp$linfct
dim(temp$linfct) #size of comparison set: Intercept and 4 conditions

#set up specific comparisons
contr <- rbind("A1 - control" = c(0, -1, 1, 0, 0),
               "A1 - A2" = c(0, 0, 1, -1, 0))
glht(mod, linfct = mcp(Condition = contr))
# check if that looks right
plot(compexpt$Condition,compexpt$Resp)

#easier way to set up contrast matrices
table(compexpt$Condition) #get sample sizes for each condition
#automatically generate contrast matrix across each condition
help("contrMat")
#this function comes with Multcomp package
contrMat(table(compexpt$Condition)) #note the defaults it has created
contrMat(table(compexpt$Condition),type="AVE") #compare each condition to average of others

contr = contrMat(table(compexpt$Condition),type="AVE") #compare each condition to average of others
glht(mod, linfct = mcp(Condition = contr))

# Brief powerpoint presentation here




###### A more complex case: afex and lsmeans #######
#example adapted from: https://stats.stackexchange.com/questions/157022/testing-contrast-in-two-way-anova-using-multcomp

#In default aov function in R
aov.3 <- aov(log(RT) ~ (Axes*Task*Hemisphere)
             + Error(SubNo/(Axes)), data=Mydata.corr.a)

#and then using ezANOVA
mix.anova <- ezANOVA(data=Mydata.corr.a, dv = log(RT), wid = SubNo, 
                     within = .(Axes),
                     between = .(Task,Hemisphere),
                     type=3,
                     return_aov = TRUE)

#to get post-hoc comparisons...
head(mix.anova) #see where the 'aov' object is
glht(mix.anova$aov, linfct = mcp(Axes = "Tukey"))
#notice that it throws up an error - this is because we have a within subjects manipulation
#which means that we get an aov.list output, which multcomp doesn't like
#(and also Tukey is only for between subjects...)

#so instead we will use some other functions!
require(lsmeans)
require(afex)
??afex  #yet another option for doing ANOVA in R

mix.anova <- aov_car(log(RT) ~ (Axes*Task*Hemisphere) + Error(SubNo/(Axes)), 
                     data=Mydata.corr.a, return = "aov")
#note that it tells you what default contrasts it is using for between subject variables
#note also that it gives you the warning re: averaging for the data

# Set up complete set of cell means (i.e. all possible combinations of variables)
ref1 <- lsmeans(mix.anova, c("Axes", "Task", "Hemisphere"))
#see that this gives us an estimate of the mean (lsmean) for each cell
ref1
str(ref1)

#set up contrasts of interest: 4x2x2 = 16 total cells / means
unique(Mydata.corr.a$Task)
unique(Mydata.corr.a$Axes)
unique(Mydata.corr.a$Hemisphere)

#make sure you give it a sensible name
#refer back to full output for ref1 to get the right order
c_list <- list(
  "E.W.CRT.Left - N.S.CRT.Left" = c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "E.W.CRT.Left - E.W.CRT.Right" = c(1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0))
summary(contrast(ref1, c_list), adjust = "holm")

#OR can now combine with functions in multcomp
summary(as.glht(contrast(ref1, c_list)), test = adjusted("bonferroni"))

#NB: an easy way to get the right number of zeros is to do... rep(0,16)
#e.g.
x<-rep(0,16)
x[1]<- 1
x[2]<- -1
"E.W.CRT.Left - N.S.CRT.Left" = x   #see where this has appeared in Environment



###### INDEPENDENT PRACTICE #######

# Complete questions 6-7








###### For further practice #######

###### car package Anova - repeated measures  #######
#From:
#http://rtutorialseries.blogspot.co.uk/2011/02/r-tutorial-series-one-way-repeated.html
#http://rtutorialseries.blogspot.co.uk/2011/02/r-tutorial-series-two-way-repeated.html

#If we want Type II or III for a repeated measures design...
#and the car package and Anova() function

#the function looks like this: Anova(mod, idata, idesign)
#mod = linear model
#idata = gives the structure for the analysis
#idesign = gives the design

#Let's go back to our original 1-way data set (lines 150-152 script 2_2ANOVA)
temp<-dcast(Mydata.corr.a, SubNo ~ Axes, value.var = "RT",
            fun.aggregate = mean, na.rm = TRUE)
#set/get columns of data for each factor level
#cbind gives a list of numbers
axesBind <- cbind(temp$E.W,temp$N.S,temp$NE.SW,temp$NW.SE)
str(axesBind)

#Generate linear model for the data
mod <- lm(axesBind ~ 1)

#Create a factor variable that codes the levels of the IV
axesFactor <- as.factor(c("E.W", "N.S", "NE.SW", "NW.SE"))
#save as a data.frame separately
axesFrame <- data.frame(axesFactor)

analysis <- Anova(mod, idata = axesFrame, idesign = ~axesFactor)
summary(analysis)


###### Let's repeat this with a more complex Repeated measures ANOVA
#Go back to our 4x2 for Axes and TwitchFactor

#summarise data with separate columns for each repeated measures factor
temp<-dcast(Mydata.corr.a, SubNo ~ Axes + TwitchFactor, value.var = "RT",
            fun.aggregate = mean, na.rm = TRUE)
names(temp)
#need to tidy up - note the NAs (so it is unbalanced)
temp<-temp[,-8]

#Bind data together as before
axesTwitchBind <- cbind(temp$E.W_High,temp$E.W_Low,
                        temp$N.S_High,temp$N.S_Low,
                        temp$NE.SW_High,temp$NE.SW_Low,
                        temp$NW.SE_High,temp$NW.SE_Low)
str(axesTwitchBind)

#Create dataframe with experimental design
#we have 4 levels of axes x 2 levels of TwitchFactor
axesTwitchFrame <- data.frame(matrix(data=NA,nrow=8,ncol=2))

#make sure we mirror the way the columns are arranged
names(temp)
#we are going to use strsplit to help us
strsplit(names(temp)[2],"_")
x<-strsplit(names(temp)[2],"_")
x[[1]]
axesTwitchFrame[1,1:2]<-x[[1]]

#Do it all in one go
for (i in 1:8)  
{x<-strsplit(names(temp)[i+1],"_")    
axesTwitchFrame[i,1:2]<-x[[1]]  }
axesTwitchFrame
names(axesTwitchFrame)[1:2]<-c("Axes","TwitchFactor")

#Now run Anova
#linear model
mod <- lm(axesTwitchBind ~ 1)

#create factors for idata/idesign part so can complete contrasts properly
axesTwitchFactor<-axesTwitchFrame
axesTwitchFactor$Axes <- as.factor(axesTwitchFrame$Axes)
axesTwitchFactor$TwitchFactor <- as.factor(axesTwitchFrame$TwitchFactor)

#Anova
analysis <- Anova(mod, idata = axesTwitchFactor, idesign = ~Axes*TwitchFactor)







