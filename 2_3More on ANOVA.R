# R teaching Workshops.
# Lotte Meteyard, 2014, University of Reading
# School of Psychology and Clinical Language Sciences

############# More ANOVA #########

#the complexity of the analysis with ANOVA depends on the experimental design
#so it can be very simple, or very complex

# Let's ratchet up the complexity for the ANOVA model

#Let's return to our TMS data set
library(dplyr)
head(Mydata.corr.a)

names(Mydata.corr.a)
# Between: Task, Hemisphere (both were counterbalanced)
# Within: Axes, Twitches
# DV: RT


############# 2 way Between Subjects ANOVA #########
# Fit a between subjects ANOVA model with Task and Hemisphere only

#average
data.summary <- ddply(Mydata.corr.a, c("SubNo","Task", "Hemisphere"), summarise,
                      meanRT = mean(RT, na.rm=TRUE))

#Hemisphere is not very nicely coded here, 1=left, 2=right
#Let's relabel first, so that we can understand better what we get
library(dplyr)
data.summary$Hemisphere<-as.factor(data.summary$Hemisphere)
data.summary$Hemisphere <- revalue(data.summary$Hemisphere, c("1" = "Left", "2" = "Right"))

#note how this is coded with the * for main effects and interactions
aov.1 <- aov(log(meanRT) ~ (Task*Hemisphere), data=data.summary)
summary(aov.1)

#if we ONLY wanted the interaction, it would look like this:
aov.1 <- aov(log(meanRT) ~ (Task:Hemisphere), data=data.summary)
summary(aov.1)

# Print condition means or effects
model.tables(aov.1,"means") 
model.tables(aov.1,"effects")


# plot interactions for two factors from the above model
with(data.summary, interaction.plot(Task, Hemisphere, log(meanRT), fun = mean))


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
levels(Mydata.corr.a$TwitchFactor)


#generate participant averages to go into ANOVA
names(Mydata.corr.a)

data.summary <- ddply(Mydata.corr.a, c("SubNo", "Axes", "TwitchFactor"), summarise,
                      meanRT = mean(RT, na.rm=TRUE))
head(data.summary)

#check normality
hist(data.summary$meanRT)
shapiro.test(data.summary$meanRT)
#why do you think this is OK now?

#run within subjects ANOVA. Note the error structure for within subjects.
aov.2 <- aov(meanRT ~ (Axes*TwitchFactor) + Error(SubNo/(Axes*TwitchFactor)),data=data.summary)
summary(aov.2)
# plot interactions for two factors from the above model
with(data.summary, interaction.plot(Axes, TwitchFactor, meanRT, fun = mean))




############# Mixed ANOVA #########
#includes IVs that are within or between

#generate participant averages to go into ANOVA
names(Mydata.corr.a)
#TwitchFactor - within subjects
#Task - between subjects

data.summary <- ddply(Mydata.corr.a, c("SubNo", "Task", "TwitchFactor"), summarise,
                      meanRT = mean(RT, na.rm=TRUE))
head(data.summary)

shapiro.test(data.summary$meanRT)

aov.3 <- aov(meanRT ~ (TwitchFactor*Task)
             + Error(SubNo/(TwitchFactor)), data=data.summary)
#you must segregate between- and within-subjects variables
#above within-subjects factors are first.

with(data.summary, interaction.plot(Task, TwitchFactor, meanRT, fun = mean))


#### INDEPENDENT PRACTICE ####
# Complete questions 1-2



################## Important notes on ANOVA in R #########

# a.k.a all the things you never knew about ANOVA
# Short powerpoint presentation here to cover content

# Type III SS takes into account all other factors and interactions
# This is, it gives you the UNIQUE variance for each effect/interaction
# This approach is valid in the presence of significant interactions.
# When interactions are not present, Type II SS is a stronger test (see refs above)
# BUT typically in cognitive science we do Type III as a default

#Two packages allow you to do Type II and III
#the car package
#the ez package

###### car package #######
# install the car package
require(car)
help(Anova)

# you explicitly state the contrasts for each factor - that is, the comparisons you want to make
mod <- lm(meanRT ~ Task * Axes, data = data.summary, contrasts = list(Task = contr.sum, Axes = contr.sum))

#for contrasts see:
help("contr.sum")
# Sum contrasts (contr.sum): compares conditions to grand mean

# then an ANOVA test on that model

# Type II suitable for main effects
Anova(mod, type = 2)
# Type III suitable for main effects and interactions
Anova(mod, type = 3)

# You can see that Task is significant in both
# But with slight change in values 



###### ez package #######
#package to complete ANOVAs with Type II and III
#much friendlier interfact, especially for complex / mixed designs

require(ez)
#the ez package ('easy anova') is straightforward to use
??ezANOVA

#let's repeat one from above - between subjects first
data.summary <- ddply(Mydata.corr.a, c("SubNo","Task", "Hemisphere"), summarise,
                      meanRT = mean(RT, na.rm=TRUE))

aov.1 <- ezANOVA(data=data.summary, dv=meanRT, wid = SubNo, between = .(Task,Hemisphere))

#we get an error- it wants a subject to be a factor
#it also wants Hemisphere to be factor (its numbers still)

#### INDEPENDENT PRACTICE ####
# Complete question 3

#ez also has some nice functions to get summary data
summary.anova <- ezStats(data=data.summary, dv=meanRT, wid = SubNo, 
                         between = .(Task,Hemisphere))
print(summary.anova)


#### INDEPENDENT PRACTICE ####
# Complete question 4

ezPrecis(data.summary)
data.summary
data.summary<-data.summary[-141,]
#so with ezANOVA and ANOVA generally, need to be careful about missing data...


###### Multiple comparisons, contrasts and contrast coding #######
# So far we have looked at pairwise comparisons
# There are packages and defaults available for completing follow up tests
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





###### A more complex case: afex and lsmeans #######
#example adapted from: https://stats.stackexchange.com/questions/157022/testing-contrast-in-two-way-anova-using-multcomp

#Go back to our mixed ANOVA using default aov function in R
data.summary <- ddply(Mydata.corr.a, c("SubNo", "Task", "Axes"), summarise,
                      meanRT = mean(RT, na.rm=TRUE))

mix.aov.1 <- aov(meanRT ~ (Task*Axes)
             + Error(SubNo/(Axes)), data=data.summary)


#to get post-hoc comparisons...
glht(mix.aov.1, linfct = mcp(Task = "Tukey"))
glht(mix.aov.1, linfct = mcp(Axes = "Tukey"))
#notice that it throws up an error - this is because we have a within subjects manipulation
#which means that we get an aov.list output, which multcomp doesn't like
#(and also Tukey is only for between subjects...)

#so instead we will use some other functions!
require(lsmeans)
require(afex)
??afex  #yet another option for doing ANOVA in R

mix.anova <- aov_car(meanRT ~ Axes * Task + Error(SubNo/Axes), 
                     data=data.summary)

# Set up complete set of cell means (i.e. all possible combinations of variables)
ref1 <- lsmeans(mix.anova, c("Axes", "Task"))
#see that this gives us an estimate of the mean (lsmean) for each cell
ref1
str(ref1)

#Lets look at the interaction plot to find something to compare
with(data.summary, interaction.plot(Task, Axes, meanRT, fun = mean))

#set up contrasts of interest: 4x2 = 8 total cells / means
unique(data.summary$Task)
unique(data.summary$Axes)
#e.g. 
rep(0,8)

#make sure you give it a sensible name
#refer back to full output for ref1 to get the right order
c_list <- list(
  "E.W.CRT - N.S.CRT" = c(1, -1, 0, 0, 0, 0, 0, 0),
  "E.W.CRT - E.W.Flanker" = c(0, 0, 0, 0, 0, 0, 0, 0))  #where should I put the 1s?
summary(contrast(ref1, c_list), adjust = "holm")

#OR can now combine with functions in multcomp
summary(as.glht(contrast(ref1, c_list)), test = adjusted("bonferroni"))

#reminder: an easy way to get the right number of zeros is to do... rep(0,16)
#e.g.
x<-rep(0,16)
x[1]<- 1
x[2]<- -1
"E.W.CRT.Left - N.S.CRT.Left" = x   #see where this has appeared in Environment



###### INDEPENDENT PRACTICE #######

# Complete questions 5-7





#### Graphing Factorial Data - more than one IV ####

#a quicker way to generat the summary data is to use the lsmeans function
ref1 <- lsmeans(mix.anova, c("Task", "Congruence"))
#it gives us the mean for each condition, and the SE / CI so we can plot
ref1
names(ref1)
str(ref1)

summary(ref1)
data.summary<-summary(ref1)
names(data.summary)

#specify data
plot <- ggplot(data.summary,aes(x=Task,y=lsmean,fill=Congruence))
#specify plot type
plot <- plot + geom_bar(position=position_dodge(),stat="identity")
plot

# add error bars with standard error/CI
plot <- plot + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL,width=.2))
#error bars int the wrong place, so shift them with position
plot <- plot + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL,width=.2),
                             position=position_dodge(.9))


#### INDEPENDENT PRACTICE ####
#complete question 8









####### weighted means example - why different SS matter #######

# Lets work through an example
# From www.personal.psu.edu/mar36/stat_461/unbalanced/unbalanced_two_factor_ANOVA.html

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



