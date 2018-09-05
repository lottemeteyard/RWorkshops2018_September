# R teaching Workshops.
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading

################## ANOVA #########

#ANOVA is used widely in Psychology/linguistics/cognitive science
#This is because it is used to analyse FACTORIAL experiments

#As for t-test, the critical distinction in ANOVA is which conditions
#are WITHIN subjects, and which are BETWEEN subjects

#First the powerpoint to go through ANOVA basics in R

#Load up packages first
require(car)
require(reshape2)
require(multcomp)
require(ggplot2)

################## 1 way ANOVA #########

# We will use the homophone data  
#Compare RT differences for different TMS coil orientations

#Data is from here: tms-smart.info

#First we need to select only the correct trials
names(Mydata)
head(Mydata)

names(Mydata)


library(dplyr)
#select only trials where people responded correctly
Mydata.corr<-filter(Mydata, ACC == 1)
head(Mydata.corr)

str(Mydata.corr)
summary(Mydata.corr)

# Let's first check the data meets assumptions for ANOVA

##### Homogeneity of variance tests ####

# Two tests come in the base package

# (1) Bartlett Test
# checks for k samples having equal variances
# sensitive to departues from normality
# so may just show non-normality
#check for each condition level in Axes
bartlett.test(RT ~ Axes,data=Mydata.corr)
#check by subject
bartlett.test(RT ~ SubNo,data=Mydata.corr)

# (2) Figner-Killeen Test of Homogeneity of Variances
# Very robust against departures from normality
# non-parametric test
fligner.test(RT ~ Axes,data=Mydata.corr)

# One familiar from SPSS:
# (3) Levene's test can be found in the package car
# so install car

leveneTest(RT ~ Axes,data=Mydata.corr)



##### Normality ####

# plot normality formally
qqnorm(Mydata.corr$RT)
qqline(Mydata.corr$RT)

hist(Mydata.corr$RT) #can see the long tail / skew that is typical for RT data

shapiro.test(Mydata.corr$RT)
#what error do we get?

# See also:
# www.dummies.com/how-to/content/how-to-test-data-normality-in-a-formal-way-in-r.html
# tests null of whether sample comes from normal distribution



############# Transform data, remove outliers #########

# Log transform usually good to normalise RTs
par(mfrow=c(1,2))
hist(Mydata.corr$RT)
hist(log(Mydata.corr$RT))

# quantile plots show less deviation
qqnorm(Mydata.corr$RT)
qqline(Mydata.corr$RT)
qqnorm(log(Mydata.corr$RT))
qqline(log(Mydata.corr$RT))

par(mfrow=c(1,1)) #put plot window back to single pane




# Still have some outliers at the extreme end

#Look for outliers in DV

#Boxplot by Subject for outliers, store results
mybp <- boxplot(RT ~ SubNo, data=Mydata.corr)

#The following is quite complex, so follow along now
#come back to this if you need it in future

#identify outliers row number in original data set
x<-which(Mydata.corr$RT %in% mybp$out, arr.in=TRUE)
head(x)
length(x)

#create new column to store coding for outliers
Mydata.corr$Out<-rep("FALSE")

#loop to identify rows and put 'TRUE' for each outlier
for (i in 1:length(x))
{Mydata.corr$Out[x[i]]<-"TRUE"}
#make new column a factor/category
Mydata.corr$Out<-as.factor(Mydata.corr$Out)
head(Mydata.corr)
Mydata.corr$Out

#look at number of outliers per subject
table(Mydata.corr$SubNo,Mydata.corr$Out)

#new data set with outliers removed 
Mydata.corr.a<-filter(Mydata.corr,Out==FALSE)

#Calculate percentage of data lost
1-dim(Mydata.corr.a)/dim(Mydata.corr) #is about 4%
#The less you lose the better


# finally, plot log transform of data with outliers removed
par(mfrow=c(1,2))
hist(log(Mydata.corr.a$RT))
qqnorm(log(Mydata.corr.a$RT))
qqline(log(Mydata.corr.a$RT))
# much better

#save workspace here for backup
#session - save workspace
#or
save.image(file="Workshop_workspace.RData")



#### INDEPENDENT PRACTICE  ####
# Complete question 1





############# One way ANOVA (between & within subjects) #########

# It's best to store output from tests into variables

# ANOVAs in R can be tricky.
# But, it will make you understand ANOVA better.
#http://myowelt.blogspot.co.uk/2008/05/obtaining-same-anova-results-in-r-as-in.html

# Between-subjects ANOVA is straightforward in R, performing repeated measures (within-subjects)
# ANOVA is not so obvious. 
#  e.g https://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/

# ANOVA is GLM / linear models with factorial IVs only, so in R it is coded like a regression
# hence the formuala entry format

#average across subjects so that ANOVA done on means, as is typical

data.summary <- ddply(Mydata.corr.a, c("SubNo","Axes"), summarise,
      meanRT = mean(RT, na.rm=TRUE))


#look at the average data for each condition
with(data.summary, tapply(log(meanRT), Axes, mean))

#We can start with a linear model 
first.lm <-lm(log(meanRT) ~ Axes, data=data.summary)
summary(first.lm)

#note that by default R has used a reference level (E.W. - usually decided by alphabetic order)
# so the intercept is the mean value for E.W. and the other model estimates
# are the difference between other conditions and E.W.

# We can then do an ANOVA on this linear model NB: this is treating the 
# conditions as between subjects (will come back to this below)
anova(first.lm)

# To do ANOVA correctly for repeated measures, we tell R to include a within subjects error term
# for each condition. This reflects that we have conditions nested within subjects
# That is, in principle, we can see the condition effect in every subject

# We will now use the aov function
help(aov)  # read through this help file, note the warnings about unbalanced data

aov.1 <-aov(log(meanRT) ~ Axes + Error(SubNo/Axes), data.summary)
#"SubNo" and "Axes" are our sources of variability. 
#The treatment we are interested in is "Axes" (that's what we want to see the effect of), 
#and this treatment effect is visible within each subject (i.e., nested within each subject). 

#The proper Error term is "SubNo/Axes", which is read as "Axes within subjects" 

summary(aov.1)





############# Post-hoc pairwise tests (within & between subjects) #########

#Within subjects
#pairwise t tests for post-hoc comparisons
pairwise.t.test(log(data.summary$meanRT), data.summary$Axes, p.adjust.method="none", paired=T)
# the output is the p value

help(p.adjust)
pairwise.t.test(log(data.summary$meanRT), data.summary$Axes, p.adjust.method="bonferroni", paired=T)


# The package 'multcomp' has other methods for multiple comparisons
# nice summary here: www.pitt.edu/~njc23/Lecture10.pdf


#Between subjects
# TukeyHSD for between subjects (pretending Axes is between subjects here)
aov(first.lm)
glht(aov(first.lm), linfct = mcp(Axes = "Tukey"))

#you can also specify the contrast matrix directly
levels(data.summary$Axes) #note the order of output for labels
contr <- rbind("E.W - N.S" = c(-1, 1, 0, 0),
               "NE.SW - NW.SE" = c(0, 0, 1, -1), 
               "NW.SE - NE.SW" = c(0, 0, -1, 1))
glht(aov(first.lm), linfct = mcp(Axes = contr))

#to get significance, confidence internvals etc.
temp<-glht(aov(first.lm), linfct = mcp(Axes = contr))
summary(temp)
confint(temp)


#### INDEPENDENT PRACTICE  ####
# Complete questions 2-5


#### Graphing Factorial Data ####

#We have our data from the anova for HomLocation
#Now we can try to make a nice bar graph, with error-bars this time!

#summarise first across participants to get their averages
data.summary1 <- ddply(Mydata.corr.a, c("SubNo","HomLocation"), summarise,
                      meanRT = mean(RT, na.rm=TRUE),
                      SDRT = sd(RT, na.rm=TRUE))

#THEN summarise AGAIN to get values for each HomLocation

data.summary2 <- ddply(data.summary1, "HomLocation", summarise,
                       MeanRT = mean(meanRT, na.rm=TRUE),
                       sdRT = sd(SDRT, na.rm=TRUE),
                       se = sd(SDRT, na.rm=TRUE)/sqrt(length(unique(SubNo))))


#build up plot - to see the plot at any time, just type 'plot'

#specify data
plot <- ggplot(data.summary2,aes(x=HomLocation,y=MeanRT))
#specify plot type
plot <- plot + geom_bar(position=position_dodge(),stat="identity")
# add error bars with standard error/CI
plot <- plot + geom_errorbar(aes(ymin=MeanRT-(se*1.96),ymax=MeanRT+(se*1.96),width=.2))

#label axes
plot<-plot + xlab("Scalp Location")
plot<-plot + ylab("Mean RT (seconds)")
#add a title
plot<-plot + ggtitle("Mean RT by Scalp Location")
#change orientation of the x axis
plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#want to change axis limits to zoom in?
plot <-  plot + coord_cartesian(ylim=c(0.2, 0.7))

# use theme and colours to make grey scale
plot<-plot + theme_bw()
plot + scale_fill_grey()


