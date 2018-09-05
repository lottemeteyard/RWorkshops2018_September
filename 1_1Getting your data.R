# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading



####################  Welcome to R ###################################################

# We are using R Studio, a much friendlier way to use R
# If we use R itself, all you get is the Console (the window below)
# Script files can be used and annotated
# Look at the bottom of this script file, you can navigate to different parts

#R Studio
#Using the command line in the console (bottom panel)
#Running code from the top panels/R Script files
#Environment
#History
#Bottom right panel: files, plots, packages, Help


#################### Housekeeping #######################

#(1) ALWAYS set working directory
getwd()

#store current directory as a filepath, useful for saving and loading
filepath<-getwd()
filepath

# Copy and paste the whole Workshops folder somewhere sensible
filepath<-"C:/FolderPathHere/WorkshopsFolder"  
#write your preferred file path in here

setwd(filepath)  #set current folder / location to work from

#a simpler way is to use the GUI in R Studio
#Bottom right, click the ... choose the file location
#then click 'More' and 'set as working directory'

# Create an R script file / analysis log #######
# File - New File - R Script (or Ctrl+Shift+N). 
# This acts as a record of what you have done and a way to store useful code
# Save this and call it something sensible.
# Using # marks writing as not code / text for annotating 
# adding ## text ## around something makes it a heading (see below) 


#################### Run some code #######################
1 + 1 #place cursor on line and click 'Run'

######### Store data in objects #######
temp <- c(1,3,5,6,17,8) #temp has now appeared in your Environment

######### Do things with those objects #######
str(temp) #structure of the data object temp
summary(temp)  #summarise data inside temp
(sum(temp))/length(temp)  #calculate mean
mean(temp)  #calculate mean
plot(temp)  #plot data in temp
order(temp)  #gives you the index of the cells in order of magnitude

# work with specific cells in the data
temp[6]
temp[6]<-20  #change cell 6 to hold the value 20
temp[6]

plot(temp)

############################ Importing Data #############################

# It is possible to import directly from excel, using the package readxl

#Find the Homophone_ExpData.xlsx under Files
#Click and then select 'Import data set' - Import
#Note the code that has been executed in the Console
#Note what has appeared in your Environment

# We can also use a specific package
# Packages - Install - search for readxl and install then
library(readxl)

#read in excel data
Homophone_data <- read_excel("Homophone_ExpData.xls")
#Check your environment - what has been imported?
#Click on the data in the Environment panel

#look at sheet names and import specific sheet
excel_sheets("Homophone_ExpData.xls")
read_excel("Homophone_ExpData.xls", sheet = "RT_data")
Homophone_RT <- read_excel("Homophone_ExpData.xls", sheet = "RT_data")


#Tidy up
rm(Homophone_data, Homophone_ExpData)


### Importing text files #########

# R can also import data from .txt or .csv files

# With data in .txt or .csv format, there are two ways to get it into R.
# (1) Import dataset in the Environment (top right panel)
# (2) Read it in yourself
Mydata <- read.table(file="TMS_data.txt",header=TRUE)
# or 
Mydata <- read.delim("TMS_data.txt")
#This has appeared as Mydata in your Environment


### Importing SPSS files #########

# It is possible to import directly from SPSS
# From SPSS data sheet, install package 'foreign'. Then set it running in R.
library(foreign)
# or
require(foreign)

# Read in SPSS file
dat2<-read.spss("SPSS_Data1.sav")  #note the warnings in red...
#this has appeared as dat2 in your environment
str(dat2)


### Data Frames, Column names #########

# work with specific cells in the data
# Now need [rows, columns], remember 'roman catholic'

Homophone_RT[9,4]  #9th row, 4th column
Homophone_RT[40,115]  #40th row, 115th column

names(Homophone_RT) #get column names
Homophone_RT$e_1.control[38]  #use column names to get cell value
Homophone_RT$r_18.homophone[15] 

names(Homophone_RT)[5] #get name of specific column


#### INDEPENDENT PRACTICE ####
#complete activities 1-3 on the worksheet.



#########  Looking at data #########

# R works with different data structures. One particularly useful one is 
# the dataframe or tibble. A data frame is more general than a matrix, 
# in that different columns can have different modes (numeric, character, factor, etc.). 
# This is similar to SPSS and Excel data sheets

# look at variable names and start tidying up
names(Homophone_RT)
# get first few rows: show rows 1 to 5 and columns 1 to 5
Homophone_RT[1:5,1:5]

#rename first column
names(Homophone_RT)[1]<-"Participant"
#rename second column column
names(Homophone_RT)[2]<-"Statistic"


#### INDEPENDENT PRACTICE ####
#Complete questions 4-5


#This kind of housekeeping is really important in R
#It may be easier to make sure you have columns named sensibly in Excel
#with only one row for columns names, rather then spend time trying to tidy in R

names(Homophone_RT)
#We can see there are some empty/junk columns at the end 163-165
#Let's check
head(Homophone_RT[,163])
tail(Homophone_RT[,163])


#### INDEPENDENT PRACTICE ####
#Complete question 6


#Lets remove those columns
dim(Homophone_RT)
Homophone_RT<-Homophone_RT[,1:162]
dim(Homophone_RT) #what has happened to the size?


#### INDEPENDENT PRACTICE ####
#Complete question 7


#list objects in your Environment
ls()

# Remove objects
rm(temp)






#########  Looking at data #########

# First we are going to work with Mydata
# This is for a Flanker and Choice Reaction Time (CRT) Task
# whilst people had TMS across different scalp locations
# we recorded the reported strength of muscle twitches for each trial (0-10 in strength)
# (Meteyard and Holmes, 2018)

#Click on the blue arrow next to Mydata in the Environment panel
#Click on Mydata itself in the Environment panel

# Look at the data
str(Mydata) 
# Notice that R has automatically turned character/nominal variables into factors 
# with levels   

names(Mydata)   # Look at all the variable names

head(Mydata$Axes)  # The first few elements in a variable
head(Mydata$SubNo)
tail(Mydata$Twitches) 


############## Recap - indexing variables ##################

# Roman Catholic - rows, columns
Mydata[47,3]
Mydata[3,9]

# When you want to look at some data use []
Mydata[47,3:5]

# When you want to apply a function use ()
levels(Mydata$Axes)

# When data is stored in a dataframe, use $column_name
names(Mydata)
Mydata$HomLocation


######## Basic descriptive statistics ##########

#explore a variable

mean(Mydata$Twitches)
mean(Mydata$Twitches,na.rm=TRUE) #to get this to function you need to remove missing values

sd(Mydata$Twitches,na.rm=TRUE)
sqrt(var(Mydata$Twitches,na.rm=TRUE))

# Frequency count for each value
# useful for checking for missing data / errors in computations
table(Mydata$Twitches)
#and useful if you want to do a chi-square analysis

# overall summary statistics
summary(Mydata$Twitches)



######## INDEPENDENT PRACTICE  ##########
#Complete question 8 and 9
#Intermediate/Advanced practice: Question 10


#### Basic plots to look at normality #######

#histograms
hist(Mydata$Twitches)
hist(Mydata$RT)

#try with Homophone data set
hist(Homophone_RT$e_1.control)

#combining plots
#http://www.statmethods.net/advgraphs/layout.html
par(mfrow=c(1,2))
hist(Mydata$RT)
hist(Mydata$Twitches)

# check distribution graphically
par(mfrow=c(1,2))
hist(Mydata$RT)
plot(density(Mydata$RT,na.rm=TRUE)) 

#return plotting space to single window (not two panels)
par(mfrow = c(1,1))


# quantile plots
qqnorm(Mydata$RT)        #plot quantiles against theoretical normal distribution
# normal distribution should fall along line
qqline(Mydata$RT)
#we can see the standard 'long tail' for reaction time data

#lets put these together to illustrate that
par(mfrow=c(2,2))
hist(Mydata$RT)
plot(density(Mydata$RT,na.rm=TRUE)) 
qqnorm(Mydata$RT)
qqline(Mydata$RT)


#### INDEPENDENT PRACTICE ####
# Complete question 11




################# Saving work #################

# The simplest thing is to save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  #check the Files tab bottom right, it should appear

# Save specific data
# e.g. save our imported data
save(Homophone_RT,file="Homophone_RT.RData")
save(Homophone_Errors,file="Homophone_Errors.RData")

# Save a text file
write.table(Homophone_RT, "HomophoneRT_data.txt", sep="\t")

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")

#And this R script is already saved!



################# General principles for R #################

# R means coding
# R demands that you problem solve. And be transparent.

# Persevere
# Look for answers
# Trial and error (lots of this)
# Store useful code
# Intelligent copy paste

# This all = TIME. 

### Citing / referencing R packages ###########
# When reporting analyses, you need to cite the specific packages you have used

# To get information to cite the base package
citation()
#check if installed and then give citation
if(nchar(system.file(package="psych"))) citation("psych")

# For a specific package
citation(package = "psych")
citation(package = "foreign")


################  Crib sheets & R helpers ######

# Read help documentation (not always useful, examples usually good)
help(setwd)
??setwd
# For example, to find statistical tests
help.search("anova")
# Return all available functions with 'test' in the name
apropos("test")   
# Return all functions in package 'psych'.
objects(package:psych)

# R reference card
# cran.r-project.org/doc/contrib/Short-refcard.pdf

# Quick R
# www.statmethods.net/

# Cookbook for R
# www.cookbook-r.com/

# Personality project (psychology specific)
# personality-project.org/r/

#### Some websites ###

http://r-statistics.co/
http://r-statistics.co/R-Tutorial.html
http://r-statistics.co/Statistical-Tests-in-R.html


################ Things to remember ###########

# Stay updated
# R and R Studio will update regularly
# Update R first, and R Studio should automatically recongise the version you have
# Some packages may not run on old versions of R

# Packages and reliability:
# The current R repository (CRAN) has ~5604 available packages
# http://cran.r-project.org/web/packages/
# (1) Packages get updated, CHANGED and removed - stay up to date
# (2) Packages may do things you don't understand - stay educated 
# (3) Packages may have bugs - stay up to date & educated & ask for help
# (4) CITE THE PACKAGES AND VERSIONS YOU USE 

# Googling a problem in R will give you discussion boards, 
# web pages and other people's solutions. 
# Remember: there are usually multiple solutions. Find one that works for you.
# The online resources and support community are HUGE
# Luckily for us, it is populated by statisticians and programmers
# and they are happy to help
# Post to a discussion board yourself if you get stuck



#######################################################################
#######################################################################