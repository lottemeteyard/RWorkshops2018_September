# R teaching Workshops.
# Norway, University of Agder
# Lotte Meteyard, 2018
# School of Psychology and Clinical Language Sciences,University of Reading


######################################################################
# Once data is in R, most of your time will be spent getting it tidy
# and doing sanity checks

# you will get used to never looking at raw data spreadsheets

# NB: there are usually several ways to do the same thing in R

################## the tidyverse ##########################

#We will look at helpful packages for tidying up data
# This set of packages forms part of the 'Tidyverse'
https://www.tidyverse.org/

# Today we will look at
reshape2
dplyr
ggplot2


################## reshape2 ##########################

#install rehsape2
library(reshape2)

#the main functions in reshape2 are melt() and cast()

#melt() and cast() allow you to move between
#long and wide format data

#wide format data has a column for each variable and data in the cells
#long format data has the data in one column and other columns are used to code that data

# for a full tutorial see http://seananderson.ca/2013/10/19/reshape.html
# we'll follow that tutorial here
# use some data built into R

#look at the data first
head(airquality)

#let's melt it first
melt(airquality)  #scroll up and down to see what has happened to the data

#because we didn't specify id variables, R tells us what it is using by default
#'variable' identifies the row for 'value' (the numeric data)

#let's melt it but this time but be more specific 
# now we are keeping the columns month and day
melt(airquality, id.vars = c("Month", "Day"))

#scroll up to the column names
# you can see Month and Day have been kept
# and variable and value have the other columns
# Solar.R, Ozone and Wind are coded in variable
# the numeric data associated with these columns are under value

# we can repeat this, but give specific names
melt(airquality, id.vars = c("Month", "Day"),
     variable.name = "climate_variable", 
     value.name = "climate_value")
#scroll up to see these as column names

# going from long to wide format is a bit more tricky
# you will most often use data frames, so you need dcast

#melt first to wide format
temp <- melt(airquality, id.vars = c("Month", "Day"))
head(temp)

#go back to long format
dcast(temp, Month + Day ~ variable)
#note how Month and Day have been kept as column variables, and the rest
#of the labels in variable have become column names, with the data
#in each of them

#let's repeat this with some data we've got
head(Homophone_RT)
names(Homophone_RT)

#first have to melt so that reshape2 can work with the data
melt(Homophone_RT, id.vars = c("Participant", "Statistic"))

head(melt(Homophone_RT, id.vars = c("Participant", "Statistic")))
tail(melt(Homophone_RT, id.vars = c("Participant", "Statistic")))
#so the RT has been put into one column, with all the other column names
#now put into one place

#repeat this, but add column names and store in a data object called 'temp'
Homophone_RT_Long <- melt(Homophone_RT, id.vars = c("Participant", "Statistic"),
             variable.name = "Condition", 
             value.name = "RT")
head(Homophone_RT_Long)
tail(Homophone_RT_Long)

#Finally, we need to take the Condition labels and turn them into two
#as they code item (e.g. e_1) and also condition (control/homophone)
#we can 'split' what is in the Condition column
#this is a useful trick if you need to specify labels for 'long' format data

str(Homophone_RT_Long$Condition)
#it is a factor, we need it to be text
Homophone_RT_Long$Condition <- as.character(Homophone_RT_Long$Condition)
str(Homophone_RT_Long$Condition)
#then we can tell R to 'split' at the "."
strsplit(Homophone_RT_Long$Condition[1],"[.]")
#or
unlist(strsplit(Homophone_RT_Long$Condition[1],"[.]"))

#Now we can create two new columns and put the new labels in there.
dim(Homophone_RT_Long)  #has 4 columns, we are going to add 5 and 6

#We need to create a loop to do this...
unlist(strsplit(Homophone_RT_Long$Condition[3],"[.]"))[1]
unlist(strsplit(Homophone_RT_Long$Condition[3],"[.]"))[2]

for (i in 1:6400) {
  Homophone_RT_Long[i,5] <- unlist(strsplit(Homophone_RT_Long$Condition[i],"[.]"))[1]
  Homophone_RT_Long[i,6] <- unlist(strsplit(Homophone_RT_Long$Condition[i],"[.]"))[2]
  }

head(Homophone_RT_Long)
tail(Homophone_RT_Long)

#Now we have the data in complete 'long' format


#### INDEPENDENT PRACTICE ####
# Complete question 1 
# Intermediate/Advanced: question 2


#Now we will practice turning this data back into wide format
#But only for Condition (i.e. collapsing across Item)
#with average/mean RT at each level of Condition 
dcast(Homophone_RT_Long, Participant ~ WordCondition, value.var = "RT",
      fun.aggregate = mean, na.rm = TRUE)
#note that we have asked it to remove any NA data


#### INDEPENDENT PRACTICE ####
# Complete questions 3
# Intermediate: question 4



####### dplyr #######
#some further methods here for exploring and summarising data
#using dplyr
#See http://genomicsclass.github.io/book/pages/dplyr_tutorial.html


# install dplyr
library(dplyr)

help(package="dplyr")
#we will work with the functions: filter, arrange, select, mutate, summarise

#Helpful summary can be found here:
https://www.rstudio.com/resources/cheatsheets/
  
# ToothGrowth data set - what does it look like
head(ToothGrowth)

####### Filter #######

# filter selects parts of the data according to some criteria
# select data for participant 2, when they made an error
# Note the similarity to the 

#Add subject numbers 
ToothGrowth$Subjects <- rep(1:10)

filter(ToothGrowth, supp == "VC")
filter(ToothGrowth, supp == "VC" & Subjects == 1)


####### Arrange #######

# arrange reorders rows 
# reorder entire data set by Subject
arrange(ToothGrowth, Subjects)

#look at this briefly (rather than whole data set)
head(arrange(ToothGrowth, Subjects))
tail(arrange(ToothGrowth, Subjects))


####### Select #######

# select helps take out data / zoom in on data you want
# note this works best for numeric data
# select Participant, RT, Item and WordCondition from Homophone_RT_Long data set:
select(Homophone_RT_Long, Participant, RT, Item, WordCondition)

# we can store this for future use
temp <- select(Homophone_RT_Long, Participant, RT, Item, WordCondition)
head(temp)
summary(temp)
dim(temp)


####### Mutate #######

# mutate allows you to add new data by transforming / using data 
# from existing columns
# create RT data in seconds (not milliseconds)
mutate(temp, RTsec = RT/1000)
head(mutate(temp, RTsec = RT/1000))

# we want to add this onto the existing data set, so we need to store it
temp <- mutate(temp, RTms = RT*1000)
head(temp)


#### INDEPENDENT PRACTICE ####
# Complete question 5-6.
# Intermediate: question 7



################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")


####### Additional code/practice  #######


################## tidying up names/cells/spaces/mislabelling ##########################


####### relabelling / recoding #######

#If you want to rename conditions, levels etc. on your data
#there is a nice function called 'revalue' in plyr (the precusor to dplyr)

library(plyr)  #note the warning because we already have dplyr installed

unique(Mydata$Hemisphere)
#this tells us how many 'unique' values there in this variable

# Lets relabel Hemisphere with left and right
# 1 = left
# 2 = right

#Change coding to a factor or text (otherwise revalue gets upset)
Mydata$Hemisphere<-as.factor(Mydata$Hemisphere)

#we are going to store the relabelled data in a new column called 'Hemlabel'
names(Mydata)

Mydata$Hemlabel<-revalue(Mydata$Hemisphere, c("1" = "Left", "2" = "Right"))

names(Mydata)
unique(Mydata$Hemlabel)




##### Get rid of spaces in cells with content/labels (common with excel imports)  #######

# Install the gdata package, we'll use the 'trim' function
library(gdata)
help(trim)

# Lets make some data with annoying spaces

x<-c(" hair", "hair", " nohair", " hair", "hair ", " nohair", "nohair", "hair ", " hair", " nohair", "nohair ")
unique(x)  #we can see the consequences of these spaces

x<-trim(x)  #overwrite the original data with trimmed data
unique(x)   #Now we only have two conditions



################## The importance of NAs ##########################

# NAs are REALLY important in R. Most or all functions have easy options
# to manage NAs in your data, but it must be coded with NAs, not blank cells etc.

# Let's create some messy data

#our data with spaces
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
#a random sample of 11 numbers from 1-20, 
y<-sample(1:20, 11)
#put these together into a data fram
x<-data.frame(cbind(x,y))
#give the columns names
names(x)<-c("HairCut","Biscuits")
#now look at it
x


# check which cells are empty (convert to text before doing this if necessary)
which(x$HairCut==" ")

# Replace missing data with NA
x$HairCut[which(x$HairCut==" ")]<-NA 
# Replace cells with XXX with NA
x$HairCut[which(x$HairCut=="XXX")]<-NA 
# Replace cells with CCC with NA
x$HairCut[which(x$HairCut=="CCC")]<-NA 
x

which(x$HairCut==" ")  # check again
which(x==" ") 



# Recreate data 
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
y<-sample(1:20, 11)
x<-data.frame(cbind(x,y))
names(x)<-c("HairCut","Biscuits")
x

# Check manually for NA (empty) cells
is.na(x)    # check whether all cells are NA or not (true/false)

# Replace whole row with NA if one value missing
x[which(x$HairCut==" "),]<-NA  
is.na(x)
x

# Check a specific column
is.na(x$HairCut) 

