#setwd("C:/Users/Oliveiras/OneDrive - Food and Agriculture Organization/Desktop/Rmood/Tutorial/vetsyn/ema_sys")

#install_github("nandadorea/vetsyn")
#install.packages("sqldf")
#install.packages("surveillance")
#install.packages("vetsyn")
#install.packages("devtools")
#install.packages("dplyr") 
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("openxlsx")
#install.packages("qcc")
#install.packages("readr")
#install.packages("stringr")
#install.packages("sqldf")
#install.packages("DescTools")

library(surveillance)
library(vetsyn)
library(devtools)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(qcc)
library(readr)
library(stringr)
library(sqldf)
library(DescTools)

data <- read_csv("data/Data_Test_Sandra.csv") #import data

data1<- data  #Duplicate the data

#Correct the dates
#class(data1$`Reporting date`) #is a character and has to pass as a date

a <- dmy(data1$`Reporting date`) #Convert a character to a date
b <- convertToDateTime(as.character(data1$`Reporting date`, origin="1899-31-12")) #Number with 5 digits to date
b1 <- as.Date(b)+1 #Miss one day - add manually
a[is.na(a)] <- b1[!is.na(b1)] #Combine both while keeping their ranks
data1$`Reporting date` <- a #Put it back in your dataframe
data1$`Reporting date`

#class(data1$`Reporting date`) # Now as a date

#see the minimum and maximum date
# min(data1$`Reporting date`) # 2019-01-01
# max(data1$`Reporting date`) # 2022-12-30

#Select the important columns for this analysis
databyspecies <- data1 %>%
  select(`Id`, `Reporting date`, `Animal species`)

#class(data1$`Reporting date`) # Now as a date

#see the minimum and maximum date
# min(data1$`Reporting date`) # 2019-01-01
# max(data1$`Reporting date`) # 2022-12-30

#I have a big gap between Oct 2020 and the begining of Dec 2021 - 2020-10-14 to 2021-11-30. Replace the missing data with the same data that is between 
# 2019-10-14 to 2020-11-30 and add one year
adddata <- with(data1, data1[(`Reporting date` >= "2019-10-14" & `Reporting date` <= "2020-11-30"), ]) #Isolate the data that I want to put again with more one year
# min(adddata$`Reporting date`) # 2019-10-15
# max(adddata$`Reporting date`) # 2020-10-13
# Add one year!
tmp <- as.POSIXlt(adddata$`Reporting date`)
tmp$year <- tmp$year+1
dates2 <- as.Date(tmp)
adddata$`Reporting date` <- dates2

#Combine the new data with the old one
data2 <- rbind(data1, adddata)
data2

# but It has an overlap of missing data (2021-10-14 to 2021-11-30) so I will add the data from (2019-10-14 to 2019-11-30) and add two years
adddata1 <- with(data2, data2[(`Reporting date` >= "2019-10-14" & `Reporting date` <= "2019-11-30"), ]) #Isolate the data that I want to put again with more 2 years
# min(adddata1$`Reporting date`) # 2019-10-15
# max(adddata1$`Reporting date`) # 2019-11-30
# Add two years!
tmp1 <- as.POSIXlt(adddata1$`Reporting date`)
tmp1$year <- tmp1$year+2
dates3 <- as.Date(tmp1)
adddata1$`Reporting date` <- dates3

#Combine the new data with the old one
data3 <- rbind(data2, adddata1)
data3

#Select the important columns for this analysis
databyspecies <- data3 %>%
  select(`Id`, `Reporting date`, `Animal species`)

#Prepare Syndromic Object!
#Select the rows that have our species of interest
databyspeciesc <- databyspecies[databyspecies$`Animal species` %in% c("Cattle", "Swine","Cattle,Swine","Unspecified arthropod,Chicken","Chicken"),]

#chicken = Unspecified arthropod,Chicken
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Unspecified arthropod,Chicken','Chicken')

#Cattle and Swine = Cattle,Swine
addrows <- databyspeciesc[databyspeciesc$`Animal species` %like% "Cattle,Swine", ] #the rows to add isolated
addrows$`Animal species` <- str_replace(addrows$`Animal species`,'Cattle,Swine','Cattle') #change the name with the Animal Specie
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Cattle,Swine','Swine') #change the name with the Animal Specie
databyspeciesf <- full_join(databyspeciesc, addrows) #Joint

#Start Tutorial DAILY
my.syndromic <- raw_to_syndromicD (id=Id, 
                                   syndromes.var= `Animal species`,
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=databyspeciesf)

plot(my.syndromic@observed[,1], type= "l") # type"l"line for default is dot 
View(my.syndromic@dates) 
#retro_summary(my.syndromic) 

my.syndromic2 <- clean_baseline(my.syndromic,
                                syndromes=c("Cattle", "Chicken","Swine"),
                                formula=list(y~sin+cos+dow+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
my.syndromic2@observed

#HoltWinters

my.syndromic2 <- holt_winters_synd(x=my.syndromic2,
                                   evaluate.window=30,#assess the last 30 days, that is way we only have UPC in last 30 days 
                                   frequency=365, #include the seven days of the week, the different cycles are the years.
                                   #The repetition cycle that we want to modulate, each year a new cycle begins.
                                   baseline.window=730,# How far back do we consider timeline - 10 years ago or 2 years ago (at least). 
                                   #Considering many years earlier can affect our results because the perspective can be completely different.
                                   #We considered two years so it is 2x365 = 730 days 
                                   limit.sd=c(2.5,3,3.5), #Give score 1 when it is 2.5x standard deviations from the mean;
                                   #Give score 2 when it is 3x standard deviations from the mean;
                                   #Give score 3 when it is 3.5x standard deviations from the mean;
                                   nahead=7, #Predict seven days (one week) Different from the guard band that is the separation between the today data to do previous data
                                   correct.baseline=2, #If has a score 2 or 3 (because it passed the score 2 (3x < 3.5x)) in the standard deviations correct and not include in the baseline 
                                   alarm.dim=1) # Save holt winters in section 1, I can put the different control charts in different numbers. 
plot(my.syndromic2)
#In the end of the graphic the red line is UCL, Blue line is the baseline, vertical line pink an alarm (Observed superior to the UPC) 

#EWMA

my.syndromic2 <- ewma_synd(x=my.syndromic2,
                           evaluate.window=30,
                           baseline.window=730,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=7,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="glm",
                           family="poisson",
                           formula=list(y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                           frequency=365)
plot(my.syndromic2)

#Shewhart

my.syndromic2 <- shew_synd(x=my.syndromic2,
                           evaluate.window=30,
                           baseline.window=730,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=7,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           UCL=1,
                           LCL=FALSE,
                           pre.process="glm",
                           diff.window=5,
                           family="poisson",
                           formula=list(y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                           frequency=365)
plot(my.syndromic2)

#CUSUM

my.syndromic2 <- cusum_synd(x=my.syndromic2,
                            evaluate.window=30,
                            baseline.window=730,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=7,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="glm",
                            family="poisson",
                            formula=list(y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                            frequency=365)
plot(my.syndromic2)
     
#WEEKLY

my.syndromicw <- raw_to_syndromicW (id=Id, 
                                    syndromes.var=`Animal species`,
                                    dates.var=`Reporting date`, 
                                    date.format="%y-%m-%d", 
                                    data=databyspeciesf)

my.syndromicw2 <- clean_baseline(my.syndromicw,
                                 formula=list(week~trend+sin+cos),
                                 limit=0.99)
#holt Winters
my.syndromicw2 <- holt_winters_synd(x=my.syndromicw2,
                                    evaluate.window=10,
                                    frequency=52,
                                    baseline.window=104,
                                    limit.sd=c(2.5,3,3.5), #default
                                    nahead=7,
                                    correct.baseline=2,
                                    alarm.dim=1)
plot(my.syndromicw2)

#EWMA

my.syndromicw2 <- ewma_synd(x=my.syndromicw2,
                            evaluate.window=10,
                            baseline.window=104,
                            lambda=0.2,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=2,
                            correct.baseline=FALSE,
                            alarm.dim=2,
                            pre.process="glm",
                            family="poisson",
                            formula=list(week~trend+sin+cos),
                            frequency=52)
plot(my.syndromicw2)


#Shewhart
my.syndromicw2 <- shew_synd(x=my.syndromicw2,
                            evaluate.window=10,
                            baseline.window=104,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=2,
                            correct.baseline=FALSE,
                            alarm.dim=3,
                            pre.process="glm",
                            family="poisson",
                            formula=list(week~trend+sin+cos),
                            frequency=52)
plot(my.syndromicw2)

#Cusum
my.syndromicw2 <- cusum_synd(x=my.syndromicw2,
                             evaluate.window=10,
                             baseline.window=104,
                             limit.sd=c(2.5,3,3.5),
                             guard.band=2,
                             correct.baseline=FALSE,
                             alarm.dim=4,
                             pre.process="glm",
                             family="poisson",
                             formula=list(week~trend+sin+cos),
                             frequency=52)
plot(my.syndromicw2)

#update

databyspeciesfupdate <- databyspeciesf

databyspeciesfupdate$`Reporting date` <- databyspeciesfupdate$`Reporting date` + 1

my.syndromic2
my.syndromic2 <- update_syndromic(x=my.syndromic2,
                                  id=Id,
                                  syndromes.var=`Animal species`, 
                                  add.syndromes=TRUE,
                                  dates.var=`Reporting date`, 
                                  date.format="%y-%m-%d", 
                                  replace.dates=TRUE,
                                  data=databyspeciesfupdate)
my.syndromic2
#error


#update

databyspeciesfcattleupdate <- databyspeciesfcattle 

databyspeciesfcattleupdate$`Reporting date` <- databyspeciesfcattleupdate$`Reporting date` + 1

my.syndromic
my.syndromic <- update_syndromic(x=my.syndromic,
                                 id=Id,
                                 syndromes.var=`Animal species`, 
                                 add.syndromes=TRUE,
                                 dates.var=`Reporting date`, 
                                 date.format="%y-%m-%d", 
                                 replace.dates=TRUE,
                                 data=databyspeciesfcattleupdate)
my.syndromic

