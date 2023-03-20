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

databyspeciesfcattle <- databyspeciesf[databyspeciesf$`Animal species` %in% c("Cattle"),]

#Start Tutorial FIRST CATTLE DAILY
my.syndromic <- raw_to_syndromicD (id=Id, 
                                   syndromes.var= `Animal species`,
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=databyspeciesfcattle)
plot(my.syndromic@observed)
View(my.syndromic@dates) 
#retro_summary(my.syndromic) 
#Continue the tutorial
#Cattle - daily

#my.syndromic@formula <- list(NA, y~trend+sin+cos+dow, y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7, NA,NA)
#give me error in the next steps

my.syndromic2 <- clean_baseline(my.syndromic,
                                formula=list(y~sin+cos+dow,y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
my.syndromic2@observed

#HoltWinters

my.syndromich <- holt_winters_synd(x=my.syndromic2,
                                   evaluate.window=30,
                                   frequency=5, #include the week only
                                   baseline.window=260,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=5,
                                   correct.baseline=2,
                                   alarm.dim=1)
plot(my.syndromich)

#EWMA

my.syndromicDe <- raw_to_syndromicD (id=Id,
                                    syndromes.var=`Animal species`,
                                    dates.var=`Reporting date`,
                                    date.format="%y-%m-%d",
                                    data=databyspeciesfcattle)

my.syndromicDe <- ewma_synd(x=my.syndromicDe,
                           evaluate.window=10,
                           baseline.window=260,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process="glm",
                           family="poisson",
                           formula=list(y~sin+cos+dow,y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                           frequency=260)
plot(my.syndromicDe)

#Shewhart

my.syndromicDs <- raw_to_syndromicD(id=Id,
                                    syndromes.var=`Animal species`,
                                    dates.var=`Reporting date`,
                                    date.format="%y-%m-%d",
                                    data=databyspeciesfcattle)

my.syndromicDs <- shew_synd(x=my.syndromicDs,
                           evaluate.window=1,
                           baseline.window=260,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=7,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           UCL=1,
                           LCL=FALSE,
                           pre.process="glm",
                           diff.window=5,
                           family="poisson",
                           formula=list(y~sin+cos+dow,y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                           frequency=260)
plot(my.syndromicDs)

#CUSUM

my.syndromicDc <- raw_to_syndromicD (id=Id,
                                    syndromes.var=`Animal species`,
                                    dates.var=`Reporting date`,
                                    date.format="%y-%m-%d",
                                    data=databyspeciesfcattle)

my.syndromicDc <- cusum_synd(x=my.syndromicDc,
                            evaluate.window=30,
                            baseline.window=260,
                            limit.sd=c(2.5,3,3.5),
                            guard.band=5,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process="glm",
                            family="poisson",
                            formula=list(y~sin+cos+dow,y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7),
                            frequency=260)
plot(my.syndromicDc)
     
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

#do the same thing 

#plot(x=my.syndromic,
     #syndromes="Cattle",
     #window=365,
     #baseline=TRUE,
     #UCL=1,
     #algorithms=NULL,
     #limit=1)
#doesn't work
#syndromic_alarm(x=my.syndromic,
                #plot.all=TRUE,
                #email.alarm.to="<dorea.meyer@gmail.com>",
                #email.noalarm.to="<dorea.meyer@gmail.com>")
#doesn't work

#CHICKEN AND SWINE WEEKLY

databyspeciesfchickenswine <- databyspeciesf[databyspeciesf$`Animal species` %in% c("Swine","Chicken"),]

my.syndromicw <- raw_to_syndromicW (id=Id, 
                                   syndromes.var=`Animal species`,
                                   syndromes.name=c("Chicken","Swine"),
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d", 
                                   data=databyspeciesfchickenswine)
#retro_summary(my.syndromicw)
#Ask why replace the previous retro_summary

my.syndromicw2 <- clean_baseline(my.syndromicw,
                                formula=list(week~trend+sin+cos),
                                limit=0.99)
#holt Winters
my.syndromicwh <- holt_winters_synd(x=my.syndromicw2,
                                  evaluate.window=10,
                                  frequency=52,
                                  baseline.window=104,
                                  limit.sd=c(2.5,3,3.5), #default
                                  nahead=2,
                                  correct.baseline=2,
                                  alarm.dim=1)
plot(my.syndromicwh)

#EWMA

my.syndromicwe <- raw_to_syndromicW (id=Id,
                                     syndromes.var=`Animal species`,
                                     syndromes.name=c("Chicken","Swine"),
                                     dates.var=`Reporting date`,
                                     date.format="%y-%m-%d",
                                     data=databyspeciesfchickenswine)

my.syndromicwe <- ewma_synd(x=my.syndromicwe,
                            syndromes=c("Chicken","Swine"),
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
plot(my.syndromicwe)


#Shewhart
my.syndromicws <- raw_to_syndromicW(id=Id,
                                    syndromes.var=`Animal species`,
                                    syndromes.name=c("Chicken","Swine"),
                                    dates.var=`Reporting date`,
                                    date.format="%y-%m-%d",
                                    data=databyspeciesfchickenswine)

my.syndromicws <- shew_synd(x=my.syndromicws,
                            syndromes=c("Chicken","Swine"),
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
plot(my.syndromicws)

#Cusum

my.syndromicwc <- raw_to_syndromicW (id=Id,
                                     syndromes.var=`Animal species`,
                                     syndromes.name=c("Chicken","Swine"),
                                     dates.var=`Reporting date`,
                                     date.format="%y-%m-%d",
                                     data=databyspeciesfchickenswine)

my.syndromicwc <- cusum_synd(x=my.syndromicwc,
                             syndromes=c("Chicken","Swine"),
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
plot(my.syndromicwc)

#update

databyspeciesfchickenswineupdate <- databyspeciesfchickenswine

databyspeciesfchickenswineupdate$`Reporting date` <- databyspeciesfchickenswineupdate$`Reporting date` + 1

my.syndromicw
my.syndromicw <- update_syndromic(x=my.syndromicw,
                                 id=Id,
                                 syndromes.var=`Animal species`, 
                                 add.syndromes=TRUE,
                                 dates.var=`Reporting date`, 
                                 date.format="%y-%m-%d", 
                                 replace.dates=TRUE,
                                 data=databyspeciesfchickenswine)
my.syndromicw
