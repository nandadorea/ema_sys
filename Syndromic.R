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

#Start Tutorial
my.syndromic <- raw_to_syndromicD (id=Id, 
                                   syndromes.var= `Animal species`,
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=databyspeciesf)
plot(my.syndromic@observed)
View(my.syndromic@dates) 
#retro_summary(my.syndromic) 
#Continue the tutorial
#Cattle - daily

#my.syndromic@formula <- list(NA, y~trend+sin+cos+dow, y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7, NA,NA)
#give me error in the next steps

my.syndromic2 <- clean_baseline(my.syndromic,
                                syndromes="Cattle",
                                formula=list(y~sin+cos+dow,y~sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
my.syndromic2@observed
#?HoltWinters()

my.syndromich <- holt_winters_synd(x=my.syndromic2,
                                   evaluate.window=30,
                                   frequency=5, #include the week only
                                   baseline.window=260,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=5,
                                   correct.baseline=2,
                                   alarm.dim=1)

#Works Holt_winters but not the ewma
my.syndromice <- ewma_synd(x=my.syndromic,
                           #syndrome= c(1,2,4,5),
                           evaluate.window=60,
                           baseline.window=260,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5),
                           guard.band=5,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           pre.process=c("glm"),
                           diff.window=5)
