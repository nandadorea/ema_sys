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


#I have a big gap between Oct 2020 and the begining of Dec 2021





#Prepare Syndromic Object!
#Select the rows that have our species of interest
databyspeciesc <- databyspecies[databyspecies$`Animal species` %in% c("Cattle", "Swine","Cattle,Swine","Unspecified arthropod,Chicken","Chicken"),]

#chicken = Unspecified arthropod,Chicken
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Unspecified arthropod,Chicken','Chicken')

#Cattle and Swine = Cattle,Swine
add <- databyspeciesc[databyspeciesc$`Animal species` %like% "Cattle,Swine", ] #the rows to add isolated
add$`Animal species` <- str_replace(add$`Animal species`,'Cattle,Swine','Cattle') #change the name with the Animal Specie
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Cattle,Swine','Swine') #change the name with the Animal Specie
databyspeciesf <- full_join(databyspeciesc, add) #Joint

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

