#setwd("C:/Users/Oliveiras/OneDrive - Food and Agriculture Organization/Desktop/Rmood/Tutorial/vetsyn/ema_sys")

#install.packages("devtools")
#install.packages("dplyr") 
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readr")
#install.packages("padr")
#install.packages("TSstudio")

library(devtools)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate)
library(readr)
library(padr)
library(TSstudio)

data <- read_csv("data/Data_Test_Sandra.csv") #import data

data1<- data  #Duplicate the data

#Correct the dates
#class(data1$`Reporting date`) #is a character and has to pass as a date

a <- dmy(data1$`Reporting date`) #Convert a character to a date
b <- convertToDateTime(as.character(data1$`Reporting date`, origin="1899-31-12")) #Number with 5 digits to date
b1 <- as.Date(b)+1 # Miss one day - add manually
a[is.na(a)] <- b1[!is.na(b1)] #Combine both while keeping their ranks
data1$`Reporting date` <- a #Put it back in your dataframe
data1$`Reporting date`

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
databyspecies <- data3 %>% select(`Id`, `Reporting date`, `Animal species`)

#Use the pad library to add the days that are missing - without any case
databyspeciest <- pad(databyspecies)# Applying pad function
databyspeciest #Print updated data - 7294 rows to 7837 rows

# Replace NAs to 0 only in the column "Id" and "Animal species"
databyspeciest$Id[is.na(databyspeciest$Id)] = 0
databyspeciest$`Animal species`[is.na(databyspeciest$`Animal species`)] = 0
databyspeciest

#Group the data for the time series
Tdatabyspecies <- databyspeciest %>% group_by(`Reporting date`, `Animal species`) %>%  
  summarise(Animal_count = n()) %>% 
  spread(`Animal species`,Animal_count, fill=0)

#Remove the 0 column 
Tdatabyspecies <- within(Tdatabyspecies, rm("0")) 

#I'm just going to analyze three species Swine, Cattle and chicken. But I have columns with these same names 
#and so I have to duplicate the data and insert into the individual columns
#Stick with the columns that interest me: Reporting Date, Cattle, Swine, Cattle.Swine, Chicken and Unspecified.arthropod.chicken
Tdatabyspecies <- Tdatabyspecies %>%
  select(`Reporting date`, `Cattle`, `Cattle,Swine`, `Chicken`,`Swine`, `Unspecified arthropod,Chicken`)
  
Tdatabyspecies_new <- Tdatabyspecies

#Now sum the data in the different columns
Tdatabyspecies_new$Cattle <- as.numeric(Tdatabyspecies_new$Cattle)
Tdatabyspecies_new$`Cattle,Swine`<- as.numeric(Tdatabyspecies_new$`Cattle,Swine`)
Tdatabyspecies_new$Cattle <- Tdatabyspecies_new$`Cattle,Swine` + Tdatabyspecies_new$Cattle

Tdatabyspecies_new$Swine <- as.numeric(Tdatabyspecies_new$Swine)
Tdatabyspecies_new$Swine <- Tdatabyspecies_new$`Cattle,Swine` + Tdatabyspecies_new$Swine

Tdatabyspecies_new$Chicken <- as.numeric(Tdatabyspecies_new$Chicken)
Tdatabyspecies_new$`Unspecified arthropod,Chicken` <- as.numeric(Tdatabyspecies_new$`Unspecified arthropod,Chicken`)
Tdatabyspecies_new$Chicken <- Tdatabyspecies_new$`Unspecified arthropod,Chicken` + Tdatabyspecies_new$Chicken

Tdatabyspeciesf <- Tdatabyspecies_new %>%
  select(`Reporting date`, `Cattle`, `Chicken`,`Swine`) 

#dataCompleted!

ts_plot(Tdatabyspeciesf, line.mode = "lines",width=1000, Xtitle = "Date",
        Ytitle = "Reported Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)

