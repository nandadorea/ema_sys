#setwd("C:/Users/Oliveiras/OneDrive - Food and Agriculture Organization/Desktop/Rmood/Tutorial/vetsyn/ema_sys")
install.packages("readr")
library(readr)
data <- read_csv("data/Data_Test_Sandra.csv")

require(qcc)

install.packages("surveillance")
library("surveillance")
install_github("nandadorea/vetsyn")
install.packages("vetsyn")
require(vetsyn)
library(devtools)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)

#First step correct the wrong dates
data_new <- data 

class(data_new$`Reporting date`) # is a character and has to pass as a date 

a <- dmy(data_new$`Reporting date`) #Character to date
b <- convertToDateTime(as.character(data_new$`Reporting date`, origin="1899-31-12")) #Number with 5 digits to date
b1 <- as.Date(b)+1 #miss one day I have to add manually
a[is.na(a)] <- b1[!is.na(b1)] # Combine both while keeping their ranks
data_new$`Reporting date` <- a # Put it back in your dataframe
data_new$`Reporting date`

class(data_new$`Reporting date`) #date

#Creation of the syndromic object

install.packages("devtools")
library(devtools)

install_github("nandadorea/vetsyn")
require(vetsyn)

# Put the days that are missing
min(data_new$`Reporting date`) # 2019-01-01
max(data_new$`Reporting date`) # 2022-12-30

# Now to simply the task is do a time series with the Animal Species and Reporting date
databyspecies <- data_new %>%
  select(`Id`, `Reporting date`, `Animal species`)

install.packages("padr")                           # Install & load padr package
library("padr")

databyspecies <- pad(databyspecies)# Applying pad function
databyspecies# Print updated data
# Now we add rows so now I have 6219 rows with a lot of NAs
# Replace NAs to 0 
databyspecies$Id[is.na(databyspecies$Id)] = 0
databyspecies$`Animal species`[is.na(databyspecies$`Animal species`)] = 0
databyspecies

#Prepare the data time series!
Tdatabyspecies <- databyspecies %>% group_by(`Reporting date`, `Animal species`) %>%  
  summarise(Animal_count = n()) %>% 
  spread(`Animal species`,Animal_count, fill=0)

#Remove the 0 column 
Tdatabyspecies <- Tdatabyspecies[,-2]

#start doing the graphic
install.packages("TSstudio")
library(TSstudio)
ts_plot(Tdatabyspecies)
#I have a big gap between Oct 2020 and the begining of Dec 2021 
#Still a lot of data I have to improve they way we see
ts_plot(Tdatabyspecies, line.mode = "lines",width=500, Xtitle = "Date",
        Ytitle = "Reported Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)

#I'm just going to analyze three species Swine, Cattle and chicken. But I have columns with these same names 
#and so I have to duplicate the data and insert into the individual columns
#Stick with the columns that interest me: Cattle, Swine, Cattle.Swine, Chicken and Unspecified.arthropod.chicken
Tdatabyspecies <- Tdatabyspecies[ ,c(1,7,8,9,22,25)]
ts_plot(Tdatabyspecies, line.mode = "lines",width=1000, Xtitle = "Date",
        Ytitle = "Reported Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)

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

Tdatabyspeciesf <- Tdatabyspecies_new[ ,c(1,2,4,5)]
#dataCompleted!

ts_plot(Tdatabyspeciesf, line.mode = "lines",width=1000, Xtitle = "Date",
        Ytitle = "Reported Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)

#Prepare Syndromic Object!
#Try to put all the species in one column
#Put the three species in the new column
databyspeciesc <- databyspecies[databyspecies$`Animal species` %in% c("Cattle", "Swine", "0", "Cattle,Swine","Unspecified arthropod,Chicken","Chicken"),]

#chicken = Unspecified arthropod,Chicken
library('stringr')
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Unspecified arthropod,Chicken','Chicken')

#Cattle and Swine = Cattle,Swine




#Start Tutorial

my.syndromic <- raw_to_syndromicD (id=ID, 
                                   syndromes.var=Tdatabyspeciesff$Cattle,
                                   syndromes.name="Cattle",
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=Tdatabyspeciesff)
plot(my.syndromic) #ERROR 1:dim(x@alarms)[3] : NA/NaN argument

# OR

my.syndromic <- raw_to_syndromicD (id=ID, 
                                   syndromes.var=Tdatabyspeciesff$Cattle,
                                   syndromes.name="Cattle",
                                   dates.var=`Reporting date`,
                                   date.format="%y-%m-%d",
                                   min.date = "2019-01-01",
                                   max.date = "2022-12-30",
                                   sort=TRUE,
                                   data=Tdatabyspeciesff)

#ERROR rep(0, (max.date - min.date + 1)) : invalid 'times' argument





