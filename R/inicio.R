#setwd("C:/Users/Oliveiras/OneDrive - Food and Agriculture Organization/Desktop/Rmood/Tutorial/vetsyn/ema_sys")
install.packages("readr")
library(readr)
data <- read_csv("data/Data_Test_Sandra.csv")


install.packages("surveillance")
install.packages("DescTools")
install_github("nandadorea/vetsyn")
install.packages("vetsyn")
install.packages("sqldf")
library("surveillance")
require(vetsyn)
library(devtools)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)
require(qcc)


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
#INICIO-----------------------------------------------------------------------------------------------------------------------------------------
# Now we add rows so now I have 6219 rows with a lot of NAs
# Replace NAs to 0 - JUST FOR TIME SERIES
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

#FIM----------------------------------------------------------------------------------------------------------------------------------------------

#Prepare Syndromic Object!
#Try to put all the species in one column
#Put the three species in the new column
databyspeciesc <- databyspecies[databyspecies$`Animal species` %in% c("Cattle", "Swine",NA, "Cattle,Swine","Unspecified arthropod,Chicken","Chicken"),]

#chicken = Unspecified arthropod,Chicken
library('stringr')
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Unspecified arthropod,Chicken','Chicken')

library(sqldf)
library("DescTools")

#Cattle and Swine = Cattle,Swine
add <- databyspeciesc[databyspeciesc$`Animal species` %like% "Cattle,Swine", ] #the rows to add isolated
add$`Animal species` <- str_replace(add$`Animal species`,'Cattle,Swine','Cattle') #change the name with the Animal Specie
databyspeciesc$`Animal species` <- str_replace(databyspeciesc$`Animal species`,'Cattle,Swine','Swine') #change the name with the Animal Specie
databyspeciesf <- full_join(databyspeciesc, add) #Joint

#Remove the ID column and put other
databyspeciesf <- databyspeciesf[,-1]
databyspeciesf <- tibble::rowid_to_column(databyspeciesf, "ID")


#Start Tutorial
my.syndromic <- raw_to_syndromicD (id=ID, 
                                   syndromes.var= `Animal species`,
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=databyspeciesf)


View(my.syndromic@dates) #ERROR 1:dim(x@alarms)[3] : NA/NaN argument #Nao tenho alarmes!
retro_summary(my.syndromic) #NAO CORRER OUTRA VEZ! 

#NAO CORRER A PARTIR DAQUI

#INICIO-----------------------------------------------------------------------------------------------------------------------------------------

# I will try to see if the problem is the "0"
databyspeciesff <-databyspeciesf[!(databyspeciesf$`Animal species`=="0"),]
databyspeciesff <- databyspeciesff[,-1]
databyspeciesff <- tibble::rowid_to_column(databyspeciesff, "ID")

my.syndromic2 <- raw_to_syndromicD (id=ID, 
                                   syndromes.var= "Animal Specie",
                                   dates.var=`Reporting date`, 
                                   date.format="%y-%m-%d",
                                   sort=TRUE,
                                   data=databyspeciesff)

plot(my.syndromic2) #ERROR 1:dim(x@alarms)[3] : NA/NaN argument SEM ALARMES NAO VAI CORRER 

# I will try removing Chicken and "0"

databyspeciesff <-databyspeciesf[!(databyspeciesf$`Animal species`=="0"| databyspeciesf$`Animal species`=="Chicken"),]
databyspeciesff <- databyspeciesff[,-1]
databyspeciesff <- tibble::rowid_to_column(databyspeciesff, "ID")

my.syndromic3 <- raw_to_syndromicD (id=ID, 
                                    syndromes.var= "Animal Specie",
                                    syndromes.name=c("Cattle","Swine"),
                                    dates.var=`Reporting date`, 
                                    date.format="%y-%m-%d",
                                    sort=TRUE,
                                    data=databyspeciesff)
plot(my.syndromic3) #ERROR 1:dim(x@alarms)[3] : NA/NaN argument

# OR

my.syndromic4 <- raw_to_syndromicD (id=ID, 
                                   syndromes.var="Animal Specie",
                                   syndromes.name=c("Cattle","Swine"),
                                   dates.var=`Reporting date`,
                                   date.format="%y-%m-%d",
                                   min.date = "2019-01-01",
                                   max.date = "2022-12-30",
                                   sort=TRUE,
                                   data=databyspeciesff)

#ERROR rep(0, (max.date - min.date + 1)) : invalid 'times' argument

#FIM-----------------------------------------------------------------------------------------------------------------------



