setwd("C:/Users/Oliveiras/OneDrive - Food and Agriculture Organization/Desktop/Rmood/Tutorial/vetsyn/ema_sys")
install.packages("readr")
library(readr)
data <- read_csv("data/Data_Test_Sandra.csv")

require(qcc)

install.packages("surveillance")
library("surveillance")

install.packages("vetsyn")
require(vetsyn)

library(dplyr) 
library(tidyr)


#First Step If don't have an observation date put equal to the reported date

data_new <- data                                                    # Duplicate data
data_new$`Observation date`[is.na(data_new$`Observation date`)] <- data_new$`Reporting date`[is.na(data_new$`Observation date`)]  # Replace NA values
data_new                                                            # Print new data

#some of the rows from Observation date doesn't have a date have a number with five digits like starting at 44000
# I want to remove that rows - I don't know if it can affect my results but i don't have reporting date
# Some rows have the reporting date and that number in the observation date so I will try to do that first ex.: 249112
data_new$`Observation date` <- ifelse (data_new$`Observation date`> 44000, data_new$`Reporting date`, data_new$`Observation date`) 
#Now it is ok I will remove the rows that don't have a date that at all - library dplyr
exclude <- c(44000:50000)
datasetnew <- data_new %>% 
  filter(!(data_new$`Observation date` %in% exclude))
# 7294 observations to 5468 observations that has a observation date or a reporting date. 

class(datasetnew$`Observation date`) # is a character and has to pass as a date 
library(lubridate)
datasetnew$`Observation date` <- dmy(datasetnew$`Observation date`) #convert character to date format
class(datasetnew$`Observation date`) #ok now is a date!

#Creation of the syndromic object

install.packages("devtools")
library(devtools)

install_github("nandadorea/vetsyn")
require(vetsyn)

# Put the days that are missing
min(datasetnew$`Observation date`) # 2016-12-19
max(datasetnew$`Observation date`) # 2020-10-12

# Now to simply the task is do a time series with the Animal Species and Observation date
databyspecies <- datasetnew %>%
  select(`Id`, `Observation date`, `Animal species`)

install.packages("padr")                           # Install & load padr package
library("padr")

databyspecies <- pad(databyspecies)# Applying pad function
databyspecies# Print updated data
# Now we add rows so now I have 6219 rows with a lot of NAs
# Replace NAs to 0 
databyspecies$Id[is.na(databyspecies$Id)] = 0
databyspecies$`Animal species`[is.na(databyspecies$`Animal species`)] = 0
databyspecies

#Prepare the data
Tdatabyspecies <- databyspecies %>% group_by(`Observation date`, `Animal species`) %>%  
  summarise(Animal_count = n()) %>% 
  spread(`Animal species`,Animal_count, fill=0)

#Remove the 0 column 
Tdatabyspecies <- Tdatabyspecies[,-2]

#start doing the graphic
install.packages("TSstudio")
library(TSstudio)
ts_plot(Tdatabyspecies)
#I have a big gap between 2017 and the begining of 2019 so I will remove that data
Tdatabyspecies_wg <- Tdatabyspecies %>% filter(`Observation date` > '2019-01-01')
ts_plot(Tdatabyspecies_wg)
#Still a lot of data I have to improve they way we see
ts_plot(Tdatabyspecies_wg, line.mode = "lines",width=500, Xtitle = "Date",
        Ytitle = "Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)

#I will remove the columns with large number of values Swine and Cattle
Tdatabyspecies_wgs <- Tdatabyspecies_wg[ ,-c(5,19)]
ts_plot(Tdatabyspecies_wgs, line.mode = "lines",width=1000, Xtitle = "Date",
        Ytitle = "Cases", title = "Number of cases per species ", Xgrid = FALSE, Ygrid = FALSE)
#What to do next by disease? 

# Finish for now 


#Tentative of Syndromic
my.syndromic <- syndromicD(datebyspecies,min.date="2016-12-19",max.date="2020-10-12")

my.syndromic <- raw_to_syndromicD (id=databyspecies$`Id`, 
                                   syndromes.var= databyspecies$`Animal species`,
                                   dates.var= databyspecies$`Observation date`,  
                                   date.format="%Y-%m-%d", 
                                   data=databyspecies)
plot(my.syndromic)

Data_alltime <- setDatesD

#I try to do Retrospective analysis it was just errors....#I think this might be because I always have NAs... When I was doing the tutorial also
# gave me this Number of detection algorithms used = NA 
##my.syndromic <- raw_to_syndromicD (id=datasetnew$Id, 
##syndromes.var= datasetnew$`Animal species`,
##dates.var=datasetnew$`Observation date`, 
##date.format="%d/%m/%Y", 
##min.date="19/12/2016", 
##max.date="12/10/2020",
##sort=TRUE,
##data=datasetnew)

##my.syndromic

## dim(my.syndromic)
##dim(my.syndromic)[1] #rows = number of time points
##dim(my.syndromic)[2] #columns = number of syndromes monitored
##dim(my.syndromic)[3] #3rd dimension = number of detection algorithms used (more later)

##retro_summary(my.syndromic, frequency=365)


my.syndromic <- raw_to_syndromicD (id=`Id`, 
                                   syndromes.var=`Animal species`,
                                   dates.var=`Observation date`, 
                                   date.format="%d/%m/%Y", 
                                   sort=TRUE,
                                   data=databyspecies)


