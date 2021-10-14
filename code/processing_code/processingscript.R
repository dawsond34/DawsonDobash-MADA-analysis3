###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#Removing all variables that include the words Score, Total, FluA, FluB, Dxname, or Activity in their name.
#Removing the variable Unique.Visit as well
#Finally remove any NA's within the data set
processeddata <- rawdata %>% select(-contains(c("Score", "Total", "FluA", "FluB", "Dxname", "Activity")), 
                                   -Unique.Visit) %>% drop_na()

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


