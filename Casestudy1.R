#install geo and data.table libraries
install.packages("geosphere")
install.packages("data.table")
#uploading libraries and tools
library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(readr)
library(lubridate)
library(rmarkdown)
library(readxl)
library(writexl)
library(geosphere)
library(data.table)
#changing directory

setwd("D:/Local Disk/Data Analysis All/Google DATA ANALySIS/cyclistdata/New folder (2)")

#Read the data of each month

jan_data<- read_csv("1_2021.csv")
feb_data<- read_csv("2_2021.csv")
march_data<- read_csv("3_2021.csv")
april_data<- read_csv("4_2021.csv")
may_data<- read_csv("5_2021.csv")
june_data<- read_csv("6_2021.csv")
july_data<- read_csv("7_2021.csv")
aug_data<- read_csv("8_2021.csv")
sep_data<- read_csv("9_2021.csv")
oct_data<- read_csv("10_2021.csv")
nov_data<- read_csv("11_2021.csv")
dec_data<- read_csv("12_2021.csv")

#Column names
#January data preparation - column names
colnames(jan_data)
jan_2021 <- rename(jan_data,
                   trip_id = "ride_id",
                   bikeid = "rideable_type",
                   starttime = "started_at",
                   stoptime = "ended_at",
                   from_station_name = "start_station_name",
                   from_station_id ="start_station_id",
                   to_station_name = "end_station_name",
                   to_station_id = "end_station_id",
                   usertype = "member_casual")
colnames(jan_2021)

#Februrary data preparation - column names
colnames(feb_data)
feb_2021 <- rename(feb_data,
                   trip_id = "ride_id",
                   bikeid = "rideable_type",
                   starttime = "started_at",
                   stoptime = "ended_at",
                   from_station_name = "start_station_name",
                   from_station_id ="start_station_id",
                   to_station_name = "end_station_name",
                   to_station_id = "end_station_id",
                   usertype = "member_casual")
colnames(feb_2021)
#March data preparation - column names
colnames(march_data)
march_2021 <- rename(march_data,
                   trip_id = "ride_id",
                   bikeid = "rideable_type",
                   starttime = "started_at",
                   stoptime = "ended_at",
                   from_station_name = "start_station_name",
                   from_station_id ="start_station_id",
                   to_station_name = "end_station_name",
                   to_station_id = "end_station_id",
                   usertype = "member_casual")
colnames(march_2021)
#April data preparation - column names
colnames(april_data)
april_2021 <- rename(april_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(april_2021)
#May data preparation - column names
colnames(may_data)
may_2021 <- rename(may_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(may_2021)
#June data preparation - column names
colnames(june_data)
june_2021 <- rename(june_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(june_2021)
#July data preparation - column names
colnames(july_data)
july_2021 <- rename(july_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(july_2021)
#August data preparation - column names
colnames(aug_data)
aug_2021 <- rename(aug_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(aug_2021)
#September data preparation - column names
colnames(sep_data)
sep_2021 <- rename(sep_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(sep_2021)
#October data preparation - column names
colnames(oct_data)
oct_2021 <- rename(oct_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(oct_2021)
#November data preparation - column names
colnames(nov_data)
nov_2021 <- rename(nov_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(nov_2021)
#December data preparation - column names
colnames(dec_data)
dec_2021 <- rename(dec_data,
                     trip_id = "ride_id",
                     bikeid = "rideable_type",
                     starttime = "started_at",
                     stoptime = "ended_at",
                     from_station_name = "start_station_name",
                     from_station_id ="start_station_id",
                     to_station_name = "end_station_name",
                     to_station_id = "end_station_id",
                     usertype = "member_casual")
colnames(dec_2021)

#Check on the colnames of all the 12 sheets
colnames(jan_2021)
colnames(feb_2021)
colnames(march_2021)
colnames(april_2021)
colnames(may_2021)
colnames(june_2021)
colnames(july_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
#Create a data set of trips for 2021
year_2021_df <- bind_rows(jan_2021, feb_2021,march_2021,april_2021,may_2021,
                          june_2021,july_2021,aug_2021,sep_2021,oct_2021,
                          nov_2021,dec_2021)
head(year_2021_df)
str(year_2021_df)

#Drop the NA data
year_2021_df_1 <- drop_na(year_2021_df)
head(year_2021_df_1)
str(year_2021_df_1)
#check that the text is all the same
year_2021_df_1$trip_id <- tolower(year_2021_df_1$trip_id)
year_2021_df_1$bikeid<-tolower(year_2021_df_1$bikeid)
year_2021_df_1$from_station_name<-tolower(year_2021_df_1$from_station_name)
year_2021_df_1$from_station_id<-tolower(year_2021_df_1$from_station_id)
year_2021_df_1$to_station_name<-tolower(year_2021_df_1$to_station_name)
year_2021_df_1$to_station_id<-tolower(year_2021_df_1$to_station_id)
year_2021_df_1$usertype<-tolower(year_2021_df_1$usertype)
head(year_2021_df_1)
#Remove duplicates
year_2021_df_2 <- distinct(year_2021_df_1)
head(year_2021_df_2)

#Change the start_time and stop_time
year_2021_df_3 <- mutate(year_2021_df_2,
                         start_datetime = as.character.Date(year_2021_df_2$starttime),
                         stop_datetime = as.character.Date(year_2021_df_2$stoptime))
head(year_2021_df_3)

#Split the day and time for the start and stop
#Start datetime
year_2021_df_4 <- separate(year_2021_df_3,start_datetime,into = c('start_day','start_time'), sep = " ")
head(year_2021_df_4)

#Stop datetime
year_2021_df_5 <- separate(year_2021_df_4,stop_datetime,into = c('stop_day','stop_time'), sep = " ")
head(year_2021_df_5)

#Calculate the time duration
year_2021_df_6 <- mutate(year_2021_df_5, 
                         time_duration = as.numeric(difftime(stoptime, starttime, units = "mins")))
str(year_2021_df_6)
head(year_2021_df_6)
#Export the data to csv file to free the memory
write_csv(year_2021_df_6,"year_2021_df.csv")
