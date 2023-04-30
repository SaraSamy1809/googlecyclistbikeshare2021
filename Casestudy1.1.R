#Read the clean data

year_2021_df_6 <- read_csv("year_2021_df.csv")
head(year_2021_df_6)
#Change the start day to weekdays
year_2021_df_6$day_of_order<- weekdays(strptime(year_2021_df_6$start_day, format = "%Y-%m-%d"))
str(year_2021_df_6)

#Calculation of the distance in meters using lat-LNG
year_2021_df_7 <- year_2021_df_7 %>%
  rowwise %>% 
  #get the distance between the start and stop in meters
  mutate(distance =distm(x=c(start_lat,start_lng),y=c(end_lat, end_lng), fun = distHaversine)) 


#separation of the hour only
year_2021_df_7 <- year_2021_df_6 %>%
  rowwise%>%
  mutate(start_hour = format(as.POSIXct(starttime), format = "%H"), stop_hour =format(as.POSIXct(stoptime), format = "%H")) 

#Months in words
year_2021_df_7$month <-months(strptime(year_2021_df_7$start_day, format = "%Y-%m-%d"))

#round the time in min
year_2021_df_7$time_duration <- round(year_2021_df_7$time_duration,0)

#Change the vector to numeric
year_2021_df_7$distance <-as.numeric(year_2021_df_7$distance)
head(year_2021_df_7)
#round the distance in meters
year_2021_df_7$distance <- round(as.numeric(year_2021_df_7$distance),2)
#there are some distance values with -ve time duration, which is a wrong entry
year_2021_df_7$time_duration <- abs(year_2021_df_7$time_duration)

year_2021_df_7$distance <- abs(as.numeric(year_2021_df_7$distance))


head(year_2021_df_7)


year_2021_df_7 <-year_2021_df_7 %>% 
#remove any zero time_duration and distance
  filter(time_duration > 0) %>%
  filter(round(distance,0) >0) 

#dropping the unrequired columns to free space
year_2021_df_7<- year_2021_df_7 %>% 
  subset(select = -c(trip_id, starttime, stoptime, start_time, stop_time,start_day, stop_day))

#Setting Night, Morning, Afternoon, Evening according to the hours for start and stop time
year_2021_df_7$start_timing = NA
 year_2021_df_7$start_timing<-  case_when(year_2021_df_7$start_hour =="00" ~ "Night",
                                          year_2021_df_7$ start_hour =="01" ~ "Night",
                                          year_2021_df_7$start_hour =="02" ~ "Night",
                                          year_2021_df_7$start_hour =="03" ~ "Night",
                                          year_2021_df_7$start_hour =="04" ~ "Night",
                                          year_2021_df_7$start_hour =="05" ~ "Morning",
                                          year_2021_df_7$start_hour =="06" ~ "Morning",
                                          year_2021_df_7$start_hour =="07" ~ "Morning",
                                          year_2021_df_7$start_hour =="08" ~ "Morning",
                                          year_2021_df_7$start_hour =="09" ~ "Morning",
                                          year_2021_df_7$start_hour =="10" ~ "Morning",
                                          year_2021_df_7$start_hour =="11" ~ "Morning",
                                          year_2021_df_7$start_hour =="12" ~ "Afternoon",
                                          year_2021_df_7$start_hour =="13" ~ "Afternoon",
                                          year_2021_df_7$start_hour =="14" ~ "Afternoon",
                                          year_2021_df_7$start_hour =="15" ~ "Afternoon",
                                          year_2021_df_7$start_hour =="16" ~ "Afternoon",
                                          year_2021_df_7$start_hour =="17" ~ "Evening",
                                          year_2021_df_7$start_hour =="18" ~ "Evening",
                                          year_2021_df_7$start_hour =="19" ~ "Evening",
                                          year_2021_df_7$start_hour =="20" ~ "Evening",
                                          year_2021_df_7$start_hour =="21" ~ "Night",
                                          year_2021_df_7$start_hour =="22" ~ "Night",
                                          year_2021_df_7$start_hour =="23" ~ "Night")
  
 year_2021_df_7$stop_timing = NA
 year_2021_df_7$stop_timing<-  case_when(year_2021_df_7$stop_hour =="00" ~ "Night",
                                          year_2021_df_7$ stop_hour =="01" ~ "Night",
                                          year_2021_df_7$stop_hour =="02" ~ "Night",
                                          year_2021_df_7$stop_hour =="03" ~ "Night",
                                          year_2021_df_7$stop_hour =="04" ~ "Night",
                                          year_2021_df_7$stop_hour =="05" ~ "Morning",
                                          year_2021_df_7$stop_hour =="06" ~ "Morning",
                                          year_2021_df_7$stop_hour =="07" ~ "Morning",
                                          year_2021_df_7$stop_hour =="08" ~ "Morning",
                                          year_2021_df_7$stop_hour =="09" ~ "Morning",
                                          year_2021_df_7$stop_hour =="10" ~ "Morning",
                                          year_2021_df_7$stop_hour =="11" ~ "Morning",
                                          year_2021_df_7$stop_hour =="12" ~ "Afternoon",
                                          year_2021_df_7$stop_hour =="13" ~ "Afternoon",
                                          year_2021_df_7$stop_hour =="14" ~ "Afternoon",
                                          year_2021_df_7$stop_hour =="15" ~ "Afternoon",
                                          year_2021_df_7$stop_hour =="16" ~ "Afternoon",
                                          year_2021_df_7$stop_hour =="17" ~ "Evening",
                                          year_2021_df_7$stop_hour =="18" ~ "Evening",
                                          year_2021_df_7$stop_hour =="19" ~ "Evening",
                                          year_2021_df_7$stop_hour =="20" ~ "Evening",
                                          year_2021_df_7$stop_hour =="21" ~ "Night",
                                          year_2021_df_7$stop_hour =="22" ~ "Night",
                                          year_2021_df_7$stop_hour =="23" ~ "Night")
 

  #making season and year_quarters using month
year_2021_df_7$season =NA
year_2021_df_7$season<- case_when(year_2021_df_7$month=="January"~"Winter",
                                  year_2021_df_7$month=="February"~"Winter",
                                  year_2021_df_7$month=="March"~"Spring",
                                  year_2021_df_7$month=="April"~"Spring",
                                  year_2021_df_7$month=="May"~"Spring",
                                  year_2021_df_7$month=="June"~"Summer",
                                  year_2021_df_7$month=="July"~"Summer",
                                  year_2021_df_7$month=="August"~"Summer",
                                  year_2021_df_7$month=="September"~"Autumn",
                                  year_2021_df_7$month=="October"~"Autumn",
                                  year_2021_df_7$month=="November"~"Autumn",
                                  year_2021_df_7$month=="December"~"Winter")
year_2021_df_7$year_quarter = NA
year_2021_df_7$year_quarter<-  case_when(year_2021_df_7$month=="January"~"Q1",
                            year_2021_df_7$month=="February"~"Q1",
                            year_2021_df_7$month=="March"~"Q1",
                            year_2021_df_7$month=="April"~"Q2",
                            year_2021_df_7$month=="May"~"Q2",
                            year_2021_df_7$month=="June"~"Q2",
                            year_2021_df_7$month=="July"~"Q3",
                            year_2021_df_7$month=="August"~"Q3",
                            year_2021_df_7$month=="September"~"Q3",
                            year_2021_df_7$month=="October"~"Q4",
                            year_2021_df_7$month=="November"~"Q4",
                            year_2021_df_7$month=="December"~"Q4")
  


head(year_2021_df_7)

str(year_2021_df_7)

#Export the sheet to the clean sheet
fwrite(year_2021_df_7, "clean_year_2021_df.pdf")
