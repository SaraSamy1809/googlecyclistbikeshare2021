#Analysis
#----------------------------Total of rides--------------------
#count the total number of trips
  nrow(year_2021_df_7)
#----------------------------Total of rides per usertype-------------------
#count the total number of rides per each usertype
  year_2021_df_7 %>% 
    count(usertype)
#viz
ggplot(data = year_2021_df_7, mapping = aes(x= usertype, fill = usertype) ) + 
  #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
  #as aes()in geom_funct will be only applied here (hierarchy level)
  geom_bar()+
  labs(title = "Number of rides made by Casual VS Memeber", x=" User_type", y="Number of rides")+
  theme(plot.title = element_text(hjust = 0.5))+ #centering the title
  scale_y_continuous(labels = scales :: comma)+ #removing the E expression
  scale_fill_discrete(name = "User_type")+ # to change the name of the legend
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 0, colour = "black")

ggsave("plot1-Number of rides made by Casual VS Memeber.png")

#----------------------------Total of rides(bike type)-------------------
#count the total ride by each bike type
  year_2021_df_7 %>% 
    count(bikeid)
#viz
ggplot(data = year_2021_df_7, mapping = aes(x= bikeid, fill = bikeid)) + 
  #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
  #as aes()in geom_funct will be only applied here (hierarchy level)
  geom_bar()+
  labs(title = "Number of rides made by bike type",x=" bike type", y="Number of rides")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))+ #centering the title
  scale_y_continuous(labels = scales :: comma)+ #removing the E expression
  scale_fill_discrete(name = "Bike_type")+ # to change the name of the legend
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 0, colour = "black")

ggsave("plot2-Number of rides per each bike type.png")

#----------------------------Total of rides(usertype+bike)-------------------
#count the number of the rides for each bike type per usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(bikeid)
#viz
ggplot(data = year_2021_df_7, mapping = aes(x= bikeid, fill = bikeid)) + 
  #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
  #as aes()in geom_funct will be only applied here (hierarchy level)
  geom_bar()+
  labs(title = "Number of rides made by bike type",caption = "Casual Vs Member",x=" bike type", y="Number of rides")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))+ #centering the title
  scale_y_continuous(labels = scales :: comma)+ #removing the E expression
  scale_fill_discrete(name = "Bike_type")+ # to change the name of the legend
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 0, colour = "black")+
  facet_wrap(~usertype)

ggsave("plot3-Number of rides per each bike type Casual VS Memeber.png")


#----------------------------Total of rides(timing)-------------------
#count the total number of rides per starttiming
  year_2021_df_7 %>% 
    count(start_timing)
#count the total number of rides per starttiming and per usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(start_timing)
#count the total number of rides per starttiming for each bike
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(start_timing)
#count the total number of rides per starttiming for each bike per usertype
  year_2021_df_7 %>% 
    group_by(usertype,bikeid) %>% 
    count(start_timing) %>% 
    print(n=22)# to show the whole tibble
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(x= start_timing, fill = start_timing)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made through the day",caption = "Casual Vs Member",x="start timing", y="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))+ #centering the title
    scale_y_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "Start_timing")+ # to change the name of the legend
    facet_wrap(~usertype~bikeid)
  
  ggsave("plot4-Number of rides per each bike type Casual VS Memeber through the day.png")
  
#----------------------------hour-------------------
#count the total number of rides per hour
  year_2021_df_7 %>% 
    count(start_hour) %>% 
    print(n=25)
#count the total number of rides per hour for each usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(start_hour) %>% 
    print(n=49)
#count the total number of rides per hour for each bikeid
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(start_hour) %>% 
    print(n=80)
#count the total number of rides per hour for each bikeid for each usertype
  year_2021_df_7 %>% 
    group_by(usertype,bikeid) %>% 
    count(start_hour) %>% 
    print(n=122)
#count the total number of rides per hour for each bikeid for each usertype and 
 #starttiming to get the busy hour for each category
  year_2021_df_7 %>% 
    group_by(usertype,bikeid,start_timing) %>% 
    count(start_hour) %>% 
    print(n=122)
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(y= start_hour, fill = start_hour)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made per hour",caption = "Casual Vs Member",y="Hour of order", x="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "Hour")+ # to change the name of the legend
    facet_grid(~usertype)
  
  ggsave("plot5-Number of rides per hour.png")
  
#----------------------------day-------------------
#count the total number of rides per each day
  year_2021_df_7 %>% 
    count(day_of_order)
#count the total number of rides per each day for each usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(day_of_order)
#count the total number of rides per each day for each bikeid
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(day_of_order) %>% 
    print(n=22)
#count the total number of rides per each day for each bikeid for each usertype
  year_2021_df_7 %>% 
    group_by(usertype,bikeid) %>% 
    count(day_of_order) %>% 
    print(n=36)
#count the total number of rides to know the busy day for start timing 
  year_2021_df_7 %>% 
    group_by(day_of_order) %>% 
    count(start_timing) %>% 
    print(n=2000)
#count the total number of rides per each day for each hour per start timing
  year_2021_df_7 %>% 
    group_by(start_hour, start_timing ) %>% 
    count(day_of_order) %>% 
    print(n=200)
#count the total number of rides per each day for each hour per start timing for each bikeid for each usertype
  year_2021_df_7 %>% 
    group_by(usertype, bikeid,start_hour, start_timing ) %>% 
    count(day_of_order) %>% 
    print(n=200)
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(y= day_of_order, fill = day_of_order)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made per day",caption = "Casual Vs Member",y="day_of_order", x="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "Hour")+ # to change the name of the legend
    facet_grid(~usertype)
  
  ggsave("plot6-Number of rides per day.png")
#----------------------------Month-------------------
#count the total number of rides per each month
  year_2021_df_7 %>% 
    count(month)
#count the total number of rides per each month per usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(month) %>% 
    print(n=24)
#count the total number of rides per each month per bike
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(month) %>% 
    print(n=36) 
#count the total number of rides per each month to know the busy day
  year_2021_df_7 %>% 
    group_by(month) %>% 
    count(day_of_order) %>% 
    print(n=100) 
#count the total number of rides per each month to know the busy day for start timing and hour
  year_2021_df_7 %>% 
    group_by(month,day_of_order) %>% 
    count(start_timing) %>% 
    print(n=2000) 
#count the total number of rides per each month to know the busy day for start timing and hour
  year_2021_df_7 %>% 
    group_by(month,day_of_order, start_timing) %>% 
    count(start_hour) %>% 
    print(n=2000) 
#count the total number of rides per each month to know the busy day for start timing and hour, usertype and bikeid
  year_2021_df_7 %>% 
    group_by(usertype, bikeid,month,day_of_order, start_timing) %>% 
    count(start_hour) %>% 
    print(n=2000) 
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(y= month, fill = month)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made per month",caption = "Casual Vs Member",y="Month", x="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "Month")+ # to change the name of the legend
    facet_grid(~usertype)
  
  ggsave("plot7-Number of rides per Month.png") 
#----------------------------Season-------------------
#count the total number of rides per season
  year_2021_df_7 %>% 
    count(season)
#count the number of rides for each usertype per season
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(season)
#count the number of rides for each bikeid per season
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(season)
#count the number of rides for each bikeid per season for each usertype
  year_2021_df_7 %>% 
    group_by(usertype,bikeid) %>% 
    count(season) %>% 
    print(n=22)
#find the peak month for each season according to each usertype
  year_2021_df_7 %>% 
    group_by(usertype,month) %>% 
    count(season) %>% 
    print(n=24)
  
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(y= season, fill = season)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made per season",caption = "Casual Vs Member",y="season", x="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "season")+ # to change the name of the legend
    facet_grid(~usertype)
  
  ggsave("plot8-Number of rides per season.png") 
#----------------------------Year Quarter-------------------
  #count the total number of rides per quarter
  year_2021_df_7 %>% 
    count(year_quarter)
  #count the number of rides for each usertype per quarter
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(year_quarter)
  #count the number of rides for each bikeid per quarter
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(year_quarter)
  #count the number of rides for each bikeid per quarter for each usertype
  year_2021_df_7 %>% 
    group_by(usertype,bikeid) %>% 
    count(year_quarter) %>% 
    print(n=22)
  #find the peak month for each quarter according to each usertype
  year_2021_df_7 %>% 
    group_by(usertype,month) %>% 
    count(year_quarter) %>% 
    print(n=24)
  #viz
  ggplot(data = year_2021_df_7, mapping = aes(y= year_quarter, fill = year_quarter)) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_bar()+
    labs(title = "Number of rides made per season",caption = "Casual Vs Member",y="year quarter", x="Number of rides")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    scale_fill_discrete(name = "year quarter")+ # to change the name of the legend
    facet_grid(~usertype)
  
  ggsave("plot9-Number of rides per year quarter.png") 
#----------------------------station-------------------
#count the number of the rides by from_station
 year_2021_df_7 %>% 
    count(from_station_name) %>% 
    print(n=836) 
  
#count the number of rides by from_station for each usertype
  year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(from_station_name) %>% 
    print(n=1637)  
    
#count the rides used by each type of bikes from the stations
  year_2021_df_7 %>% 
    group_by(bikeid) %>% 
    count(from_station_name) %>% 
    print(n=3000)
#count the rides using the timing, day and season per each usertype
  year_2021_df_7 %>% 
    group_by(usertype, bikeid, start_timing) %>% 
    count(from_station_name) %>% 
    print(n=4000)
  year_2021_df_7 %>% 
    group_by(usertype, bikeid, day_of_order) %>% 
    count(from_station_name) %>% 
    print(n=4000)
  year_2021_df_7 %>% 
    group_by(usertype, bikeid, season) %>% 
    count(from_station_name) %>% 
    print(n=4000)
#viz
  top_10_stations <- year_2021_df_7 %>% 
    group_by(usertype) %>% 
    count(from_station_name, sort= TRUE) %>% 
    slice_head(n=5)
  
  ggplot(data = top_10_stations, mapping = aes(y= from_station_name,x= n, fill =from_station_name )) + 
    #applied the mapping here an not in the geom_bar as it will be applied to the whole built-up layers 
    #as aes()in geom_funct will be only applied here (hierarchy level)
    geom_col()+
    labs(title = "Top 10 start points per no. of rides",caption = "Casual Vs Member",x="number of rides", y="station")+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
    scale_x_continuous(labels = scales :: comma)+ #removing the E expression
    facet_grid(~usertype)
  
  ggsave("plot10-Top 10 start points per no. of rides.png") 

  

#-----------------------------------Time duration of the trip---------------
    max(year_2021_df_7$time_duration)  
    min(year_2021_df_7$time_duration) 
    mean(year_2021_df_7$time_duration)
    sum(year_2021_df_7$time_duration)
    summary(year_2021_df_7$time_duration)
    
#summary(usertype)
    year_2021_df_7 %>% 
      group_by(usertype) %>%
      summarise(first_quartile = as.numeric(quantile(time_duration,0.25)),
                second_quartile = as.numeric(quantile(time_duration,0.50)), 
                median_time_duration = median(time_duration),
                mean_time_duration = mean(time_duration),
                third_quartile = as.numeric(quantile(time_duration,0.75)),
                IQR = as.numeric(quantile(time_duration,0.75))-as.numeric(quantile(time_duration,0.25)),
                #allowed limits so if the values fall outside them they aren't considered as outliers
                max_time_duration = as.numeric(quantile(time_duration,0.75)) + 1.5*(as.numeric(quantile(time_duration,0.75))-as.numeric(quantile(time_duration,0.25))),
                min_time_duration=as.numeric(quantile(time_duration,0.25))- 1.5*(as.numeric(quantile(time_duration,0.75))-as.numeric(quantile(time_duration,0.25))),
                max_time_duration_outlier = max(time_duration),
                min_time_duration_outlier = min(time_duration))
#viz
    ggplot(year_2021_df_7, mapping = aes(x=usertype, y=time_duration, fill= usertype))+
      geom_boxplot()+
      stat_boxplot(geom = "errorbar")+
      labs(title = "summary of the  Time duration", x=" User_type", y="Time_duration(min)")+
      theme(plot.title = element_text(hjust = 0.5))+ #centering the title
      scale_fill_discrete(name = "User_type")+
      coord_cartesian(ylim = c(as.numeric(quantile(year_2021_df_7$time_duration,0.25))+10-1.5*(as.numeric(quantile(year_2021_df_7$time_duration,0.75))-as.numeric(quantile(year_2021_df_7$time_duration,0.25))),as.numeric(quantile(year_2021_df_7$time_duration,0.75))+40+1.5*(as.numeric(quantile(year_2021_df_7$time_duration,0.75))-as.numeric(quantile(year_2021_df_7$time_duration,0.25)))))
    
    #saving the plot
    ggsave("plot11-Statistical summary of the  Time duration of trips for each user type.png")
    
    
#total of duration for the whole trips for each usertype
    year_2021_df_7 %>% 
      group_by(usertype) %>% 
      summarize(sum(time_duration))
#total of duration for the whole trips for each usertype for each bike
    year_2021_df_7 %>% 
      group_by(usertype, bikeid) %>% 
      summarize(sum(time_duration))

#calculating the average of the duration per hour, timing, day, month, season, from and to stations 
  
#hour(total)
    year_2021_df_7 %>% 
      group_by(start_hour) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#hour(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,start_hour) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=48)
#viz
    ggplot(year_2021_df_7, aes(y=start_hour, x = mean(time_duration), fill= start_hour))+
      geom_col()+
      labs(title = "Mean duration taken per hour",caption = "Casual Vs Member",y="Hour", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "Start_hour")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot12-Average duraion taken per each hour in mins.png")
#timing(total)
    year_2021_df_7 %>% 
      group_by(start_timing) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#timing(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,start_timing) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)  
    #viz
    ggplot(year_2021_df_7, aes(y=start_timing, x = mean(time_duration), fill= start_timing))+
      geom_col()+
      labs(title = "Mean duration taken through the day",caption = "Casual Vs Member",y="Start_timing", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "Start_timing")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot13-Average duraion taken through the day.png")
#day(total)
    year_2021_df_7 %>% 
      group_by(day_of_order) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#day(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,day_of_order) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
    
    #viz
    ggplot(year_2021_df_7, aes(y=day_of_order, x = mean(time_duration), fill= day_of_order))+
      geom_col()+
      labs(title = "Mean duration taken per the day",caption = "Casual Vs Member",y="Order_day", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "order day")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot14-Average duraion taken per the day.png")
#month(total)
    year_2021_df_7 %>% 
      group_by(month) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#month(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,month) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24) 
    #viz
    ggplot(year_2021_df_7, aes(y=month, x = mean(time_duration), fill= month))+
      geom_col()+
      labs(title = "Mean duration taken per month",caption = "Casual Vs Member",y="month", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "month")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot15-Average duraion taken per month.png")
#season(total)
    year_2021_df_7 %>% 
      group_by(season) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#season(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,season) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
    #viz
    ggplot(year_2021_df_7, aes(y=season, x = mean(time_duration), fill= season))+
      geom_col()+
      labs(title = "Mean duration taken per season",caption = "Casual Vs Member",y="season", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "Season")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot16-Average duraion taken per season.png")
#year_quarter(total)
    year_2021_df_7 %>% 
      group_by(year_quarter) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
#year_quarter(usertype)
    year_2021_df_7 %>% 
      group_by(usertype,year_quarter) %>% 
      summarize(mean(time_duration)) %>% 
      print(n=24)
    #viz
    ggplot(year_2021_df_7, aes(y=year_quarter, x = mean(time_duration), fill= year_quarter))+
      geom_col()+
      labs(title = "Mean duration taken per year_quarter",caption = "Casual Vs Member",y="year_quarter", x="Mean Duration")+
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
      scale_x_continuous(labels = scales :: comma)+ #removing the E expression
      scale_fill_discrete(name = "year_quarter")+ # to change the name of the legend
      facet_grid(~usertype)
    ggsave("Plot17-Average duraion taken per year_quarter.png")
    
#station (total)
     year_2021_df_7 %>% 
      group_by(from_station_name) %>% 
      summarize(mean(time_duration)) %>% 
       print(n=1000)
#station (usertype)
     year_2021_df_7 %>% 
       group_by(usertype,from_station_name) %>% 
       summarize(mean(time_duration)) %>% 
       print(n=2000)
#viz     
 top_10_average <-year_2021_df_7 %>% 
   group_by(usertype, from_station_name) %>% 
   summarise(mean(time_duration)) 
top_10_average<-top_10_average[order(-top_10_average$`mean(time_duration)`),]#add the , to the end
top_10_average<-top_10_average %>% 
  group_by(usertype) %>% 
  slice_head(n=5)

ggplot(top_10_average, aes(y=top_10_average$`mean(time_duration)`, x=top_10_average$from_station_name, fill = top_10_average$from_station_name))+
  geom_col(position = position_nudge(x=0.3))+
  labs(title = "Mean duration taken per Top10 Start points",caption = "Casual Vs Member",x="station", y="Mean Duration")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
  scale_y_continuous(labels = scales :: comma)+ #removing the E expression
  scale_fill_discrete(name = "station")+ # to change the name of the legend
  facet_grid(~usertype)

ggsave("Plot18-Average duraion taken per Top10 Start points.png")
 

#--------------------------Distance(meter)-------------------
max(year_2021_df_7$distance) 
min(year_2021_df_7$distance) 
mean(year_2021_df_7$distance)
sum(year_2021_df_7$distance)
summary(year_2021_df_7$distance)

#summary(usertype)
year_2021_df_7 %>% 
  group_by(usertype) %>%
  summarise(first_quartile = as.numeric(quantile(distance,0.25)),
            second_quartile = as.numeric(quantile(distance,0.50)), 
            median_time_duration = median(distance),
            mean_time_duration = mean(distance),
            third_quartile = as.numeric(quantile(distance,0.75)),
            IQR = as.numeric(quantile(distance,0.75))-as.numeric(quantile(distance,0.25)),
            #allowed limits so if the values fall outside them they aren't considered as outliers
            max_time_duration = as.numeric(quantile(distance,0.75)) + 1.5*(as.numeric(quantile(distance,0.75))-as.numeric(quantile(distance,0.25))),
            min_time_duration=as.numeric(quantile(distance,0.25))- 1.5*(as.numeric(quantile(distance,0.75))-as.numeric(quantile(distance,0.25))),
            max_time_duration_outlier = max(distance),
            min_time_duration_outlier = min(distance))
#viz
ggplot(year_2021_df_7, mapping = aes(x=usertype, y=distance, fill= usertype))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar")+
  labs(title = "summary of the distance", x=" User_type", y="Distance(meters)")+
  theme(plot.title = element_text(hjust = 0.5))+ #centering the title
  scale_fill_discrete(name = "User_type")+
  coord_cartesian(ylim = c(as.numeric(quantile(year_2021_df_7$distance,0.25))+10-1.5*(as.numeric(quantile(year_2021_df_7$distance,0.75))-as.numeric(quantile(year_2021_df_7$distance,0.25))),as.numeric(quantile(year_2021_df_7$distance,0.75))+40+1.5*(as.numeric(quantile(year_2021_df_7$distance,0.75))-as.numeric(quantile(year_2021_df_7$distance,0.25)))))

#saving the plot
ggsave("plot19-Statistical summary of the distance of trips for each user type.png")

     
#total of distance for the whole trips for each usertype
     year_2021_df_7 %>% 
       group_by(usertype) %>% 
       summarize(sum(distance))
#total of distance for the whole trips for each usertype for each bike
     year_2021_df_7 %>% 
       group_by(usertype, bikeid) %>% 
       summarize(sum(distance))
     
#calculating the average of the duration per hour, timing, day, month, season, from and to stations 
     
#hour(total)
     year_2021_df_7 %>% 
       group_by(start_hour) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#hour(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,start_hour) %>% 
       summarize(mean(distance)) %>% 
       print(n=48)
     
#viz
     ggplot(year_2021_df_7, aes(y=start_hour, x = mean(distance), fill= start_hour))+
       geom_col()+
       labs(title = "Mean distance taken per hour",caption = "Casual Vs Member",y="Hour", x="Mean Distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "Start_hour")+ # to change the name of the legend
       facet_grid(~usertype)
     
     ggsave("Plot20-Average distance taken per each hour in meters.png")
#timing(total)
     year_2021_df_7 %>% 
       group_by(start_timing) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#timing(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,start_timing) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)  
#viz
     ggplot(year_2021_df_7, aes(y=start_timing, x = mean(distance), fill= start_timing))+
       geom_col()+
       labs(title = "Mean distance taken through the day",caption = "Casual Vs Member",y="Start_timing", x="Mean distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "Start_timing")+ # to change the name of the legend
       facet_grid(~usertype)
     ggsave("Plot21-Average distance taken through the day.png")
#day(total)
     year_2021_df_7 %>% 
       group_by(day_of_order) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#day(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,day_of_order) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
     
#viz
     ggplot(year_2021_df_7, aes(y=day_of_order, x = mean(distance), fill= day_of_order))+
       geom_col()+
       labs(title = "Mean distance taken per day",caption = "Casual Vs Member",y="Order_day", x="Mean distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "order_day")+ # to change the name of the legend
       facet_grid(~usertype)
     ggsave("Plot22-Average distance taken per the day.png")
#month(total)
     year_2021_df_7 %>% 
       group_by(month) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#month(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,month) %>% 
       summarize(mean(distance)) %>% 
       print(n=24) 
 #viz    
     ggplot(year_2021_df_7, aes(y=month, x = mean(distance), fill= month))+
       geom_col()+
       labs(title = "Mean distance taken through the day",caption = "Casual Vs Member",y="month", x="Mean distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "month")+ # to change the name of the legend
       facet_grid(~usertype)
     ggsave("Plot23-Average distance taken per month.png")
#season(total)
     year_2021_df_7 %>% 
       group_by(season) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#season(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,season) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
     
     #viz    
     ggplot(year_2021_df_7, aes(y=season, x = mean(distance), fill= season))+
       geom_col()+
       labs(title = "Mean distance taken per season",caption = "Casual Vs Member",y="season", x="Mean distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "season")+ # to change the name of the legend
       facet_grid(~usertype)
     ggsave("Plot24-Average distance taken per season.png")
     
#year_quarter(total)
     year_2021_df_7 %>% 
       group_by(year_quarter) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
#year_quarter(usertype)
     year_2021_df_7 %>% 
       group_by(usertype,year_quarter) %>% 
       summarize(mean(distance)) %>% 
       print(n=24)
     
#viz    
     ggplot(year_2021_df_7, aes(y=year_quarter, x = mean(distance), fill= year_quarter))+
       geom_col()+
       labs(title = "Mean distance taken per year_quarter",caption = "Casual Vs Member",y="year_quarter", x="Mean distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_x_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "year_quarter")+ # to change the name of the legend
       facet_grid(~usertype)
     ggsave("Plot25-Average distance taken per year_quarter.png")
#station (total)
     year_2021_df_7 %>% 
       group_by(from_station_name) %>% 
       summarize(mean(distance)) %>% 
       print(n=1000)
#station (usertype)
     year_2021_df_7 %>% 
       group_by(usertype,from_station_name) %>% 
       summarize(mean(distance)) %>% 
       print(n=2000)
#viz     
     top_10_avg_dist <-year_2021_df_7 %>% 
       group_by(usertype, from_station_name) %>% 
       summarise(mean(distance)) 
     top_10_avg_dist<-top_10_avg_dist[order(-top_10_avg_dist$`mean(distance)`),]#add the , to the end
     top_10_avg_dist<-top_10_avg_dist %>% 
       group_by(usertype) %>% 
       slice_head(n=5)
     
     ggplot(top_10_average, aes(y=top_10_avg_dist$`mean(distance)`, x=top_10_avg_dist$from_station_name, fill = top_10_avg_dist$from_station_name))+
       geom_col(position = position_nudge(x=0.3))+
       labs(title = "Mean distance taken per Top10 Start points",caption = "Casual Vs Member",x="station", y="Mean Distance")+
       theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5))+ #centering the title
       scale_y_continuous(labels = scales :: comma)+ #removing the E expression
       scale_fill_discrete(name = "station")+ # to change the name of the legend
       facet_grid(~usertype)
     
     ggsave("Plot26-Average distance taken per Top10 Start points.png")
     