#loading necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(readr)

setwd("H:/R_Data")

#Loading monthly cyclistic data sets for 2021
jan_2021 <- read_csv("jan2021_tripdata.csv")
feb_2021 <- read_csv("feb2021_tripdata.csv")
mar_2021 <- read_csv("mar2021_tripdata.csv")
apr_2021 <- read_csv("apr2021_tripdata.csv")
may_2021 <- read_csv("may2021_tripdata.csv")
jun_2021 <- read_csv("jun2021_tripdata.csv")
jul_2021 <- read_csv("jul2021_tripdata.csv")
aug_2021 <- read_csv("aug2021_tripdata.csv")
sep_2021 <- read_csv("sep2021_tripdata.csv")
oct_2021 <- read_csv("oct2021_tripdata.csv")
nov_2021 <- read_csv("nov2021_tripdata.csv")
dec_2021 <- read_csv("dec2021_tripdata.csv")

#Compare structures of monthly data frames
str(jan_2021)
str(feb_2021)
str(mar_2021)
str(apr_2021)
str(may_2021)
str(jun_2021)
str(jul_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)

#Merging monthly data into a yearly data structure
tripdata_2021 <- bind_rows(jan_2021, feb_2021, mar_2021, apr_2021,
                           may_2021, jun_2021, jul_2021, aug_2021,
                           sep_2021, oct_2021, nov_2021, dec_2021)

#Separate start date and time into start date, start time, end date and end time
tripdata2_2021 <- tripdata_2021 %>% 
  separate(started_at, into =  c("start_date", "start_time"), sep = " ") %>%
  separate(ended_at, into = c("end_date", "end_time"), sep = " ")

#Create an additional column for the month
tripdata2_2021$month <- format(as.Date(tripdata2_2021$start_date, format = "%m/%d/%Y"), "%b")

#Convert ride_length from time to num
tripdata2_2021$ride_length <- as.numeric(tripdata2_2021$ride_length)

#Check for negative ride_length values
sum(tripdata2_2021$ride_length < 0, na.rm = T)

#Check for 0 ride_length values
sum(tripdata2_2021$ride_length == 0, na.rm = T)

#Check for ride_length values greater than 1 day
sum(tripdata2_2021$ride_length > 86400, na.rm = T)

#Check for N/A values in ride_length
is.na(tripdata2_2021$ride_length)

#Remove data entries with N/A ride lengths
tripdata_clean_2021 <- drop_na(tripdata2_2021,"ride_length")

#Convert day_of_week from number to chr, Note: Currently 1 = Sunday, 7 = Saturday
tripdata_clean_2021$day_of_week <- recode(tripdata_clean_2021$day_of_week,  "1"="Sunday", "2"="Monday", "3"="Tuesday",
                                                                            "4"="Wednesday", "5"="Thursday","6"="Friday",
                                                                            "7"="Saturday")

#Summary of tripdata_clean_2021
tripdata_clean_2021 %>%
  group_by(member_casual) %>%
  summarise(mean(ride_length), median(ride_length), min(ride_length), max(ride_length))

#Comparing casual riders and members based on day of the week. Note: 1 = Sunday and 7 = Saturday
day_of_week_rider_summary <-  tripdata_clean_2021 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), avg_duration = mean(ride_length))

#Comparing casual riders and members based on Month
month_rider_summary <-  tripdata_clean_2021 %>%
  group_by(member_casual ,month) %>%
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) 

#Reordering months
month_rider_summary$month <- factor(month_rider_summary$month, levels = month.abb)

#Comparing Casual riders and members based on bike types
biketype_rider_summary <-  tripdata_clean_2021 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) 


#Plot of rides based on day of the week
#position = "dodge" places stacked bars side-by-side
ggplot(data = day_of_week_rider_summary, aes(x = day_of_week, y = number_of_rides, fill= member_casual))+ 
  geom_col(position = position_dodge(.5), width = 0.5) + xlim("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") +
  scale_y_continuous(labels = comma) + labs(title = "Number of Rides Based on Day of the Week for 2021", x = "Day of the Week", y = "Number of Rides") + theme_bw() + scale_fill_discrete(name = "Rider Type")

#Plot of ride duration based on day of the week 
ggplot(data = day_of_week_rider_summary,aes(x = day_of_week, y = avg_duration/60, fill= member_casual))+ 
  geom_col(position = position_dodge(0.5), width = 0.5) + xlim("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") + 
  scale_y_continuous(labels = comma) + labs(title = "Average Ride Duration Based on Day of the Week for 2021", x = "Day of the Week", y = "Average Ride Duration (minutes)") + theme_bw() + scale_fill_discrete(name = "Rider Type") 

#Line graph of rides based on month
ggplot(data = month_rider_summary,aes(x = month, y = number_of_rides, group = member_casual, color = member_casual))+ geom_line(linewidth = 1.5) + 
  labs(title = "Number of Rides Based on Month for 2021", x = "Month", y = "Number of Rides")+ scale_x_discrete(limits = month.abb) +
  scale_y_continuous(labels = comma) + theme_bw() + scale_fill_discrete(name = "Rider Type") + theme(axis.text.x = element_text(angle = 90))


#Bar graph of rides based on month
ggplot(data = month_rider_summary, aes(x = month, y = number_of_rides, fill= member_casual))+ geom_col(position = position_dodge(0.5), width = 0.5) + 
  labs(title = "Number of Rides Based on Month for 2021", x = "Month", y = "Number of Rides")+ 
  scale_y_continuous(labels = comma) + theme_bw() + scale_fill_discrete(name = "Rider Type") + theme(axis.text.x = element_text(angle = 90)) + scale_x_discrete(limits = month.abb)

#Plot of ride duration based on month
ggplot(data = month_rider_summary,aes(x = month, y = avg_duration/60, fill= member_casual))+ geom_col(position = position_dodge(0.5), width = 0.5) +
  labs(title = "Average Ride Duration Based on Month for 2021", x = "Month", y = "Average Ride Duration (minutes)") + 
  scale_y_continuous(labels = comma) + theme_bw() + scale_fill_discrete(name = "Rider Type") + theme(axis.text.x = element_text(angle = 90)) + scale_x_discrete(limits = month.abb)

#Plot of rides based on bike type
ggplot(data = biketype_rider_summary,aes(x = rideable_type, y = number_of_rides, fill= member_casual))+ 
  geom_col(position = position_dodge(0.5), width = 0.5) + labs(title = "Number of Rides Based on Bike Type for 2021", x = "Bike Type", y = "Number of Rides")+ 
  scale_y_continuous(labels = comma) + theme_bw() + scale_fill_discrete(name = "Rider Type") + scale_x_discrete(labels = c("classic_bike" = "Classic bike", "docked_bike" = "Docked bike", "electric_bike" = "Electric bike" ))


#Plot of ride duration based on bike type
ggplot(data = biketype_rider_summary, aes(x = rideable_type, y = avg_duration/60, fill= member_casual))+ 
  geom_col(position = position_dodge(0.5), width = 0.5) + labs(title = "Average Ride Duration Based on Bike Type for 2021", x = "Bike Type", y = "Average Ride Duration (minutes)")+ 
  scale_y_continuous(labels = comma) + theme_bw() + scale_fill_discrete(name = "Rider Type") + scale_x_discrete(labels = c("classic_bike" = "Classic bike", "docked_bike" = "Docked bike", "electric_bike" = "Electric bike" ))


