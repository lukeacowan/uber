library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(leaflet)
library(modelr)
library(tidyverse)

rm(list = ls())

#setwd("C:/Users/lukec/OneDrive/Documents/data332/uber/uber_project/data")

# reading in files
april_data <- readRDS(file = "uber-raw-data-apr14.rds")
may_data <- readRDS(file = "uber-raw-data-may14.rds")
june_data <- readRDS(file = "uber-raw-data-jun14.rds")
july_data <- readRDS(file = "uber-raw-data-jul14.rds")
august_data <- readRDS(file = "uber-raw-data-aug14.rds")
september_data <- readRDS(file = "uber-raw-data-sep14.rds")

# combining datasets into one dataframe
all_data <- rbind(april_data, may_data, june_data, july_data, august_data, september_data) 

# changing the schema of the Date.Time column  
all_data$date_time <- mdy_hms(all_data$Date.Time)

# extracting the date and time values
all_data$year <- lubridate::year(all_data$date_time)
all_data$month <- lubridate::month(all_data$date_time)
all_data$day <- lubridate::day(all_data$date_time)
all_data$hour <- lubridate::hour(all_data$date_time)
all_data$minute <- lubridate::minute(all_data$date_time)
all_data$second <- lubridate::second(all_data$date_time)

# setting up the day of week and week number columns for later use in pivot tables
all_data$weekday <- lubridate::wday(all_data$date_time, label=TRUE)
all_data$week <- ceiling(all_data$day / 7)

# making new column for names of months to show month names in the legends
all_data <- all_data %>%
  mutate(month_name = month.name[month])

# creating pivot table for hours
trips_by_hour <- all_data %>%
  group_by(hour) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_hour, "trips_by_hour.csv", row.names = FALSE)

# creating pivot table for hours and months
trips_by_hour_month <- all_data %>%
  group_by(hour, month_name) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_hour_month, "trips_by_hour_month.csv", row.names = FALSE)

# creating pivot table for days and months
trips_by_day_month <- all_data %>%
  group_by(month, month_name, day) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_day_month, "trips_by_day_month.csv", row.names = FALSE)

# This pivot table is identical to the previous one apart from the name. I created it because making
# two graphs from the same pivot table was giving me an error
trips_by_month_day <- all_data %>%
  group_by(month, month_name, day) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_month_day, "trips_by_month_day.csv", row.names = FALSE)

# creating pivot table for days
trips_by_day <- all_data %>%
  group_by(day) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_day, "trips_by_day.csv", row.names = FALSE)

# creating pivot table for days of the week and months
trips_by_weekday_month <- all_data%>%
  group_by(weekday, month) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_weekday_month, "trips_by_weekday_month.csv", row.names = FALSE)

# creating pivot table for months
trips_by_month <- all_data %>%
  group_by(month, month_name) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_month, "trips_by_month.csv", row.names = FALSE)

# creating pivot table for bases and months
trips_by_base_month <- all_data %>%
  group_by(Base, month_name) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_base_month, "trips_by_base_month.csv", row.names = FALSE)

# creating pivot table for hours and days
trips_by_hour_day <- all_data %>%
  group_by(hour, day) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_hour_day, "trips_by_hour_day.csv", row.names = FALSE)

# creating pivot table for months and weeks
trips_by_month_week <- all_data %>%
  group_by(month, week) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_month_week, "trips_by_month_week.csv", row.names = FALSE)

# creating pivot table for bases and days of the week
trips_by_base_day_of_week <- all_data %>%
  group_by(Base, weekday) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(trips_by_base_day_of_week, "trips_by_base_day_of_week.csv", row.names = FALSE)

# creating pivot table for the leaflet map
leaflet_data <- all_data %>%
  group_by(Lat, Lon) %>%
  summarise(Trips = n())

# reducing the number of observations to 100 and ordering from highest to lowest  
leaflet_data_top_locations <- leaflet_data[order(-leaflet_data$Trips), ] %>%
  head(n=100)

# saving pivot table
write.csv(leaflet_data_top_locations, "leaflet_data_top_locations.csv", row.names = FALSE)

# extracting the date value
all_data$date <- as.Date(all_data$date_time)

# creating pivot table for the model
modeling_data <- all_data %>%
  group_by(date) %>%
  summarise(Trips = n())

# saving pivot table
write.csv(modeling_data, "modeling_data.csv", row.names = FALSE)



