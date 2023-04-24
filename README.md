# **Uber Analysis** :car:

## Introduction
- This document will provide a data dictionary, describe the data preparation process, and provide analysis through visualization for six months of data (Apr-Sep 2014) collected from uber trips in the greater New York area.
---
## Data Dictionary :orange_book:
The columns within the initial datasets include the following:
- Date.Time: The date (m/d/Y) and time (h/m/s) of the uber trip
- Lat: The latitude coordinate of the uber location
- Lon: The longitude coordinate of the uber location
- Base: I am unsure what this term refers to
---

## Data Prep :hammer:
1. I bound the data files, changed the schema of the Date.Time column, extracted the individual date and time values, and created columns for the days of the week, the week numbers within a month, and the names of each month.
```
all_data <- rbind(april_data, may_data, june_data, july_data, august_data, september_data) 

all_data$date_time <- mdy_hms(all_data$Date.Time)

all_data$year <- lubridate::year(all_data$date_time)
all_data$month <- lubridate::month(all_data$date_time)
all_data$day <- lubridate::day(all_data$date_time)
all_data$hour <- lubridate::hour(all_data$date_time)
all_data$minute <- lubridate::minute(all_data$date_time)
all_data$second <- lubridate::second(all_data$date_time)

all_data$weekday <- lubridate::wday(all_data$date_time, label=TRUE)
all_data$week <- ceiling(all_data$day / 7)

all_data <- all_data %>%
  mutate(month_name = month.name[month])
```

2. I created pivot tables and saved them to csv files.
```
trips_by_hour <- all_data %>%
  group_by(hour) %>%
  summarise(Trips = n())

write.csv(trips_by_hour, "trips_by_hour.csv", row.names = FALSE)
```

- This was done for all of the pivot tables.
- Within my shiny r script, I read in all of these pivot tables
```
trips_by_hour <- read.csv("trips_by_hour.csv")
```

3. I made a pivot table for the leaflet map, ordered the data from highest to lowest count, and reduced the data to the 100 largest observations.
```
leaflet_data <- all_data %>%
  group_by(Lat, Lon) %>%
  summarise(Trips = n())
 
leaflet_data_top_locations <- leaflet_data[order(-leaflet_data$Trips), ] %>%
  head(n=100)
```
---

## Data Analysis :mag:
1. Set up the UI configuration in the following manner.
```
tabPanel("Trips by Hour",
         div(style = "text-align: center;",
             h1("Uber Trips by Hour"),
             p("The first peak in trips taken per hour can be seen from 7-9 AM, which is likely a product of people 
             traveling to work. From 2-3 AM (the hour with the least trips - 45,865)
             until 6 PM (5-6 PM being the hour with the most trips - 336,190), a general increase in trips can be seen, 
             followed by a gradual decrease until 2 AM. The peak seen between 
             4 and 6 PM is likely a product of people getting off of work and/or going to get dinner."),
         plotOutput('plot_01', height = "675px"),
            
         column(2, tableOutput("trips_by_hour_pivot"))
         )
),
```

- Separate tabs were created for each unique visualization. There are 13 in total.
- leafletOutput() was used for structuring the leaflet visualization rather than plotOutput().

2. Displayed the pivot table outputs for hours and days on their respective tabs.
```
output$trips_by_hour_pivot <- renderTable({
  trips_by_hour})
  
output$trips_by_day_pivot <- renderTable({
  trips_by_day})
```

3. Created bar, stacked bar, and grouped bar charts based on the pivot tables.
```
trips_by_day_month <-
    ggplot(trips_by_day_month, aes(x = day, y = Trips, fill = month_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#C70039", "#0072B2", "#FFC300")) +
    labs(title = "Uber Trips by Day and Month", x = "Day", fill = "Month")  
```
- This was done for seven of the pivot tables.

4. Created heatmaps based on four of the remaining pivot tables.
```
trips_by_hour_day <-
    ggplot(trips_by_hour_day, aes(x=hour, y=day, fill=Trips)) +
    geom_tile() +
    scale_fill_gradient(low="cyan", high="black") +
    labs(title = "Heatmap of Trips by Hour and Day", x = "Hour", y = "Day") +
    theme_minimal()
```

5. Created a leaflet visualization to display where the uber trips were recorded.
```
leaflet_map <-
    leaflet(leaflet_data_top_locations) %>% 
    addTiles() %>% 
    addMarkers(lng = leaflet_data_top_locations$Lon, lat = leaflet_data_top_locations$Lat, label = leaflet_data_top_locations$Trips)
```

6. Created a prediction model for trips by date.
```
output$pred_model <- renderPlot({
    model <- lm(Trips ~ date, data = modeling_data)
    
    modeling_data <- modeling_data %>%
      add_predictions(model)
    modeling_data %>%
      ggplot(aes(date, pred, group = 1)) +
      geom_point(color = "steelblue") +
      geom_smooth(se = FALSE) +
      labs(title = "Trips by Date Prediction Model", x = "Dates", y = "Trips")
  })
```
---

## Shinyapp Link :link:
- https://lukecowan.shinyapps.io/uber/

