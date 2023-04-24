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

#setwd("C:/Users/lukec/OneDrive/Documents/data332/uber_project/pivot_tables")

# reading in pivot tables
trips_by_base_day_of_week <- read.csv("trips_by_base_day_of_week.csv")
trips_by_base_month <- read.csv("trips_by_base_month.csv")
trips_by_day <- read.csv("trips_by_day.csv")
trips_by_day_month <- read.csv("trips_by_day_month.csv")
trips_by_hour <- read.csv("trips_by_hour.csv")
trips_by_hour_day <- read.csv("trips_by_hour_day.csv")
trips_by_hour_month <- read.csv("trips_by_hour_month.csv")
trips_by_month <- read.csv("trips_by_month.csv")
trips_by_month_day <- read.csv("trips_by_month_day.csv")
trips_by_month_week <- read.csv("trips_by_month_week.csv")
trips_by_weekday_month <- read.csv("trips_by_weekday_month.csv")
leaflet_data_top_locations <- read.csv("leaflet_data_top_locations.csv")
modeling_data <- read.csv("modeling_data.csv")

#setting up ui
ui<-fluidPage( 
  
  titlePanel(title = "Explore Uber Data"),
  h4("April-September 2014 Trips"),
  
  # Create a tabset panel with two tabs
  tabsetPanel(
    
    # First tab content
    tabPanel("Trips by Hour",
             div(style = "text-align: center;",
                 h1("Uber Trips by Hour"),
                 p("The first peak in trips taken per hour can be seen from 7-9 AM, which is likely a product of people traveling to work. From 2-3 AM (the hour with the least trips - 45,865)
                    until 6 PM (5-6 PM being the hour with the most trips - 336,190), a general increase in trips can be seen, followed by a gradual decrease until 2 AM. The peak seen between 
                    4 and 6 PM is likely a product of people getting off of work and/or going to get dinner."),
             plotOutput('plot_01', height = "675px"),
             # creating a pivot table object
             column(2, tableOutput("trips_by_hour_pivot"))
             )
    ),
    
    # Second tab content
    tabPanel("Trips by Hour and Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Hour and Month"),
                 p("The trend from the previous chart - an increasing number of trips from 3 AM to 6 PM - is reflected within each month, albeit to varying degrees. Based on the proportion of the stacked bars taken up by each month,
                   we can conclude that September observed the most uber trips taken."),
                 plotOutput('plot_02', height = "700px")
             )
    ),
    
    # Third tab content
    tabPanel("Trips by Day and Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Day and Month"),
                 p("The stacked bar for the thirty-first day is much smaller than the rest because April, June and September end on the 30th. Disregarding that unique circumstance, the fewest trips are taken on the first day of the
                   month, and the greatest number of trips are taken on the 30th. Overall, the number of trips taken throughout the months fluctuate up and down, and this is the case for each individual month as well."),
                 plotOutput('plot_03', height = "675px")
             )
    ),
    
    # Fourth tab content
    tabPanel("Trips by Day",
             div(style = "text-align: center;",
                 h1("Uber Trips by Day"),
                 p("There is no clear upward or downward trend in the number of trips taken throughout the months because the numbers constantly fluctaute between increasing and decreasing values. The 30th day of the six months had
                   the most trips overall with a value of 167,160, and the 31st day had the fewest trips with a value of 78,073. The first day of the 6 months had the lowest average of trips taken with a value of 21,238.33."),
             plotOutput('plot_04', height = "675px"),
             # creating a pivot table object
             column(2, tableOutput("trips_by_day_pivot"))
             )
    ),
    
    # Fifth tab content
    tabPanel("Trips by Weekday and Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Weekday and Month"),
                 p("Although it varies based on the month, we can see that the most uber rides are typically taken on Thursdays and Fridays. April is the only exception to this rule. The relative amounts of rides taken each month generally increase,
                   which contributes to the upward trend that can be viewed regarding overall trips taken from April to September. Note: The legend and fill values were in sequential order in my pivot table script, but it converted to alphabetical here for some reason"),
                 plotOutput('plot_05', height = "675px")
             )
    ),
    
    # Sixth tab content
    tabPanel("Trips by Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Month"),
                 p("As previously mentioned, there is a consistent upward trend in the number of trips taken by month. There is an especially large jump between August and September. April had the fewest trips recorded with a value of 564,516, and September 
                   had the most recorded trips with a value of 1,028,136."),
                 plotOutput('plot_06', height = "700px")
             )
    ),
    
    # Seventh tab content
    tabPanel("Trips by Base and Month",
             div(style = "text-align: center;",
                 h1("Uber Trips by Base and Month"),
                 p("Among the charts with three variables we have looked at thus far, Base and Month have the lowest correlation in regards to the number of trips taken. This phenomenon can be viewed in the case of trips associated with base B02764 in September 2014.
                   There were more trips taken in that month than in the other months combined for this specific base. This along with the large September value of base B02617 could explain why there was such a large jump in trips taken between August and September.
                   Substantially less trips were associated with bases B02512 and B02764 than there were with the other 3."),
                 plotOutput('plot_07', height = "650px")
             )
    ),
    
    # Eighth tab content
    tabPanel("Trips by Hour and Day Heatmap",
             div(style = "text-align: center;",
                 h1("Uber Trips by Hour and Day"),
                 p("This heat map captures the trends identified in the hour bar chart from earlier. Dark patches, which denote a greater number of trips, can be seen to occur between 3 and 10 PM, while lighter patches occur between midnight and 5 AM. The tile values
                   for the 31st day are all lighter relative to the other days because April, June, and September end on the 30th."),
                 plotOutput('plot_08', height = "675px")
             )
    ),
    
    # Ninth tab content
    tabPanel("Trips by Month and Day Heatmap",
             div(style = "text-align: center;",
                 h1("Uber Trips by Month and Day"),
                 p("This chart more effectively captures the trends previously mentioned, those being the constant fluctuation in trips taken throughout the days of the month as well as an increase in the number of trips taken each month compared to the last. A few interesting
                   cells (dates) to note are April 30th, May 25th and 26th, and July 5th and 6th due to their contrast in shading with the days that precede and follow them. The nonexistent 31st dates for April, June, and September are clearly represented in this visualization."),
                 plotOutput('plot_09', height = "675px")
             )
    ),
    
    # Tenth tab content
    tabPanel("Trips by Month and Week Heatmap",
             div(style = "text-align: center;",
                 h1("Uber Trips by Month and Week"),
                 p("The darkening of the cells as we move along the x-axis signifies the increase in trips taken from month to month. The first week of July is quite light relative to the other first weeks and the week that follows it; recall that July 5th and 6th had exceptionally low 
                   numbers of trips taken. The week 5 cells are much lighter compared to the cells from other weeks becuase only 2-3 days lie within this week specification."),
                 plotOutput('plot_10', height = "675px")
             )
    ),
    
    # Eleventh tab content
    tabPanel("Trips by Base and Weekday",
             div(style = "text-align: center;",
                 h1("Uber Trips by Base and Weekday"),
                 p("There is an upward trend in trips taken by weekday from Sunday (lowest total trips observed) through Friday (highest total trips observed). The shading for the bases relative to the day of the week are consistent as can be observed by the fact that the changes in shade
                   (whether lighter or darker) correspond with the other bases. Note: Moving this visualization from the pivot table script changed the weekday chronology from sequential to alphabetical, and I could not figure out how to revert it."),
                 plotOutput('plot_11', height = "675px")
             )
    ),
    
    # Twelfth tab content
    tabPanel("Leaflet map",
             div(style = "text-align: center;",
                 h1("Top 100 Most Common Pick-up Locations"),
                 p("While the entire dataset includes data points in a much larger variety of regions in New York and New Jersey, I wanted to zone in on the three hotspots for uber trips. The first and least prominent of these is in lower Manhattan, especially at or near the Meatpacking District.
                   Second on our list of common trip areas are various terminals (4, 5, 7, and 8) at JFK International Airport. Our last and most prominent hotspot is located at terminals A, B, and C of Edo Seaplane Base. These terminals alone account for a considerable amount of the top uber ride locations within this dataset."),
                 leafletOutput('map', height = "675px")
             )
    ),
    
    # Thirteenth tab content
    tabPanel("Trips by Date Prediction Model",
             div(style = "text-align: center;",
                 h1("Uber Trips by Date Prediction Model"),
                 p("This model displays the total number of trips taken each day throughout the six month period. Each date observation is represented by a point. The predictive line of best fit tells us that the number of uber trips taken steadily rises from the beginning of April to late May/early June. At this point, the number 
                   of trips remains stagnant for about two weeks before increasing again at a gradual pace for the remaining months through September with a slight uptick between August and September. Note: 183 date values are shown at the bottom becuase I could not figure out how to segment the axis labels by specific intervals."),
                 plotOutput('pred_model', height = "675px")
             )
    ),
                   
  )
)



server<-function(input,output){
  
  # displaying hours pivot table
  output$trips_by_hour_pivot <- renderTable({
    trips_by_hour})
  
  # displaying days pivot table
  output$trips_by_day_pivot <- renderTable({
    trips_by_day})
  
  # creating hourly chart
  trips_by_hour_chart <-
    ggplot(trips_by_hour, aes(x = hour, y = Trips, fill = as.factor(hour))) +
    geom_col(show.legend = FALSE) +
    labs(title = "Uber Trips by Hour", x = "Hour")  
  
  # creating hourly month chart
  trips_by_hour_month <-
    ggplot(trips_by_hour_month, aes(x = hour, y = Trips, fill = month_name)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Uber Trips by Hour and Month", x = "Hour", fill = "Month")
  
  # creating daily month chart
  trips_by_day_month <-
    ggplot(trips_by_day_month, aes(x = day, y = Trips, fill = month_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#C70039", "#0072B2", "#FFC300")) +
    labs(title = "Uber Trips by Day and Month", x = "Day", fill = "Month")  
  
  # creating daily chart 
  trips_by_day_chart <-
    ggplot(trips_by_day, aes(x = day, y = Trips, fill = day)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Uber Trips by Day", x = "Day") 
  
  #creating days of the week chart
  trips_by_weekday_month <-
    ggplot(trips_by_weekday_month, aes(x = month, y = Trips, fill = weekday)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#C70039", "#0072B2", "#FFC300", "#FF5733")) +
    labs(title = "Uber Trips by Weekday and Month", x = "Month", fill = "Weekday") 
  
  # creating monthly chart
  trips_by_month <-
    ggplot(trips_by_month, aes(x = month, y = Trips, fill = month_name)) +
    scale_fill_manual(values = c("#FF5733", "#7D3C98", "#348000", "#3498DB", "#E74CEC", "#FFC300")) +
    geom_col(show.legend = FALSE) +
    #titling and labeling the plot
    labs(title = "Uber Trips by Month", x = "Month")
  
  # creating bases and month chart
  trips_by_base_month <-
    ggplot(trips_by_base_month, aes(x = Base, y = Trips, fill = month_name)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.9) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#C70039", "#0072B2", "#FFC300")) +
    #titling and labeling the plot
    labs(title = "Uber Trips by Base and Month", x = "Base", fill = "Month")
  
  # creating hourly day heatmap
  trips_by_hour_day <-
    ggplot(trips_by_hour_day, aes(x=hour, y=day, fill=Trips)) +
    geom_tile() +
    scale_fill_gradient(low="cyan", high="black") +
    labs(title = "Heatmap of Trips by Hour and Day", x = "Hour", y = "Day") +
    theme_minimal()
  
  # creating monthly day heatmap
  trips_by_month_day <-
    ggplot(trips_by_month_day, aes(x=month, y=day, fill=Trips)) +
    geom_tile() +
    scale_fill_gradient(low="yellow", high="black") +
    labs(title = "Heatmap of Trips by Month and Day", x = "Month", y = "Day") +
    theme_minimal()
  
  # creating monthly week heatmap
  trips_by_month_week <-
    ggplot(trips_by_month_week, aes(x=month, y=week, fill=Trips)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="darkblue") +
    labs(title = "Heatmap of Trips by Month and Week", x = "Month", y = "Week") +
    theme_minimal() 
  
  # creating bases and day of week heatmap
  trips_by_base_day_of_week <-
    ggplot(trips_by_base_day_of_week, aes(x=weekday, y=Base, fill=Trips)) +
    geom_tile() +
    scale_fill_gradient(low="cyan", high="darkorange") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title = "Heatmap of Trips by Base and Weekday", x = "Weekday")
  
  # creating leaflet map for top 100 pick-up spots
  leaflet_map <-
    leaflet(leaflet_data_top_locations) %>% 
    addTiles() %>% 
    addMarkers(lng = leaflet_data_top_locations$Lon, lat = leaflet_data_top_locations$Lat, label = leaflet_data_top_locations$Trips)
  
  # creating linear prediction model
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
  
    # displaying chart visualizations
    output$plot_01 = renderPlot({trips_by_hour_chart})
    output$plot_02 = renderPlot({trips_by_hour_month})
    output$plot_03 = renderPlot({trips_by_day_month})
    output$plot_04 = renderPlot({trips_by_day_chart})
    output$plot_05 = renderPlot({trips_by_weekday_month})
    output$plot_06 = renderPlot({trips_by_month})
    output$plot_07 = renderPlot({trips_by_base_month})
    output$plot_08 = renderPlot({trips_by_hour_day})
    output$plot_09 = renderPlot({trips_by_month_day})
    output$plot_10 = renderPlot({trips_by_month_week})
    output$plot_11 = renderPlot({trips_by_base_day_of_week})
    output$map = renderLeaflet({leaflet_map})
    
  }
  


shinyApp(ui=ui, server=server) 
