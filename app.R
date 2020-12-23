# This app uses ocean data from NOAA's National Data Buoy Center (NDBC) to assess
# current surf and wind conditions for Central California. As well, the surf 
# prediction data generated from NOAA's Wave Watch III (WWIII) model is visualized
# for the Eastern N. Pacific. 

library(rNOMADS)
library(GEOmap)
library(plotly)
library(shiny)
library(rnoaa)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ncdf4)
library(htmltools)
theme_set(theme_light())


# This post explains how to plot the NOAA's Wave Watch III model data: 
# https://bovineaerospace.wordpress.com/tag/noaa/

swell_map <- function(tbl) { 
  model.urls <- GetDODSDates("wave")
  latest.model <- model.urls$url[14]
  model.runs <- GetDODSModelRuns(latest.model) # Eastern N. Pacific
  latest.model.run <- tail(model.runs$model.run, 1)
  time <- c(0,0)
  lon <- c(0, 274)
  lat <- c(0, 202)
  
  wave.data <- DODSGrab(latest.model, latest.model.run,
                        "htsgwsfc", time, lon, lat)
  wave.grid <- ModelGrid(wave.data, c(0.25, 0.25))
  #Remove "no data" values
  wave.grid$z[which(wave.grid$z>1e10, arr.ind=TRUE)] <- NA
  colormap <- rainbow(500, start=0, end=5/6)
  image(wave.grid$x, sort(wave.grid$y), wave.grid$z[1,1,,], col = colormap,
        xlab = "Longitude", ylab = "Latitude",
        main = paste("Pacific Swell Activity for ", format(Sys.Date(), "%B %d, %Y")), useRaster = TRUE)
  plotGEOmap(coastmap, border = "black", add = TRUE,
             MAPcol = "black") 
}

# Here we access data from NOAA's National Data Buoy Center. We will use data 
# streaming from buoy 46042

# Leaflet map of buoys around Santa Cruz
west_coast <- buoy_stations() %>% 
  filter(between(lon, -150, -122) & between(lat, 20, 37.5)) 

central_coast <- leaflet(data = na.omit(west_coast),
                         options = leafletOptions(zoomControl = FALSE,
                                                  minZoom = 8, maxZoom = 8,
                                                  dragging = FALSE)) %>% 
  addTiles() %>% 
  addCircleMarkers(~lon, ~lat, label = ~htmlEscape(paste0('buoyID: ', station))) %>%
  addScaleBar('bottomleft') %>%
  addControl('NDBC buoys near SC', position = "topleft")

# import data from buoy 46042
sc_waves <- buoy(dataset = "stdmet", buoyid = 46042, year = 9999)

# Clean data
sc <- sc_waves$data %>% tail(n= 10000) %>% 
  mutate(time = as.Date(time)) %>% 
  rename(date = time) %>% 
  arrange(desc(date)) %>% 
  drop_na(wave_height)

# Plot wind data
wind_data <- sc %>% 
  drop_na(wind_spd) %>% 
  group_by(date) %>% 
  summarize(wind_spd = mean(wind_spd),
            wind_dir = mean(wind_dir),
            gust = mean(gust)) %>% 
  ungroup() %>% 
  ggplot(aes(date, wind_spd, fill = gust)) +
  geom_col() +
#  geom_line(aes(group = wind_spd), alpha = 0.5, size = 1) +
  scale_fill_viridis_c(name = "Wind gusts",
                       option = "C") +
  
  labs(
    x = '',
    y = 'Wind speed'
  )

# Plot wave and wave period data
waves_col <- sc %>% 
  group_by(date) %>% 
#  filter(date > '2020-11-15') %>% 
  summarize(wave_height = mean(wave_height),
            dominant_wpd = mean(dominant_wpd)) %>% 
  ungroup() %>% 
  ggplot(aes(date, wave_height, fill = dominant_wpd)) +
  geom_col() +
#  geom_line(alpha = 0.5, size = 1) +
  scale_fill_viridis_c(name = "Wave period",
                       option = "D") +
  
  labs(
    x = '',
    y = 'Wave height'
  )

# Removing this plot
# wave_data <- sc %>% 
#   group_by(date) %>% 
#   summarize(wave_height = mean(wave_height),
#             dominant_wpd = mean(dominant_wpd)) %>% 
#   ungroup() %>% 
#   ggplot(aes(date, wave_height, fill = dominant_wpd)) +
#   geom_col() +
#   geom_line(alpha = 0.5, size = 1.5) +
#   scale_color_gradient2(low="green", mid="black",
#                         high="red", midpoint = 12, space ="Lab")
# 

# Creating dashboard
ui <- dashboardPage(
  dashboardHeader(title = h2("Santa Cruz Surf and Wind Reporter")),
  dashboardSidebar(
  disable = TRUE),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"), 
    fluidRow(box("Buoy map",
                 leafletOutput('sc_buoys')),
    box("wave prediction map",
                 plotOutput("WWIII"))), 
    fluidRow(box("Surf height and period from buoy 46042",
                 plotlyOutput("buoy")),
    box("wind data from buoy 46042",
                 plotlyOutput("wind")))
  ))

server <- function(input, output, session) {
  # sidebar
  output$sc_buoys<- renderLeaflet({
    central_coast
  })
  
  #------- dashboard body ------------
  
  # wave prediction map for West Coast
  output$WWIII <- renderPlot({
    swell_map()
  })
  
  # buoy data for wave height and period
  output$buoy <- renderPlotly({
    ggplotly(waves_col)
  })
  
  # wind data
  output$wind <- renderPlotly({
    ggplotly(wind_data)
  })
  
  # Deaths per million by State map
  output$waves <- renderPlot({
    wave_data
  })
  
}

shinyApp(ui = ui, server = server)
