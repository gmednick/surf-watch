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
library(readr)
library(ncdf4)
library(htmltools)
library(rtide)
library(scales)
library(googlesheets4)
theme_set(theme_light())


# This post explains how to plot the NOAA's Wave Watch III model data: 
# https://bovineaerospace.wordpress.com/tag/noaa/

swell_map <- function(tbl) { 
#  model.urls <- GetDODSDates("wave")
  model.urls <- read_rds('model_url.rds')
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
# west_coast <- buoy_stations() %>% 
#   filter(between(lon, -150, -122) & between(lat, 20, 37.5)) %>% 
#   drop_na() %>% 
#   write_rds('nearby_buoys.rds')

dat <- read_rds('nearby_buoys.rds')
central_coast <- leaflet(data = dat,
                         options = leafletOptions(zoomControl = FALSE,
                                                  minZoom = 8, maxZoom = 8,
                                                  dragging = FALSE)) %>% 
  addTiles() %>% 
  addCircleMarkers(~lon, ~lat, label = ~htmlEscape(paste0('buoyID: ', station))) %>%
  addScaleBar('bottomleft') %>%
  addControl('NDBC buoys near SC', position = "topleft")

# Import wave direction data 
# sc_dir <- buoy(dataset = "swden", buoyid = 46042, year = 9999)
# sc_swell_dir <- sc_dir$data %>%  tail(1000) %>% arrange(desc(time)) %>% 
#   drop_na(principal_wave_dir) %>% 
#   slice_head(n = 1) %>% 
#   select(wave_direction = mean_wave_dir) 
sc_swell_dir <- read_rds('sc_swell_dir.rds')
#sc_swell_dir <- read_sheet("https://docs.google.com/spreadsheets/d/1TK0JW4ByrXUUWHgzu33cahgYJ-CGip3N6ar4Tba7W4M/edit#gid=0")
# Import tide data
tides <- tide_height(stations = "Monterey Harbor",
            minutes = 1L,
            from = as.Date(Sys.Date()),
            to = as.Date(Sys.Date()),
            tz = "PST8PDT", harmonics = rtide::harmonics) %>% 
  mutate(tide = TideHeight * 3.28,
         time = format(DateTime, "%I:%M %p", tz = "PST8PDT"))

p <- tides %>%
  ggplot(aes(DateTime, tide)) +
  geom_line(aes(group = 1)) +
  scale_x_datetime(
    name = "Date",
    labels = label_date("%I:%M %p", tz = "PST8PDT")
  ) +
  scale_y_continuous(name = "Tide Height (ft)") +
  labs(title = paste0("Tide chart for ", format(Sys.Date(), '%b %d, %Y'))) +
  ggplot2::aes(text = paste0(
    "Time: ", time,
    "\nTide: ", paste0(round(tide, 1), ' ft')
  ))

current_tide <- tides %>% 
  filter(DateTime == round(Sys.time(), 'mins')) %>% 
  select(tide)

# import data wave, period, wind and wind direction data from buoy 46042
# sc_waves <- buoy(dataset = "stdmet", buoyid = 46042, year = 9999, limit=500, add_units = TRUE)
# 
# # Clean data
# sc <- sc_waves$data %>% tail(n= 10000) %>% 
#   mutate(time = as.Date(time)) %>% 
#   rename(date = time) %>% 
#   arrange(desc(date)) %>% 
#   drop_na(wave_height)
sc <- read_rds('sc_waves.rds')
#sc <- read_sheet("https://docs.google.com/spreadsheets/d/1xhFx2bvq0h7pXYby-NDD9GUIKvrDTn54Dy3WCT6Tvq8/edit#gid=0")
buoy_reads <- sc %>% slice_head(n = 1) %>% 
  select(date, wind_spd, wind_dir, wave_height, dominant_wpd) %>% 
  mutate(date = format(date, "%b %d, %Y"),
         wave_height = round(wave_height*3.28, 1),
         wind_spd = round(wind_spd, 1)
  )

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
  ) +
  ggplot2::aes(text = paste0(
    "Date: ", format(date, format = " %a, %b %d"),
    "\nWind speed: ", paste0(round(wind_spd, 1), ' mph'),
    "\nWind direction: ", paste0(round(wind_dir), " \u00b0"),
    "\nGusts: ", paste0(round(gust, 1), ' mph')
    ))

# Plot wave and wave period data
waves_col <- sc %>% 
  group_by(date) %>% 
  summarize(wave_height = mean(wave_height*3.28),
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
  ) +
  ggplot2::aes(text = paste0(
    "Date: ", format(date, format = '%a, %b %d'),
    "\nWave height: ", paste0(round(wave_height, 1), ' ft'),
    "\nDominant period: ", paste0(round(dominant_wpd, 1), " secs"),
    "\nWave direction: ", paste0(round(sc_swell_dir$wave_direction), " \u00b0")
  ))

# Creating dashboard
ui <- dashboardPage(
  dashboardHeader(title = h2("Santa Cruz Surf and Wind Reporter")),
  dashboardSidebar(
  disable = TRUE),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"),
    fluidRow(valueBoxOutput("wave_hgt", width = 4),
             valueBoxOutput("wave_period", width = 4),
             valueBoxOutput("wave_dir", width = 4)),
    fluidRow(valueBoxOutput("wind_speed", width = 4),
             valueBoxOutput("wind_dir", width = 4),
             valueBoxOutput("tide", width = 4)),
    fluidRow(box("wave prediction map",
                 plotOutput("WWIII")),
             box("Buoy map",
                 leafletOutput('sc_buoys'))), 
    fluidRow(box("Surf height and period from buoy 46042",
                 plotlyOutput("buoy")),
    box("wind data from buoy 46042",
                 plotlyOutput("wind"))),
    fluidRow(box("tides",
                 plotlyOutput("tidesD"), width = 12, height = 2))
  ))

server <- function(input, output, session) {
  
  output$wave_hgt <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(buoy_reads$wave_height), " ft"),
      subtitle = paste0("Wave height for ", buoy_reads$date),
      color = 'aqua',
      icon =icon("fas fa-water") 
    )
  })
  output$wave_period <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(buoy_reads$dominant_wpd), " sec"),
      subtitle = "Wave period",
      color = 'aqua',
      icon =icon("fas fa-water")
    )
  })
  output$wave_dir <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(sc_swell_dir$wave_direction),  " \u00b0"),
      subtitle = "swell direction",
      color = 'aqua',
      icon =icon("fas fa-compass") 
    )
  })
  output$wind_speed <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(buoy_reads$wind_spd), " mph"),
      subtitle = "Wind speed",
      color = 'teal',
      icon =icon("fas fa-wind") 
    )
  })
  output$wind_dir <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(buoy_reads$wind_dir),  " \u00b0"),
      subtitle = "Wind direction",
      color = 'teal',
      icon =icon("far fa-compass") 
    )
  })
  output$tide <- renderValueBox({
    valueBox(
      value = paste0(prettyNum(round(current_tide$tide, 2)), " ft"),
      subtitle = "Current tide",
      color = 'green',
      icon =icon("fas fa-swimming-pool") 
    )
  })
  output$sc_buoys <- renderLeaflet({
    central_coast
  })
  
  #------- dashboard body ------------
  
  # wave prediction map for West Coast
  output$WWIII <- renderPlot({
    swell_map()
  })
  
  output$tidesD <- renderPlotly({
    ggplotly(p, tooltip = 'text')
  })
  
  # buoy data for wave height and period
  output$buoy <- renderPlotly({
    ggplotly(waves_col, tooltip = 'text')
  })
  
  # wind data
  output$wind <- renderPlotly({
    ggplotly(wind_data, tooltip = 'text')
  })
  
  # 
  output$waves <- renderPlot({
    wave_data
  })
  
  
}

shinyApp(ui = ui, server = server)
