#### Scraping surf forcast and spot location data
### Written by Steve Scherrer

#### The following script scrapes the current surf report from surfline and 
#### regional surf info from wannasurf.com for the purpose of determining 
#### the current swell and if a spot is likely to benifit under climate change
##################################################################################

## Installing principle dependencies
# install.packages('rvest')
library(rvest)
library(tidyverse)
## Scraping surf forcasts from surfline
steamer_lane = "https://www.surfline.com/surf-report/steamer-lane/5842041f4e65fad6a7708805"
## Scrape forcast
forecast <- read_html(steamer_lane) %>%
  html_nodes(".quiver-surf-height , .quiver-reading , #observed-wave-range, #observed-wind-conditions") %>%
  html_text() 
## Extract current wave height and forcasted wave height
wave_height = forecast[1]
suggestion = forecast[3]

### Scraping Wannasurf.com
test_url = "https://wannasurf.com/spot/North_America/USA/Hawaii/Oahu/canoes/index.html"
# html(test_url)

## Testing a method for extracting pertinant data
scraped_page = read_html(test_url)   %>%
  html_nodes("#wanna-item-specific-2columns-right") %>%
  html_text() 

page_data = data.frame()
split_data = strsplit(scraped_page, split = "\n")
good_swell_direction = strsplit(split_data[[1]][3], split = "Good swell direction")[[1]][2]
good_wind_direction = strsplit(split_data[[1]][4], split = "Good wind direction")[[1]][2]
swell_size = strsplit(split_data[[1]][5], split = "Swell size")[[1]][2]
best_tide_position = strsplit(split_data[[1]][6], split = "Best tide position")[[1]][2]
best_tide_movement = strsplit(split_data[[1]][7], split = "Best tide movement")[[1]][2]
week_crowd = strsplit(split_data[[1]][10], split = "Week crowd")[[1]][2]
weekend_crowd = strsplit(split_data[[1]][11], split = "Week-end crowd")[[1]][2]
page_data = rbind(page_data, data.frame(good_swell_direction, good_wind_direction, swell_size, best_tide_position, best_tide_movement, weekend_crowd, weekend_crowd))

## Using RCrawler to parse site's directory
read_html("https://wannasurf.com/spot/") %>%
  follow_link(i="Algeria") %>%
  html_text()

wanna_surf_spot_url = "https://wannasurf.com/spot/"
spot_links = c()

## Extracting and looping through the list of directory links
global_links = LinkExtractor(wanna_surf_spot_url)[[2]]
for(i in 1:length(global_links)){
  ## Extracting only links pointing to regional spot data
  if('spot' %in% strsplit(global_links[i], split = "/")[[1]] & !('index.html' %in% strsplit(global_links[i], split = "/")[[1]])){
    region_links = LinkExtractor(global_links[i])[[2]]
    ## We now have a vector of region links. We only want to extract links that build off these region links.
    global_components = strsplit(global_links[i], split = "/")[[1]]
    if("index.html" %in% global_components){
      global_link = paste(global_components[-which(global_components == "index.html")],collapse = "/")
    } else {
      global_link = global_links[i]
    }
    for(j in 1:length(region_links)){
      region_component = strsplit(region_links[j], split = global_link)[[1]][2]
      if(!is.na(region_component)){
        if(!any(c("photo", "video", "comment") %in% strsplit(region_component, split = "/")[[1]]) & region_links[j] != global_links[i]){
          print(paste('i = ', i))
          print(paste('j = ', j))
          print(region_links[j])
          spot_links = c(spot_links, region_links[j])
        }
      }
    }
  }
}


## Recursively extracting all relevant child links from each global link. Digging down the heirarchy if you will.
while(parent_link != global_links[length(global_links)])
  # get child links
  child_links = LinkExtractor(parent_link)[[2]]
# loop through child links
for(j in 1:length(child_links)){
  # If the child link isn't the same as the parent
  if(nchar(child_links[j]) != nchar(parent_link)){
    # split on the phrase index.html
    children = strsplit(child_links[j], split = strsplit(parent_link, split = "index.html")[[1]][1])[[1]]
    if(length(children) > 1){
      # If the link contains the phrase 'index.html'
      if(all(strsplit(global_links[i], split = "/")[[1]] %in% strsplit(child_links[j], split = "/")[[1]])){
        # Keep splitting
        split_children = strsplit(children[2], split = "/")[[1]]
        # If we've hit the link's rock bottom
        if(!any(c("photo", "video", "comment") %in% split_children)){
          ## Scrape the page
          scraped_page = read_html(parent_link)   %>%
            html_nodes("#wanna-item-specific-2columns-right") %>%
            html_text() 
          # Write out relvant infromation
          split_data = strsplit(scraped_page, split = "\n")
          good_swell_direction = strsplit(split_data[[1]][3], split = "Good swell direction")[[1]][2]
          good_wind_direction = strsplit(split_data[[1]][4], split = "Good wind direction")[[1]][2]
          swell_size = strsplit(split_data[[1]][5], split = "Swell size")[[1]][2]
          best_tide_position = strsplit(split_data[[1]][6], split = "Best tide position")[[1]][2]
          best_tide_movement = strsplit(split_data[[1]][7], split = "Best tide movement")[[1]][2]
          week_crowd = strsplit(split_data[[1]][10], split = "Week crowd")[[1]][2]
          weekend_crowd = strsplit(split_data[[1]][11], split = "Week-end crowd")[[1]][2]
          # Save all of this out to our data frame
          page_data = rbind(page_data, data.frame(good_swell_direction, good_wind_direction, swell_size, best_tide_position, best_tide_movement, weekend_crowd, weekend_crowd))
          
        }
      }
    }
  }
}

### Now need to filter out links to other pages
country_links = LinkExtractor(region_links[14])[[2]]
spot_links = LinkExtractor(country_links[231])[[2]]

test_url = "https://wannasurf.com/spot/Africa/Algeria/AinBarbar/index.html"

scraped_page = read_html(test_url)   %>%
  html_nodes("#wanna-item-specific-2columns-right") %>%
  html_text() 


### Getting Tide data 
install.packages('TIDER')
