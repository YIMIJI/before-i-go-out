library(shiny)
library(shinyjs)
library(shinyTime)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(s2)
library(sp)
library(googleway)
library(rgdal)
library(stringr)
library(raster)
library(mapsapi)
library(sf)
library(geosphere)

# Dear Prof/TA, for this version of the app to work properly aesthetically,  all packages needs to be updated to the latest version.
# Some key packages to check would be leaflet (version 2.1.1) 

towns <- read.csv("Singapore-town.csv") # based on weather forecast api(area) & Wikipedia (region)
names(towns) <- c("area","region")
hdbcarparks <- read.csv("hdb_carparks_final.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output){
  
  # -----------------------------------Functions-----------------------------------
  # Both reactive functions and functions
  
  getRainData <- reactive({
    date <- as.character(input$date)
    time <- strftime(input$time,"%T")
    
    print("New Rain Data")
    
    url = "https://api.data.gov.sg/v1//environment/rainfall?date_time="
    url <- paste0(url,date,"T",time)
    r_data <- fromJSON(url)
    
    rain_reading <- as.data.frame(r_data$items$readings)
    rain_loc <- as.data.frame(r_data$metadata$stations) %>% dplyr::select(location)
    rain_loc$lat <- rain_loc$location$latitude
    rain_loc$long <- rain_loc$location$longitude
    rain_loc <- rain_loc %>% dplyr::select(lat,long)
    
    rain_data <- cbind(rain_reading,rain_loc)
    colnames(rain_data) <- c("station","forecast","lat","long")
    rain_data
  
  })

  # For some spatial values the output obtained is in the form of "integer(0)" which
  # cannot be manipulated to return any boolean output therefore this function will
  # cater to that  by checking if it is an integer and if the length is 0

  getIsInteger0 <- function(x){
    is.integer(x) && length(x) == 0L
  }
  
  getIntersectingAreas <- function(r, rain_int){
    counter = 0
    Polygon_names <- as.data.frame(rain_int@data[,2])
    # Coerce to spatial format
    route <- as_Spatial(r$geometry)
    route <- st_as_sf(route)
    # Set crs basis
    route <- st_set_crs(route, 4326)
    # Obtain the "POINT" format of the route to extract start and stop coordinates
    pt <- st_cast(route, "POINT")
    start <- pt[1,]
    end <- pt[nrow(pt),]
    for (i in 1:nrow(rain_int)){
      poly <- rain_int[i,]
      poly1 <- st_as_sf(poly)
      poly2 <- st_set_crs(poly1, 4326)
      # Some packages uses s2 instead of s4 format of the spatial data, we deactivate the use of s2
      sf::sf_use_s2(FALSE)
      # Intersect  polygon and line
      joint <- st_intersects(route,poly2)
      rte <- st_intersection(route,poly2)
      
      if (getIsInteger0(joint[[1]])) {
        Polygon_names$intersect[i] <- 0
      }
      else {
        Polygon_names$intersect[i] <- 1
        # Obtain length of route
        rte_length <- st_length(rte)
        counter <- counter + 1
        if (counter == 1){
          route_poly <- poly
          rte_length_set <- rte_length
        }
        else {
          route_poly <- rbind(route_poly, poly)
          rte_length_set <- rbind(rte_length, rte_length_set)
        }
      }
    }
    result_list <- list(Polygon_names, route_poly, route, rte_length_set, start, end)
    return(result_list)
  }
  
  getWeatherData <- reactive({
    date <- as.character(input$date)
    time <- strftime(input$time,"%T")
    
    print("New weather Data")
    
    url = "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time="
    url <- paste0(url,date,"T",time)
    w_data <- fromJSON(url)
    
    weather_forecast <- as.data.frame(w_data$items$forecasts)
    weather_loc <- as.data.frame(w_data$area_metadata$label_location)
    wf_data <- cbind(weather_forecast,weather_loc)
    colnames(wf_data) <- c("area","forecast","lat","long")
    wf_data
  })
  
  getTempData <- reactive({
    date <- as.character(input$date)
    time <- strftime(input$time,"%T")
    
    print("New temp data")
    
    url = "https://api.data.gov.sg/v1/environment/air-temperature?date_time="
    url <- paste0(url,date,"T",time)
    t_data <- fromJSON(url)
    
    temp_forecast <- as.data.frame(t_data$items$readings) %>% dplyr::select(value)
    
    temp_loc <- as.data.frame(t_data$metadata$stations) %>% dplyr::select(name,location)
    temp_loc$lat <- temp_loc$location$latitude
    temp_loc$long <- temp_loc$location$longitude
    temp_loc <- temp_loc %>% dplyr::select(name,lat,long)
    
    temp_data <- cbind(temp_forecast,temp_loc)
    
    colnames(temp_data) <- c("temp","area","lat","long")
    temp_data
  })
  
  getLocation <- function(StartLoc,EndLoc){
    
    # Route API
    ggmap::register_google(key = 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew')
    key <- 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew'
    
    # Google Directions API
    
    doc <- 
      mp_directions(
        origin = StartLoc, 
        destination = EndLoc, 
        alternatives = FALSE,
        key = key,
        quiet = TRUE
      )
    r <- # tryCatch({
      mp_get_routes(doc)
    # },
    # error = function(e) {
    #   message('No data in chosen Analysis.', e)
    #   showModal(
    #     modalDialog(
    #       div("Inputs might be over or under specified, please revise your inputs")
    #       ,easyClose = TRUE)
    #   )},
    # finally={ # important to include finally = 
    #   message("\n End of the getLocation function")
    # })
  }
  
  getRealTimeCarparkAvailability <- function(datetime) {
    # Car park Availability API
    ggmap::register_google(key = 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew')
    key <- 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew'
    
    datetime_in <- datetime %>% substr(1, 19) %>% gsub(" ", "T", .)
    url <- paste0("https://api.data.gov.sg/v1/transport/carpark-availability?date_time=", datetime_in)
    a <- as.data.frame(fromJSON(url))
    a1 <- a$items.carpark_data[[1]]
    
    for(i in 1:nrow(a1)){
      carpark_info <- a1[[1]][[i]]
      carpark_info <- carpark_info[carpark_info$lot_type == "C"] #focus only on C type lots
      if (length(carpark_info) == 3){
        a1$total_lots[[i]] <- carpark_info$total_lots[carpark_info$lot_type == "C"]
        a1$lot_type[[i]] <- carpark_info$lot_type[carpark_info$lot_type == "C"]
        a1$lots_available[[i]] <- carpark_info$lots_available[carpark_info$lot_type == "C"]
      }
    }
    
    a1<- a1 %>% as.data.frame() %>% dplyr::select(2:6)
    return(a1)
  }
  
  getNearestHDBCarparks <- function(destination){
    key <- 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew'
    doc <- mp_geocode(
      addresses = destination,
      key = key,
      quiet = TRUE)
    
    pnt <- mp_get_points(doc)
    pnt2 <- pnt %>% mutate(lon = st_coordinates(pnt)[,1], lat = st_coordinates(pnt)[,2]) %>% as.data.frame()
    input_coord <- pnt2 %>% dplyr::select(lon,lat)
    
    hdb_coords <- hdbcarparks %>% dplyr::select(lon,lat)
    distances <- distm(input_coord, hdb_coords, fun=distGeo)[1,] 
    distances_sorted <- sort(distances)
    
    nearest_3 <- data.frame()
    for (i in 1:3) {
      current_distance <- distances_sorted[i]
      id <- which(distances==current_distance)
      current_info <- data.frame(name = hdbcarparks$car_park_no[id], 
                                 dist = as.integer(round(current_distance, 0)),
                                 lon = hdbcarparks$lon[id], 
                                 lat = hdbcarparks$lat[id])
      nearest_3 <- rbind(nearest_3, current_info)
    }
    return(nearest_3)
  }
  
  getDuration <- function(result_list){
    loc <- text_reactive()
    r <- getLocation(loc[[1]], loc[[2]])
    distance <- r$distance_text
    shapes <- getMap()
      
    # Duration data for users
    rte_length <- as.data.frame(result_list[[4]])
    rte_length$route_area <- result_list[[2]]@data$PLN_AREA_N
    colnames(rte_length) <- c("route_length","route_area")
    rte_length <- rte_length %>% mutate(Percent_of_route = route_length/r$distance_m) %>% mutate(Duration_of_route = (route_length/sum(route_length))*r$duration_s)
    rain_set <- cbind(shapes[[3]]@data$PLN_AREA_N, shapes[[3]]@data$rain)
    colnames(rain_set) <- c("route_area","rain")
    rte_length <- merge(rte_length, rain_set, by = "route_area")
    rte_length <- rte_length %>% mutate(Expected_duration =  round((as.numeric(rain)*0.125+1)*Duration_of_route),2)
    
    Original_Duration <- round(sum(rte_length$Duration_of_route)/60)
    Total_duration_w_delays <- round(sum(rte_length$Expected_duration)/60)
    Delay <- Total_duration_w_delays - Original_Duration
    Percentage_delay <- round(((Total_duration_w_delays/Original_Duration)-1)*100,2)
    
    if (Percentage_delay < 0){
      Percentage_delay <- 0
    }
  
    duration_list <- list(rte_length, Original_Duration, Total_duration_w_delays, Percentage_delay, Delay, distance)
    return(duration_list)
  
  }
  
  getMap <- reactive({
    wf_data <- getWeatherData()
    rain_data <- getRainData()
    temp_data <- getTempData()
    
    shape <- readOGR(dsn = "singapore-towns", layer = "Area")
    shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=WGS84"))
    shapeData@data$PLN_AREA_N <- str_to_title(shapeData@data$PLN_AREA_N)
    
    # Deleting Palau Tekong and replacing it with Palau Ubin renamed as North Eastern Island
    wf_data <- wf_data %>% filter(area != "Pulau Tekong")
    wf_data$area[which(wf_data$area == "Pulau Ubin")] <- "North-Eastern Islands"
    
    # Polygon Mapping Fixed
    shapeData@data$PLN_AREA_N <- str_to_title(shapeData@data$PLN_AREA_N)
    
    rain_s2 <- s2_lnglat(rain_data$long, rain_data$lat)
    weather_s2 <- s2_lnglat(wf_data$long, wf_data$lat)
    wf_data$closest <- s2_closest_feature(weather_s2,rain_s2)
    wf_data$rain <- rain_data[wf_data$closest,"forecast"]
    
    # Combining Sembawang
    sembawang <- shapeData[which(shapeData@data$PLN_AREA_N %in% c("Sembawang", "Simpang")),]
    sembawang <- aggregate(sembawang)
    
    # Combining Changi
    changi <-shapeData[which(shapeData@data$PLN_AREA_N %in% c("Changi", "Changi Bay")),]
    changi <- aggregate(changi)
    
    # Combining all City areas
    city <- shapeData[which(shapeData@data$PLN_AREA_N %in% c("Downtown Core","Marina East","Newton","Orchard","Marina South",
                                                             "Museum","Outram","River Valley","Rochor","Singapore River","Straits View")),]
    city <- aggregate(city)
    
    shapeData_edited <- bind(shapeData, city, sembawang, changi)
    row <- nrow(shapeData_edited)
    shapeData_edited[row-2,] <- shapeData@data[shapeData@data$PLN_AREA_N == "Museum",]
    shapeData_edited@data$PLN_AREA_N[row-2] <- "City"
    shapeData_edited[row-1,] <- shapeData@data[shapeData@data$PLN_AREA_N == "Sembawang",]
    shapeData_edited[row,] <- shapeData@data[shapeData@data$PLN_AREA_N == "Changi",]
    
    fixed <- shapeData_edited[56:58,]
    shapeData_final <- bind(shapeData[which(!(shapeData@data$PLN_AREA_N %in% c("Sembawang", "Simpang","Changi","Changi Bay","Downtown Core",
                                                                               "Marina East","Newton","Orchard","Marina South","Museum","Outram",
                                                                               "River Valley","Rochor","Singapore River","Straits View"))),],fixed)
    rain_int<- merge(shapeData_final,wf_data, by.x="PLN_AREA_N", by.y ="area")
    shapes <- list(shapeData_final, wf_data, rain_int, rain_data)
    return(shapes)
  })
  
  getMapDestination <- reactive({
    if (length(input$destination) > 0) {
      map <- leaflet() %>% addTiles()
      #Geocoding function
      key <- 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew'
      doc <- mp_geocode(addresses = input$destination,
                        key = key,
                        quiet = TRUE)
      pnt <- mp_get_points(doc)
      pnt2 <- pnt %>% mutate(lon = st_coordinates(pnt)[,1], lat = st_coordinates(pnt)[,2]) %>% as.data.frame()
      input_coord <- pnt2 %>% dplyr::select(lon,lat)
      colorFactors <- c("#4568dc", "#78517C", "#F17950")
      
      nearest_3 <- getNearestHDBCarparks(input$destination)
      for (i in 1:3) {
        near_carpark <- nearest_3[i,]
        carpark_coord <- near_carpark %>% dplyr::select(lon, lat)
        pair <- rbind(input_coord, carpark_coord)
        dist_label <- paste0(near_carpark[1, 'dist'], "m")
        avail_final <- getRealTimeCarparkAvailability(Sys.time())
        
        if (near_carpark$name %in% avail_final$carpark_number){
          # Lots availability
          availlots <- avail_final$lots_available[avail_final$carpark_number==near_carpark$name]
          totallots <- avail_final$total_lots[avail_final$carpark_number==near_carpark$name]
          
          popupdetails <- paste0("<b>", near_carpark$name, "</b><br>", availlots, " out of ", 
                                 totallots, " lots available")
        } 
        else{
          popupdetails <- paste0("<b>", near_carpark$name, "</b>")
        }
        
        hdbcarparkicon <- makeAwesomeIcon("car", library = "fa", markerColor = "white")
        
        map <- map %>% addAwesomeMarkers(data=near_carpark, lng=~lon, lat=~lat, icon=~ hdbcarparkicon, popup=popupdetails) %>%
          addPolylines(data=pair, lng=~lon, lat=~lat,
                       label=dist_label,
                       labelOptions =labelOptions(noHide = T, direction = "middle",
                                                  style = list("font-size" = "12px")), color = colorFactors[i], opacity =  1)
      }
      return(map)
    }
    
    else{
      map <- leaflet() %>% setView(lat=1.332, lng=103.82, zoom=11) %>% addTiles()
    }
  })
  
  # ----------------------------------- Output For UI Side
  # ----------- Tab 2: Weather Tab
  
  # Set Base Map
  
  m <- leaflet() %>% setView(lng = 103.83, lat = 1.332, zoom = 12) %>% addTiles()  #show basemap at SG at=1.36, lng=103.82
  
  # Outputs
  
  output$map <- renderLeaflet({
    shapes <- getMap()
    
    #Partition area by colour
    m <- m %>% addPolygons(data=shapes[[1]], weight = 2, stroke = TRUE, smoothFactor = 0.1, fillOpacity = 0.7, fillColor = "aliceblue") %>%
      addMarkers(data = shapes[[2]], ~long, ~lat,label = ~area, layerId = ~area)
    
    pal <- colorFactor(palette = "Blues", domain = shapes[[3]]$rain)
    
    m <- m %>% addPolygons(data = shapes[[3]], weight = 2, stroke = TRUE, smoothFactor = 0.1, fillOpacity = 0.7, fillColor = ~pal(rain)) %>%
      addLegend("bottomright", pal = pal, values = shapes[[3]]$rain,
                title = "Rain Intensity",
                labFormat = labelFormat(suffix = "mm"),
                opacity = 1)
    m
  }
  )
  
  #interactive map to click on markers
  
  m_data <- reactiveValues(select = NULL)
  
  observeEvent(input$map_marker_click,{m_data$select <-input$map_marker_click})
  
  observeEvent(input$location, {m_data$select <- NULL}) #to reset the value of clicked map when using drop-down
  
  
  output$RegionBox <- renderUI({
    select_area <- m_data$select$id
    
    if (is.null(input$location) & is.null(select_area)) {
      infoBox("Selected Region","None - select from map/below",icon=icon("location-crosshairs"),fill=FALSE,width=12)
    } 
    
    else  if (length(select_area > 0)) {
      infoBox("Selected Region",towns[which(towns$area == select_area),2],icon=icon("location-crosshairs"),fill=FALSE,width=12)
      
    }
    
    else  if (length(input$location) > 0) {  
      infoBox("Selected Region",towns[which(towns$area == input$location),2],icon=icon("location-crosshairs"),fill=FALSE,width=12)
      
    }
    
  })
  
  output$LocationBox <- renderUI({
    select_area <- m_data$select$id
    
    if (is.null(input$location) & is.null(select_area)) {
      infoBox("Selected Area","None - select from map/below",icon=icon("city"),fill=FALSE,width=12)
    }
    
    else  if (length(select_area > 0)) {
      wf_data <- getWeatherData()
      wf_data <- wf_data %>% filter(area != "Pulau Tekong")
      wf_data$area[which(wf_data$area == "Pulau Ubin")] <- "North-Eastern Islands"
      infoBox("Selected Area",dplyr::select(filter(wf_data,area==select_area), area),icon=icon("city"),fill=FALSE,width=12)
    }
    
    else  if (length(input$location) > 0) {
      infoBox("Selected Area",input$location,icon=icon("city"),fill=FALSE,width=12)
    }
    
  })
  
  output$WeatherBox <- renderUI({
    select_area <- m_data$select$id
    wf_data <- getWeatherData()
    wf_data <- wf_data %>% filter(area != "Pulau Tekong")
    wf_data$area[which(wf_data$area == "Pulau Ubin")] <- "North-Eastern Islands"
    if (is.null(input$location) & is.null(select_area)) {
      infoBox("2- Hour Weather Forecast Details","None - select from map/below",icon=icon("sun"),fill=FALSE,width=12)
    } 
    
    else if (length(select_area > 0)) {
      infoBox("2- Hour Weather Forecast Details",dplyr::select(filter(wf_data,area==select_area), forecast),icon=icon("sun"),fill=FALSE,width=12)
    }
    
    else if (length(input$location) > 0) {
      infoBox("2- Hour Weather Forecast Details", wf_data %>% filter(area == input$location) %>% dplyr::select(forecast),icon=icon("sun"),fill=FALSE,width=12)
    }
    
  })
  
  output$RainBox <- renderUI({
    select_area <- m_data$select$id
    
    rain_data <- getRainData()
    wf_data <- getWeatherData()
    wf_data <- wf_data %>% filter(area != "Pulau Tekong")
    wf_data$area[which(wf_data$area == "Pulau Ubin")] <- "North-Eastern Islands"
    
    rain_s2 <- s2_lnglat(rain_data$long, rain_data$lat)
    weather_s2 <- s2_lnglat(wf_data$long, wf_data$lat)
    wf_data$closest <- s2_closest_feature(weather_s2,rain_s2)
    
    a <- wf_data[which(wf_data$area == input$location),5]
    
    # Categorizes rainfall intensity into five categories
    # Based on :
    # https://water.usgs.gov/edu/activity-howmuchrain-metric.html#:~:text=Slight%20rain%3A%20Less%20than%200.5,than%208%20mm%20per%20hour.
    Rain_intensity <- function(x) {
      if (x==0) {
        return("No rain")
      }
      if (x > 0 & x <= 0.5) {
        return("Light rain")
      }
      if (x > 0.5 & x <= 4) {
        return("Moderate rain")
      }
      if (x > 4 & x <= 8) {
        return("Heavy rain")
      }
      if (x > 8) {
        return("Very Heavy rain")
      }
    }
    
    if (is.null(input$location) & is.null(select_area)) {
      infoBox("Current Rain Intensity","None - select from map/below",icon=icon("cloud-rain"),fill=FALSE,width=12)
    } 
    
    else if (length(select_area > 0)) {
      b <- wf_data[which(wf_data$area == select_area),5]
      rain_b <- rain_data[b,2]
      infoBox("Current Rain Intensity",paste(Rain_intensity(rain_b),", ",rain_b,"mm"),icon=icon("cloud-rain"),fill=FALSE,width=12)
    }
    
    else if (length(input$location) > 0) {
      rain_a <- rain_data[a,2]
      infoBox("Current Rain Intensity",paste(Rain_intensity(rain_a),", ",rain_a,"mm"),icon=icon("cloud-rain"),fill=FALSE,width=12)
    }
  })
  
  output$TemperatureBox <- renderUI({
    select_area <- m_data$select$id
    
    temp_data <- getTempData()
    wf_data <- getWeatherData()
    wf_data <- wf_data %>% filter(area != "Pulau Tekong")
    wf_data$area[which(wf_data$area == "Pulau Ubin")] <- "North-Eastern Islands"
    
    temp_s2 <- s2_lnglat(temp_data$long, temp_data$lat)
    weather_s2 <- s2_lnglat(wf_data$long, wf_data$lat)
    wf_data$closest <- s2_closest_feature(weather_s2,temp_s2)
    
    c <- wf_data[which(wf_data$area == input$location),5] #which nearest corresponding row to be chosen from
    
    if (is.null(input$location) & is.null(select_area)) {
      infoBox("Current Temperature Details","None - select from map/below",icon=icon("temperature-half"),fill=FALSE,width=12)
    } 
    
    else if (length(select_area > 0)) {
      d <- wf_data[which(wf_data$area == select_area),5] # which nearest corresponding row to be chosen from
      infoBox("Current Temperature Details",paste(temp_data[d,1],"\u00B0C"),icon=icon("temperature-half"),fill=FALSE,width=12)
    }
    
    else if (length(input$location) > 0) {
      infoBox("Current Temperature Details",paste(temp_data[c,1],"\u00B0C"),icon=icon("temperature-half"),fill=FALSE,width=12)
    }
  })
  
  
  # ----------- Tab 3: Route Planning Tab
  text_reactive <- eventReactive(input$submit,{list(input$StartLoc, input$EndLoc)})

  output$map2 <- renderLeaflet({
    loc <- text_reactive()
    tryCatch({ r <- 
      getLocation(loc[[1]], loc[[2]])
    },
    error = function(e) {
      message('No data in chosen Analysis.', e)
      showModal(
        modalDialog(
          div("Input might be over or under specified, please revise your input")
          ,easyClose = TRUE)
      )},
    finally={ # important to include finally = 
      message("\n End of the getMapDestination() function")
    shapes <- getMap()
    })
    rain_int<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    pal <- colorFactor(palette = "Blues", domain = rain_int$rain)
    result_list <- getIntersectingAreas(r, rain_int)
    
    duration_list <- getDuration(result_list)
    
    # Interactive Map
    m <- m %>% addPolygons(data=result_list[[2]], weight = 2, stroke = TRUE, smoothFactor = 0.1, fillOpacity = 0.65, fillColor = ~pal(rain)) %>%
      addCircleMarkers(data = result_list[[5]], color = "green") %>% 
      addCircleMarkers(data = result_list[[6]], color = "red") %>% 
      addPolylines(data = result_list[[3]], opacity = 1, weight = 5, color = "black") %>%
      addLegend("bottomright", pal = pal, values = shapes[[3]]$rain,
                title = "Rain Intensity",
                labFormat = labelFormat(suffix = "mm"),
                opacity = 1)
    m
  })
  
  output$Duration <- renderInfoBox({
    loc <- text_reactive()
    shapes <- getMap()
    r <- getLocation(loc[[1]],loc[[2]])
    rain_int<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    result_list <- getIntersectingAreas(r, rain_int)
    duration_list <- getDuration(result_list)
    
    infoBox("Expected Trip Duration", paste0(duration_list[[2]], " minutes"), icon = icon("clock"))
  })
  
  output$Distance <- renderInfoBox({
    loc <- text_reactive()
    shapes <- getMap()
    r2 <- getLocation(loc[[1]],loc[[2]])
    rain_int2<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    result_list2 <- getIntersectingAreas(r2, rain_int2)
    duration_list2 <- getDuration(result_list2)
    
    infoBox("Distance", paste0(duration_list2[[6]]), icon = icon("road"))
  })
  
  output$DelayDuration <- renderInfoBox({
    loc <- text_reactive()
    shapes <- getMap()
    r3 <- getLocation(loc[[1]],loc[[2]])
    rain_int3<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    result_list3 <- getIntersectingAreas(r3, rain_int3)
    duration_list3 <- getDuration(result_list3)
    
    infoBox("Delay", paste0(duration_list3[[5]], " minutes"), icon = icon("plane-slash"))
  })
  
  output$DelayTripDuration <- renderInfoBox({
    loc <- text_reactive()
    shapes <- getMap()
    r3 <- getLocation(loc[[1]],loc[[2]])
    rain_int3<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    result_list3 <- getIntersectingAreas(r3, rain_int3)
    duration_list3 <- getDuration(result_list3)
    
    infoBox("Delayed Trip Duration", paste0(duration_list3[[3]], " minutes"), icon = icon("triangle-exclamation"))
  })
  
  output$PercentageDelay <- renderInfoBox({
    loc <- text_reactive()
    shapes <- getMap()
    r4 <- getLocation(loc[[1]],loc[[2]])
    rain_int4<- merge(shapes[[1]],shapes[[2]], by.x="PLN_AREA_N", by.y ="area")
    result_list4 <- getIntersectingAreas(r4, rain_int4)
    duration_list4 <- getDuration(result_list4)
    
    infoBox("Delay in Percentage", paste0(duration_list4[[4]], " %"), icon = icon("percent"))
  })
  
  # ----------- Tab 4: CarPark Availability Tab
  
  submit_reactive <- eventReactive(input$submit2,{})
  
  #Map output with 3 carpark markers
  output$map3 <- renderLeaflet({
    submit_reactive()
    
    tryCatch({
      getMapDestination()
    },
    error = function(e) {
      message('No data in chosen Analysis.', e)
      showModal(
        modalDialog(
          div("Input might be over or under specified, please revise your input")
          ,easyClose = TRUE)
      )},
    finally={ # important to include finally = 
      message("\n End of the getMapDestination() function")
    })
  })
  
  output$result_text <- renderUI({
    submit_reactive()
    
    if (length(input$destination) > 0) {
      
      # Generate string to contain distance results
      result_string <- character()
      
      result_string <- paste0(result_string, '<span style="font-size:20px;color:#101010">',
                              '<b> Nearest HDB Carparks: </b> </span> <br>')
      nearest_3 <- getNearestHDBCarparks(input$destination)
      colorFactors <- c("#4568dc", "#78517C", "#F17950")
      
      for (i in 1:3) {
        near_carpark <- nearest_3[i,]
        result_string <- paste0(result_string,'<span style="font-size:20px;color:',colorFactors[i],'">',
                                '<b> Carpark ' , i, " (", near_carpark$dist, 'm)','</b> <br>')
        cp_loc <- which(hdbcarparks$car_park_no == near_carpark$name)
        
        cp_carparkadd <- hdbcarparks[cp_loc, 2]
        result_string <- paste0(result_string, cp_carparkadd, '<br>')
        
        cp_typeofparking <- hdbcarparks[cp_loc, 4]
        cp_freeparking <- hdbcarparks[cp_loc, 6]
        cp_nightparking <- hdbcarparks[cp_loc, 7]
        result_string <- paste0(result_string,'<span style="font-size:15px;">',
                                '<b> Parking System: </b>', cp_typeofparking, '<br>',
                                '<b> Free Parking: </b>', cp_freeparking, '<br>',
                                '<b> Night Parking: </b>', cp_nightparking, "</span>")
        
        # Lots availability
        avail_final <- getRealTimeCarparkAvailability(Sys.time())
        
        if (near_carpark$name %in% avail_final$carpark_number){
          availlots <- avail_final$lots_available[avail_final$carpark_number == near_carpark$name]
          totallots <- avail_final$total_lots[avail_final$carpark_number == near_carpark$name]
          result_string <- paste0(result_string, '<p style="font-size:15px;">',
                                  '<b>Lots Availability: </b>', availlots, " out of ",
                                  totallots, " lots available", "</span></p>")
        }
        
        else {
          result_string <- paste0(result_string, '<br> <br>')
        }
      }
      
      HTML(result_string)
    }
  })
  
})