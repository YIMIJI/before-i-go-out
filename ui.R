library(shiny)
library(shinyjs)
library(shinyTime)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)


# Dear Prof/TA, for this version of the app to work properly aesthetically,  all packages needs to be updated to the latest version.
# Some key packages to check would be leaflet (version 2.1.1) 

towns <- read.csv("Singapore-town.csv") # based on weather forecast api(area) & Wikipedia (region)
names(towns) <- c("area","region")
hdbcarparks <- read.csv("hdb_carparks_final.csv", stringsAsFactors = FALSE)

shinyUI(navbarPage(title = "BIGO Live", 
                 tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
     tabPanel("Home",
              includeHTML("home.html"),
              tags$script(src = "plugins/scripts.js"),
              tags$head(
                tags$link(rel = "stylesheet",
                          type = "text/css",
                          href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css")
              )
     ),

                 
    # Tab 2 Weather Details
    tabPanel("Weather Details",
      dashboardPage(
        dashboardHeader(title = "Rainfall Data"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
          fluidRow(
            column(width = 4,
                   box(id = 'infos',
                       title = "Information (Rainfall Data)", status = "info", solidHeader = TRUE, width = NULL,
                       fluidRow(
                         column(width = 12,
                                p("This tab gives a brief overview of what the weather is like in Singapore at the current time. 
                                The sidebar shows the region, area, forecasted weather, live rain intensity and live temperature
                                of the selected location. This feature does not support inputs that are in the future."))
                           ),
                         fluidRow(uiOutput("RegionBox")),
                         fluidRow(uiOutput("LocationBox")),
                         fluidRow(uiOutput("WeatherBox")),
                         fluidRow(uiOutput("RainBox")),
                         fluidRow(uiOutput("TemperatureBox")),
                         
                         fluidRow(
                           column(width = 12,
                                  selectizeInput(inputId = "location",
                                                 label = h4("Select Location"),
                                                 choices = towns[,1],
                                                 multiple = TRUE,
                                                 options=list(placeholder='e.g. Bishan', maxItems=1)))),
                         
                         fluidRow(
                            column(width = 6,
                                   timeInput(inputId = "time", "Time:", value = Sys.time())),
                            column(width = 6,
                                   dateInput(inputId = "date", "Date:", value = Sys.Date())))
                           
                           )),
                  
                column(width = 8,
                       box(tags$style(type = "text/css", ".box-body {height:80vh}"),
                           title = "Current Rainfall Data",leafletOutput(outputId = "map", height = "100%"), width = NULL ))
              )
        )
      )
      ),
    
      # Tab 3 Route Planning
      tabPanel("Route Planning",
        dashboardPage(
          dashboardHeader(title = "Singapore Route"),
          dashboardSidebar(disable = TRUE),
          dashboardBody(
            fluidRow(
                column(width = 4,
                       box(id = 'infos',
                           title = "Information (Route Planning)", status = "info" , solidHeader = TRUE,width = NULL,
                           fluidRow(
                              column(width = 12,
                                      p("This tab allows the user to input their start and end destinations, and it helps estimate the delay the user
                                      may experience due to the current weather situation. This allows the user to better plan their schedules according
                                        to any potential delays they may experience from bad weather conditions.")
                                  )),
                           fluidRow(
                             column(width = 6,
                                    textInput(inputId="StartLoc", label="Start Location", value = "", width = NULL, placeholder = "NUS")),
                             
                             column(width = 6,
                                    textInput(inputId="EndLoc", label="End Location", value = "", width = NULL, placeholder = "Changi Airport")),
                             
                             column(width = 6,
                                    actionButton("submit", label = "Submit")),
                             
                             column(width = 12, style= 'padding-top:15px',
                                    uiOutput("Duration")),
                             
                             column(width = 12,
                                    uiOutput("Distance")),
                             
                             column(width = 12,
                                    uiOutput("DelayDuration")),
                             
                             column(width = 12,
                                    uiOutput("DelayTripDuration")),
                             
                             column(width = 12,
                                    uiOutput("PercentageDelay"))
                             )
                           )
                       ),
                column(width = 8,
                       box(tags$style(type = "text/css", ".box-body {height:80vh}"),
                           title = "Singapore Map Route Planning",leafletOutput(outputId = "map2", height = "100%"), width = NULL ))
                )
                   )
               )
      ),
      
      # Tab 4 Carpark Availability
      tabPanel("CarPark Availability",
        dashboardPage(
          dashboardHeader(title = "Nearest Carpark Availability"),
          dashboardSidebar(disable = TRUE),
          dashboardBody(
            fluidRow(
              column(width = 4,
                     box(id = 'infos',
                         title = "Information (Nearest Carpark)", status = "info" , solidHeader = TRUE,width = NULL,
                         
                         fluidRow(
                           column(width = 12,
                                  p("This tab displays the 3 carparks nearest to the destination. Information about these carparks such as lots
                                    availability and distance are displayed, to allow for the user to plan where to park before starting their journey.")
                                  )),
                         fluidRow(
                           column(width = 12,textInput(inputId="destination", label="End Location", value = "", width = NULL, placeholder = "Singapore University of Technology and Design")),
                           
                           column(width = 12,
                                  actionButton("submit2", label = "Submit"))),
                         
                         fluidRow(
                           column(width = 12,
                                  uiOutput("result_text"), style= 'padding-top:15px'))
                          )
                          
                   ),
              column(width = 8,
                     box(tags$style(type = "text/css", ".box-body {height:80vh}"),
                         title = "Carpark Availability Map",leafletOutput(outputId = "map3", height = "100%"), width = NULL ))
                 )
               )
             )),
      # Tab 5 - About Us
      tabPanel("About Us",
               includeHTML("about.html"),
               shinyjs::useShinyjs(),
               tags$head(
                 tags$link(rel = "stylesheet",
                           type = "text/css",
                           href = "plugins/carousel.css"),
                 tags$script(src = "plugins/holder.js")
               ),
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               )
    )
  )
)