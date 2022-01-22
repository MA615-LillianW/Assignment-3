#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(RColorBrewer)
library(maps)
library(tigris)
library(leaflet)
options(tigris_use_cache = TRUE)
library(magrittr)
library(rvest)
library(reshape2)
#library(ggiraph)
library(RColorBrewer)
library(geojsonio)
library(shinyWidgets)
library(shinythemes)
#library(systemfonts)
library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(leaflet)
library(htmltools)
library(stringr)
library(dygraphs)
library(xts)
#library(geoR)
require(sp)
library(sp)
library(gstat)
library(sf)
library(georob)
library(rgdal)
library(raster)
library(dplyr)
library(spData)
#library(spDataLarge) #install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
library(tmap)
library(png)
#add data
addRepo("geanders")
data("hurr_tracks")
data("rain")
##########
#data manipulation
buoys_name<-c('42001','42002','42035','42019','42007','42040',
              '42020','42039','42036')
buoys_lat<-c(25.942,26.055,29.232,27.910,30.090,29.207,
             26.968,28.787,28.501)
buoys_long<-c(89.657,93.646,94.413,95.345,88.769,88.237,
              96.693,86.007,84.508)

buoys <- data.frame(Name=buoys_name,Lat = buoys_lat,Long = buoys_long*-1, Speed=1)

#create bouys data
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename="
url2 <- ".txt.gz&dir=data/historical/stdmet/"
urls <- str_c(url1, buoys_name,"h2008", url2, sep = "")


for(i in 1:9){
  suppressMessages(
    buoy_table <- read_table(urls[i], col_names = TRUE))
  #get rid of first row
  buoy_table <- buoy_table[-1,]
  buoy_table$DATE <- paste0(buoy_table$`#YY`,'-',buoy_table$MM,'-',buoy_table$DD)
  buoy_table$DATE <- as.Date(buoy_table$DATE)
  
  buoy_sub <- buoy_table %>% filter(DATE >= '2008-09-09' & DATE <= '2008-09-18')
  buoy_sub$WSPD <- as.numeric(buoy_sub$WSPD)
  
  buoy_sub$TIME <- paste0(buoy_sub$DATE,' ',buoy_sub$hh,":",buoy_sub$mm,':00' )
  buoy_sub$TIME <- as.POSIXct(buoy_sub$TIME, tz = 'UTC')
  tmp <- data.frame(TIME=buoy_sub$TIME, WSPD=buoy_sub$WSPD)
  if(i==1)
    buoy_data<-data.frame(TIME=buoy_sub$TIME,WSPD=buoy_sub$WSPD)
  else{
    tmp <-data.frame(TIME=buoy_sub$TIME,WSPD=buoy_sub$WSPD)
    buoy_data<- left_join(buoy_data,tmp,by="TIME")
  }
}

colnames(buoy_data)<- c('TIME', buoys_name)

#filling missing values using mean
for(i in 2:10)
  for(j in 1:nrow(buoy_data)){
    if(is.na(buoy_data[j,i])){
      jtmp=j+1
      while(is.na(buoy_data[jtmp,i])) jtmp<-jtmp+1
      buoy_data[j,i]<-mean(buoy_data[j-1,i],buoy_data[jtmp,i])
      
    }
  }

#pivot longer
buoy_data2 <- buoy_data %>% pivot_longer(2:10, names_to = 'buoys', values_to = 'wind')

buoy_geo <- merge(buoy_data2, buoys, by.x = 'buoys', by.y = 'Name')

#buoy datatable
buoy_geo1 <- buoy_geo[,-6]
buoy_geo1$TIME <- as.character(buoy_geo1$TIME)

#rainfall datatable
data('rain') 
data('county_centers')
rain_dat <- rain %>% filter(storm_id == 'Ike-2008')
rain <- merge(rain_dat, county_centers, by = 'fips', all.x = TRUE)

comb_rain <- rain
county <- st_read("UScounties/UScounties.shp",quiet = TRUE)
county$fips <- county$FIPS


#check NAs
#sum(is.na(buoy_data))

#convert buoy_geo to geo data
buoy_shp <- buoys %>% dplyr::select(Name, Long, Lat)
coordinates(buoy_shp) = c('Long', 'Lat')
ll2 = '+proj=longlat +datum=NAD83'
proj4string(buoy_shp) = CRS(ll2)

##############




tranbouy<-t(buoy_data)
colnames(tranbouy)<-tranbouy[1,]
tranbouy<-tranbouy[-1,]
tranbouy<-cbind(Name=row.names(tranbouy),tranbouy)
tranbouy<-data.frame(tranbouy)
buoytime<-left_join(buoys,tranbouy,by='Name')
for(i in 4:243){
  buoytime[,i]<-as.numeric(buoytime[,i])}



###############

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Hurricane Exposure</a>'), id="nav",
             windowTitle = "Hurricane Ike 2008",
             
             tabPanel("Hurricane Exposure",
                      sidebarLayout(   
                      sidebarPanel(
                        #first conditional panel for rainfall
                        conditionalPanel(condition = "input.tabselected == 1",
                                         #choose slider range 
                                         sliderInput("lag", "Days included:", 
                                                     min= -5, max= 3, value= c(-5,3) ),
                                         #numeric input of rainfall, value = initial value
                                         numericInput("rain_limit", label = h3("Rain limit"), min = 0, value = 0),
                                         numericInput("dist_limit", label = h3("Distance limit"), min = 0, value = 0)), 
                        
                        conditionalPanel(condition = "input.tabselected == 2",
                                         #numeric input of wind, value = initial value
                                         numericInput("wind_limit", label = h3("Wind limit"), min = 0, value = 0)), 
                        
                        conditionalPanel(condition = "input.tabselected == 3",
                                         #pikcer input of event, value = initial value
                                         pickerInput("event_select", "Event:",   
                                                     choices = c("flood", "tornado", "wind","tropical_storm"), 
                                                     selected = c("flood"),
                                                     multiple = FALSE))      
                        ),
                        #show different types of plot
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Rainfall", value = 1, plotlyOutput("rainfall_plot")),
                            tabPanel("Wind", value = 2, plotlyOutput("wind_plot")),
                            tabPanel("Event", value = 3, plotlyOutput("event_plot")), 
                            id = "tabselected"
                          )
                        )
                        
             )),
             
             tabPanel("Buoys mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("buoy_plot", width="100%", height="100%"),
                
                          
                          #absolute location of the panel on the map
                          absolutePanel(id = "controls", class = "panel panel-default",
                                           bottom = 10, right = 10, width = 500, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                          
                          h5('Hurricane wind speed observed by buoys', align = "center"),
                          #h4(textOutput("result"), align = "center"),
                          #plot the dynamic time series plot
                          selectizeInput(inputId = "selected_buoys",
                                         label = "selected_ids",
                                         choices = buoys$name, #shape file
                                         selected = '42001',
                                         multiple = TRUE),
                          #select input on maps
                         
                          dygraphOutput("wind_timeplot", height="300px", width="100%"),
                          sliderInput("buoy_time", "Time", min(buoy_data$TIME), max(buoy_data$TIME), width = "100%",
                                      value = min(buoy_data$TIME)),
                          span(("Circles show the wind speed measured by the buoys"),align = "left", style = "font-size:80%"))
                          #plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                          #  span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%")
                          #,#tags$br(),
                          #span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                          
                          #sliderTextInput("sars_plot_date",
                          #                label = h5("Select mapping date"),
                          #                choices = format(unique(sars_cases$date), "%d %b %y"),
                          #                selected = format(sars_max_date, "%d %b %y"),
                          #                grid = FALSE,
                          #                animate=animationOptions(interval = 3000, loop = FALSE))
                          )
                      
                      
                      ),
             
             tabPanel("Kriging and Prediction",
                      sidebarLayout(   
                         sidebarPanel(
                            #first conditional panel for rainfall
                            conditionalPanel(
                               
                               span(tags$i(h6("The rainfall in the area where the hurricane was advancing affects each other.")), style="color:#045a8d"),
                               span(tags$i(h6("Using the log(max precipitation rainfall)+1 to do kriging.")), style="color:#045a8d"),
                               span(tags$i(h6('The slope represents the correlation between two points. The greater the slope, the stronger the correlation.')), style="color:#045a8d"),
                               
                               condition = "input.varioselected == 1",
                               numericInput("VarioDays", label = h3("Days"), min = -5,max = 3, value = 0),
                               checkboxInput('Varioline', 'Show line'),
                               "The variogram characterizes the spatial continuity or roughness of a data set. (Seleting the Gaussian Model)"
                               #choose slider range 
                               #sliderInput("lag", "Days included:", 
                               #           min= -5, max= 3, value= c(-5,3) ),
                               #numeric input of rainfall, value = initial value
                               #numericInput("rain_limit", label = h3("Rain limit"), min = 0, value = 0),
                               #numericInput("dist_limit", label = h3("Distance limit"), min = 0, value = 0)), 
                            ),
                            conditionalPanel(
                               span(tags$i(h6('Areas affected by the hurricane have significantly increased rainfall.')), style="color:#045a8d"),
                               
                               span(tags$i(h6('Transfer the "max precipitation rainfall" to "max precipitation rainfall/10" to reduce the scale')), style="color:#045a8d"),
                               
                               condition = "input.varioselected == 2",
                               numericInput("ObserDays", label = h3("Days"), min = -5,max = 3, value = 0),
                               
                               #numeric input of wind, value = initial value
                               #numericInput("wind_limit", label = h3("Wind limit"), min = 0, value = 0)), 
                            ),
                            conditionalPanel(
                               span(tags$i(h6('According to spatial continuity, find the relationship between the distance from a point in space to the hurricane’s landfall point and the maximum rainfall, and predict the rainfall map.')), style="color:#045a8d"),
                               condition = "input.varioselected == 3",
                               
                               #pikcer input of event, value = initial value
                               pickerInput("Pred_select", "Prediction:",   
                                           choices = c("Prediction", "Lower bound 95% Prediction interval", "upper bound 95% Prediction interval"), 
                                           selected = c("Prediction"),multiple = FALSE),
                               "In statistical inference, specifically predictive inference, a prediction interval is an estimate of an interval in which a future observation will fall, with a certain probability, given what has already been observed."
                            )
                         ),
                         #show different types of plot
                         mainPanel(
                            tabsetPanel(
                               tabPanel("Variogram", value = 1, plotOutput("Variogram_plot")),
                               tabPanel("Observation", value = 2, tmapOutput("Observation_plot")),
                               tabPanel("Prediction", value = 3, imageOutput("Prediction_plot")), 
                               id = "varioselected"
                            )
                         )
                         
                      )
             ),
             
             tabPanel("Data",
                      conditionalPanel(
                         'input.dataset === "buoy_geo1"',
                         helpText("Click the column header to sort a column.")
                         #checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                         #names(diamonds), selected = names(diamonds))
                      ),
                      conditionalPanel(
                         'input.dataset === "rain"',
                         helpText("Click the column header to sort a column.")
                      ),
                      mainPanel(
                         tabsetPanel(
                            id = 'dataset',
                            tabPanel("Buoy Data", DT::dataTableOutput("mytable1")),
                            tabPanel("Rainfall Data", DT::dataTableOutput("mytable2"))
                            
                         )
                      ),
                      # downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      #"Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                      # "Johns Hopkins Center for Systems Science and Engineering.")
                   ),
             
             tabPanel("About this site",
                      tags$div(
                         tags$h4("Background"), 
                         "Hurricane Ike produced a damaging, destructive and deadly storm surge across the upper Texas and southwest Louisiana coasts. 
                         And it will likely end up being the third costliest natural disaster in the United States behind Hurricane Katrina and Hurricane Andrew.",
                         tags$br(),tags$br(),
                         "Over the warm waters of the Gulf of Mexico, Ike grew in size and intensified to a category two hurricane with maximum winds of 100 mph by that evening. 
                         Ike continued to move northwest toward the Texas coast as the hurricane crossed the central and northwest Gulf of Mexico. Although Ike's intensity remained in the category 2 range, the cyclone continued to grow and became a very large hurricane. 
                         The diameter of tropical storm force winds covering a total of 425 miles from the northwest to southeast as Ike approached the upper Texas coast on Friday, September 12th. 
                         Ike made landfall at 2:10 am CDT Saturday, September 13th near Galveston, Texas. Ike was a category 2 hurricane at landfall with maximum sustained winds of 110 mph.",
                         tags$br(),tags$br(),tags$h4("Our work"),
                         "We selected the data 'Ike 2008' in hurricaneexposure package, generating a series of plots on rainfall, wind and events in 7 days, which intuitively reflect the hurricane's effection around the landfall and surronding areas.
                         In order to precisely observe the path of the hurricane, we plotted a time series chart based on the wind speed changes recorded by the buoys around the landfall.
                         Besides, we explored the spatial continuity between points formed after the hurricane made landfall, and then fitted a spatial model of the distance from each point on the space to the hurricane landfall and the maximum rainfall before and after the hurricane's arrival. 
                         Moreover, we predicted the map of rainfall at each point on the space before and after the hurricane's arrival within the prediction interval.",
                         tags$br(),tags$br(),tags$h4("Code"),
                         "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/MA615-Homework/Assignment-3","Github."),
                         tags$br(),tags$br(),tags$h4("Sources"),
                         tags$b("National Data Buoy Center: "), tags$a(href="https://www.ndbc.noaa.gov/obs.shtml","National Data Buoy System"),tags$br(),
                         tags$b("Explore and Map County-Level Hurricane Exposure in the United States : "), tags$a(href="https://cran.r-project.org/web/packages/hurricaneexposure/index.html","Hurricane Exposure Package Website"),tags$br(),
                         tags$br(),tags$br(),tags$h4("Authors"),
                         "Zhihui Zhang, Shicong Wang, Keliang Xu, Tao He",tags$br(),
         
                         #tags$br(),tags$br(),tags$h4("Contact"),
                         #"edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                        # tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                      )
             ),
                     
                      
             
  )          
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
 library(dygraphs)
 output$rainfall_plot <- renderPlotly({
   if(input$rain_limit + input$dist_limit > 0){
     map_rain_exposure(storm = "Ike-2008", days_included = input$lag, rain_limit = input$rain_limit, dist_limit = input$dist_limit)+ 
       ggtitle("Ike-2008") + 
       theme(plot.title = element_text(hjust = 0.5))
   }else{
   map_counties(storm = "Ike-2008", metric = "rainfall", days_included = input$lag) +
     ggtitle("Ike-2008") +
     theme(plot.title = element_text(hjust = 0.5))}
 })
 output$wind_plot <- renderPlotly({
   if(input$wind_limit > 0){
     map_wind_exposure(storm = "Ike-2008", wind_limit = input$wind_limit)
   }else{
   map_counties(storm = "Ike-2008", metric = "wind") +
     ggtitle("Ike-2008") +
     theme(plot.title = element_text(hjust = 0.5))}
 })
 output$event_plot <-  renderPlotly({
   map_event_exposure(storm = "Ike-2008", event_type = input$event_select)
 })
 
 output$buoy_plot <-  renderLeaflet({
   tmptime<-as.numeric(difftime(input$buoy_time, min(buoy_data$TIME), units = "hours"))/
     as.numeric(buoy_data$TIME[2]-buoy_data$TIME[1])+1
   buoys$Speed<-buoytime[,tmptime+3]
   
   pal <- colorNumeric(c("#eeccdd", "#ca6095"), domain = buoys$Speed)
   
   leaflet(buoys) %>% addTiles() %>%
     setView(lat = 29, lng = -87, zoom = 5.45) %>%
     addMarkers(-94.7, 29.3, popup = "Ike-2008") %>% 
     addCircles(~Long, ~Lat, radius= buoys$Speed*5000, 
                popup = str_c(buoys$Name,':',buoytime[,tmptime+3],'m/s'),
                color=~pal(Speed),fillColor=~pal(Speed), fillOpacity = 0.7) %>%
     addCircleMarkers(~Long, ~Lat, layerId = buoys$Name, radius= 10 ,color=~pal(Speed),fillColor=~pal(Speed), fillOpacity = 0.7 )  %>% 
     leaflet::addLegend(position = "bottomleft", pal = pal, values = ~Speed,title="Speed m/s")
   
 })


 output$Variogram_plot<-renderPlot({
    comb_rain_lag  <- comb_rain %>% filter(lag == input$VarioDays)
    coordinates(comb_rain_lag) = c('longitude', 'latitude')
    ll2 = '+proj=longlat +datum=NAD83'
    proj4string(comb_rain_lag) = CRS(ll2)

    comb_rain_lag$precip_max = comb_rain_lag$precip_max /10
    
    v = variogram(precip_max ~ 1, data=comb_rain_lag)
    #plot(v)
    if(input$Varioline==1){
       vmi = vgm(model='Gau', psill=6, range=1000*4,nugget=1)
       v.fit = fit.variogram(v, vmi)
       plot(v, v.fit)
    }
    else {
       plot(v)
    }
    
 })
 
 output$Observation_plot<-renderTmap({
    
    comb_rain2 <- comb_rain %>% filter(lag == input$ObserDays)
    comb_rain2$precip_max = comb_rain2$precip_max /10
    Rain <- merge(county, comb_rain2, by = "fips")
    Rain1 <- Rain[,-6]
    Rain2 <- sf::st_make_valid(Rain1)
    tm_shape(Rain2) +
       tm_polygons(col ="precip_max", palette = "Blues",style = "fixed", breaks = c(0, 1.4, 2.8 , 4.2, 5.6 , 7.0),
                   legend.hist = TRUE) +
       tm_layout(legend.outside = TRUE) +
       tmap_options(check.and.fix = TRUE)
    
    
 })

 
 output$Prediction_plot <- renderImage({
    
    if(input$Pred_select=="Prediction"){
       return(
          list(src = file.path("pred.png"), height = 420, width = 600, contentType = "Images/jpeg/png"))
    }
    else{
       if(input$Pred_select=="Lower bound 95% Prediction interval"){
          return(
             list(src = file.path("lower bound 95% PI.png"), height = 420, width = 600,contentType = "Images/jpeg"))
       }
       else{
          return(
             list(src =file.path("upper bound 95% PI.png"), height = 420, width = 600,contentType = "Images/jpeg"))
       }
    }
    
 },deleteFile=FALSE)
 
 

 


 observeEvent(input$buoy_plot_marker_click, {
   click <- input$buoy_plot_marker_click
   ##print(click$id)
   #every time I click, add it to selectInput
   #if I click the buoys twices
   if(click %in% input$selected_buoys && length(input$selected_buoys) != 1){
     #input$selected_buoys <- input$selected_buoys[input$selected_buoys != click]
     print(setdiff(input$selected_buoys, click))
     updateSelectInput(session, "selected_buoys", "Click on Station", 
                       choices = buoys$Name, 
                       selected = setdiff(input$selected_buoys, click))
   }else{
     if(click %in% input$selected_buoys){
       updateSelectInput(session, "selected_buoys", "Click on Station", 
                         choices = buoys$Name, 
                         selected = c(input$selected_buoys))
     }else{
   buoy <- input$buoy_plot_marker_click$id 
   #print(c(input$selected_buoys, buoy))
   updateSelectInput(session, "selected_buoys", "Click on Station", 
                     choices = buoys$Name, 
                     selected = c(input$selected_buoys, buoy))}}
 })
 
 buoy_sub <- reactive({
   buoy_geo[buoy_geo$buoys %in% input$selected_buoys,]
   #print(buoy_geo[buoy_geo$buoys %in% input$selected_buoys,])
 })
 
 
 output$wind_timeplot <- renderDygraph({
   if(length(input$selected_buoys) == 1){
     base <- xts(x = buoy_sub()$wind, order.by = buoy_sub()$TIME)
     base %>%
       dygraph() %>%
       dySeries(label = input$selected_buoys) %>%
       dyOptions(stackedGraph = TRUE) %>%
       dyRangeSelector(height = 20)
   }else{
     buoy_1 <- buoy_sub() %>% filter(buoys == input$selected_buoys[1])
     base <- xts(x = buoy_1$wind, order.by = buoy_1$TIME)
     for(i in input$selected_buoys[-1]){
       buoy_tmp <- buoy_sub() %>% filter(buoys == i)
       base <- cbind(base,xts(x = buoy_tmp$wind, order.by = buoy_tmp$TIME))
     }
     colnames(base) <- input$selected_buoys
     base %>%
       dygraph() %>%
       dyOptions(stackedGraph = TRUE) %>%
       dyRangeSelector(height = 15)
   }
   
   
   
 })
 
 
 
 output$result <- renderText({
   paste("You chose",  unique(buoy_sub()$buoys))
 })
 
 # choose columns to display
 output$mytable1 <- DT::renderDataTable({
    DT::datatable(buoy_geo1,options = list(orderClasses = TRUE))
 })
 
 
 # customize the length drop-down menu; display 5 rows per page by default
 output$mytable2 <- DT::renderDataTable({
    DT::datatable(rain, options = list(orderClasses = TRUE))
    
    
 })
 

  
}

# Run the application 
shinyApp(ui = ui, server = server)


