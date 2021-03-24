#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#install.packages("viridis") # Install
#install.packages("readxl") 
library("viridis") 
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)

library(shiny)

library(readxl)
destfile <- "NYC_Airbnb2019.xlsx"

nycmap <- read_excel(destfile)
nycmap <- with(nycmap, nycmap[!(nzchar(name) | is.na(name)), ])
head(nycmap)
neighbourhood_group <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")

room <- c("Entire home/apt", "Private room", "Shared room")

groupColors <- colorFactor(c("#E03A3C", "#009DDC","#62BB47"),
                           domain = c("Entire home/apt", "Private room","Shared room"))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   
                
                leafletOutput(outputId = "map", width = "100%", height = "100%"),
                
            
   
   ##### Listings ##########               
   tabPanel("Listings, neighbourhood_groupughs and Price Changes",    
            fluidRow(
              
              
              column(9,
                     h3(""),
                     plotlyOutput(outputId = "graph1", width=1000, height =350),
                     br(),
                     plotlyOutput(outputId = "tab_price", width=1000, height =350)
              )
              
            )
            
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mapdf <- reactive({
  nycmap %>%
    filter(neighbourhood_group %in% input$select_neighbourhood_group 
             #room_type %in% input$select_room 
             #price >= input$slider_price[1] &
             #price <= input$slider_price[2] 
           ) 
})

# create the map
output$map <- renderLeaflet({
  leaflet(nycmap) %>% 
    setView(lng = -73.95, lat = 40.7	, zoom = 9)  %>% #setting the view over ~ center of North America
    addTiles() %>% 
    addCircles(data = nycmap, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~price/25, popup = ~as.character(price), label = ~as.character(paste0("Type: ", sep = " ", room_type)),  fillOpacity = 0.5)
})

# observe an event
observe({ #require a trigger to call the observe function
  proxy <- leafletProxy("map",data = mapdf()) %>% #don't forget ()
    clearMarkerClusters() %>% 
    clearMarkers() %>%
    # circle
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~groupColors(room_type),
                     group = "CIRCLE",
                     popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                    'Room Type:', room_type,'<br/>',
                                    'Price:', price,'<br/>',
                                   # 'Rating Score:', review_scores_rating, '<br/>',
                                    'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
    # cluster
    addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                     group = "CLUSTER",
                     popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                    'Room Type: ', room_type, '<br/>',
                                    'Price:', price,'<br/>',
                                    #'Rating Score:', review_scores_rating, '<br/>',
                                    'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
    # circle/ cluster panel
    addLayersControl(
      baseGroups = c("CIRCLE","CLUSTER"),
      options = layersControlOptions(collapsed = FALSE)
    ) 
})

## reactivate count dataframe for map graph1 



## reactivate price dataframe for map graph2
pricedf <- reactive({
  mapdf() %>% 
    group_by(., room_type) %>% 
    summarise(., avg_price = round(mean(price),2))
})




##### Listings, neighbourhood_groupughs and Price Changes #######################
## reactivate dataframe for listings grapgh




}
# Run the application 
shinyApp(ui = ui, server = server)

