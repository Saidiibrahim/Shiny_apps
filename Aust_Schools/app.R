#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(dplyr)
library(readxl)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Australian Schools"),

    # Sidebar with a slider input for number of bins 
    navbarPage( "Location of Schools in Australia", id = "main",
                tabPanel("Map", leafletOutput("asmap", height = 1000)),
                tabPanel("Data Explorer", DT::dataTableOutput("table"))
        )

    
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    #import and clean data
    
    # Read in data
    schools <- read_xlsx('schools.xlsx')
    
    # To dataframe
    schools <- data.frame(schools)
    colnames(schools) <- c('ACARA ID', 'School Name', 'Suburb', 'State', 'Postcode', 'Type', 'Sector', 
                           'Status', 'Parent School ID', 'AGE ID', 'Latitude', 'Longitude') # Renaming columns
    
    # Remove first row duplicate
    schools <- schools[-1,]
    
    
    # Convert lat and lon variables to numeric
    schools <- transform(schools, Latitude = as.numeric(Latitude), 
                         Longitude = as.numeric(Longitude))
    # create colour palette
    pal <- colorFactor(palette = c("grey", "blue", "black", "cyan"),
                       levels = c('Primary', 'Combined', 'Special', 'Secondary'))
    
    #create the leaflet map
    output$asmap <- renderLeaflet({
        leaflet(schools) %>% 
            addTiles( group = "OSM") %>% 
            addResetMapButton() %>% 
            addProviderTiles(provider = "CartoDB", group = "Carto") %>% 
            addProviderTiles(provider = "Esri", group = "Esri") %>% 
            addCircleMarkers(radius = 2, color = ~pal(Type), clusterOptions = markerClusterOptions(),
                             popup = ~paste0("<b>", School.Name, "</b>", "<br/>",Type )) %>% 
            addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), position = "topleft") %>% 
            addLegend(position = "bottomright", pal = pal,
                      values = c("Primary", "Combined", "Special", "Secondary")) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
