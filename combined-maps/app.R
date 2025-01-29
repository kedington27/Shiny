library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(here)

data <- st_read(here("geocodeddata", "combined_nightlife_geo_sf.geojson"))

ui = fluidPage(
  titlePanel("1970-1986 NYC Queer Spaces Map"),
  sidebarLayout(
    sidebarPanel(
      p("This Leaflet map displays New York City's nightlife establishments with dynamic geocoding and filtering."),
      selectInput("filter_type", "Filter by Type:", 
                  choices = c("All", "bar","resturant","bar/resturant", "bar/restaurant", "club", "cruising", 
                              "baths","bath", "night club", "private club", "disco", "bookstore", "sex shop", 
                              "erotica", "cinema", "juice bar", "coffee shop", "theatre", "gym", "after hours place",
                              "bar/disco", "cabaret", "resturant/cabaret", "bar/club", "cruising at movies",
                              "activist dances", "massage", "leather mail order"), 
                  selected = "All"),
      selectInput("filter_borough", "Filter by Borough",
                  choices = c("All", "Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"),
                  selected = "All"),
      sliderInput("filter_years", "Filter by Years of Operation:", 
                  min = 1950, max = 2010, value = c(1950, 2010), step = 1, sep = "")
    ),
    
    mainPanel(
      leafletOutput("map")  # Output for the Leaflet map
    )
  )
)

server <- function(input, output) {
  # Reactive filtered data
  filtereddata <- reactive({
    
    # Filter by type if not "All"
    if (input$filter_type != "All") {
      data <- data |> filter(type == input$filter_type)
    }
    
    # Filter by borough if not "All"
    if (input$filter_borough != "All") {
      data <- data |> filter(Borough == input$filter_borough)
    }
    data
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    
    leaflet(data = filtereddata()) %>%
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 11) %>%
      addCircleMarkers(
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        popup = ~paste("<strong>", name, "</strong><br>",
                       "Type: ", type, "<br>",
                       "Opened: ", opened, "<br>",
                       "Closed: ", ifelse(is.na(closed), "Still Open", closed)),
        radius = 5,
        color = "#FF69B4",
        fillOpacity = 0.9
      )
  })
}

shinyApp(ui, server)
