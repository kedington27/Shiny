library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(here)
library(rsconnect)
library(bslib)

data <- st_read(here("new-geocode", "combined_nightlife_geo_sf.geojson"))

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/kedington27",
  target = "_blank"
)
link_website <- tags$a(
  shiny::icon("r-project"), "Mapping-Spatial-Networks-of-Queer-Pleasure-Within-New-York-City",
  href = "https://github.com/kedington27/Shiny",
  target = "_blank"
)

ui <- page_navbar(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  title = "1970-1986 NYC Queer Spaces Map",
  
  # Map page
  nav_panel(
    title = "Map",
    fluidRow(
      column(3, offset = 1,
             selectInput("filter_type", "Filter by Type:", 
                         choices = c("All", "bar", "restaurant", "bar/club", "bar/restaurant", "club", "cruising", 
                                     "baths", "disco", "bookstore", "erotica", "cinema", "theatre", 
                                     "gym", "bar/disco", "cabaret", "bar/cabaret", "massage", "backroom", "bar/backroom", "bar/club/backroom", 
                                     "bar/restaurant/cabaret","disco/backroom", "disco/cabaret", "erotica/cruising", "juice bar/disco",
                                     "restaurant/café", "hotel", "sex-shop", "juice-bar", "coffee-shop", "activist-dances",
                                     "baths/gym/fantasy-rooms", "book/sex-shop", "cruising-at-movies", "gay-department-store", "gift-shop",
                                     "porn-theatre/cruising", "private-club/restaurant", "leather-mail-order", "private-disco/cabaret"), 
                         selected = "All")
      ),
      column(3,
             selectInput("filter_borough", "Filter by Borough:",
                         choices = c("All", "Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"),
                         selected = "All")
      ),
      column(5,
             sliderInput("filter_years", "Filter by Years of Operation:", 
                         min = 1840, max = 2025, value = c(1840, 2025), step = 5, sep = "")
      )
    ),
    fluidRow(
      column(12, leafletOutput("map", height = 800)) # Output for the Leaflet map
    )
  ),
  # About the Map page
  nav_panel(
    title = "About the Map",
    h2("About the Map/Instructions"),
    p("Below is a map of the five boroughs of New York City's queer bars, clubs, 
      bathhouses, bookstores, cinemas, restaurants, sex stores, backrooms, and cruising 
      locations open between 1970-1986. Some of the establishments were open before 1970 and many of 
      them closed after 1986; however, those are the bounding years of my study. I have still included the
      dates on the slider that fall outside of my temporal interest so that it is a more honest
      representation of the data. Many of the dates are approximate because of the fleeting nature of
      queer establishments. The majority of my dates were found either through the NY Times, other online 
      journals and forums, Bob Damon's Address Book, Ciao! Magazine, Michael's Thing, and other gay travel guides."),
    p("To use the map, You can filter by three different categories: type, borough, and year. To do so you can play 
      with the filter tabs and the sliding scale. To use the sliding scale I recommend starting with the right side toggle
      because the way it is coded means that there will be more drastic and accurate change over time moving backwards.
      You can zoom in and out on the map and its various points through scrolling on it. Lastly, you can click on the individual 
      points on the map to see the name of the location, the type of location, years of operation, and whether or not the dates of 
      operation are approximate or not. All the locations that say 'yes' to the approximate label are locations that I could not find 
      more concrete dates for in my sources. All dates that say closed 2025 are still currently open as of 2025.")
  ),
  
  
  # Links menu in the navbar
  nav_menu(
    title = "Back to Home Site",
    align = "right",
    nav_item(link_website)
  ),
  
  nav_menu(
    title = "GitHub",
    align = "right",
    nav_item(link_shiny)
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
      data <- data |> filter(borough == input$filter_borough)
    }
  
    
    # Filter by years of operation
    data <- data |> 
      filter(opened >= input$filter_years[1] | closed >= input$filter_years[1], 
             opened <= input$filter_years[2])
    
    # Add row so map won't glitch if there is no data
    if (nrow(data) == 0) {
      data <- data %>% 
        add_row(geometry = st_sfc(st_point(c(1, 1)), crs = 4326))
    }
    
    # Return filtered data
    data

  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    
    leaflet(data = filtereddata()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-74.00, 40.71, zoom = 11) %>%
      addCircleMarkers(
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        popup = ~paste("<strong>", name, "</strong><br>",
                       "Type: ", type, "<br>",
                       "Opened: ", opened, "<br>",
                       "Closed: ", ifelse(is.na(closed), "Still Open", closed),  "<br>",
                       "Approximate: ", date_approx),
        radius = 6,
        color = "#FF69B4",
        fillOpacity = 0.6
      )
  })
}

shinyApp(ui, server)
