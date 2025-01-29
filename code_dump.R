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


# Not working
output$map <- renderLeaflet({
  
  leaflet(data = data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(-74.00, 40.71, zoom = 11)
  
})

observe({
  leafletProxy("map", data = filtereddata()) %>% 
    clearMarkers() %>% 
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




ui = page_navbar(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  title = "1970-1986 NYC Queer Spaces Map",
  nav_panel(title = "About the Map/Instructions",
            h2("About the Map/How to Use It"),
            p("Below is a map of the five boroughs of New York City's queer bars, clubs, 
      bathhouses, bookstores, cinemas, restaurants, sex stores, backrooms, and cruising 
      locations open between 1970-1986. Some of the establishments were open before 1970 and many of 
      them close after 1986 however those are the bounding years of my study. I have still included the
      dates on the slider that fall outside of my temporal interest so that it is a more honest
      representation of the data. Many of the dates are approximate because of the fleeting nature of
      queer establishments. The majority of my dates were found either through the NY Times, other online 
      jounrals and forums, Bob Damon's Address Book, Ciao! Magazine, Michael's Thing, and other gay travel guides.
      To use the map select the types of locations and which borough you are interested in seeing and drag the
      date filter to the dates you want then click on the points on the map for more specific information about
      the establishment. Any establishment with a yes answer for the approximate category means that I made an educated
      guess on the open and close dates.")
  ),
  nav_panel(title = "Interactive Map"),
  fluidRow(
    column(3, offset = 1,
           selectInput("filter_type", "Filter by Type:", 
                       choices = c("All", "bar","restaurant","bar/club", "bar/restaurant", "club", "cruising", 
                                   "baths", "disco", "bookstore", "erotica", "cinema", "theatre", 
                                   "gym", "bar/disco", "cabaret", "bar/cabaret", "massage", "backroom", "bar/backroom", "bar/club/backroom", 
                                   "bar/restaurant/cabaret","disco/backroom", "disco/cabaret", "erotica/cruising", "juice bar/disco",
                                   "restaurant/café", "private club"), 
                       selected = "All")
    ),
    column(3,
           selectInput("filter_borough", "Filter by Borough",
                       choices = c("All", "Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"),
                       selected = "All")
    ),
    column(5,
           sliderInput("filter_years", "Filter by Years of Operation:", 
                       min = 1900, max = 2025, value = c(1900, 2025), step = 1, sep = "")
    )),
  fluidRow(
    column(12, leafletOutput("map", height = 800)) # Output for the Leaflet map
  )
)

nav_menu(
  title = "links",
  align = "right",
  nav_item(link_shiny),
  nav_item(link_website)
)