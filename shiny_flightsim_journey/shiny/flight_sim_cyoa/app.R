library(shiny)
library(dplyr)
library(rnaturalearthdata)
library(ggplot2)
library(ggrepel)

# load data----
load("shinyData.RData")

calculate_distance <- function(lon1, lat1, lon2, lat2) {
  require(geosphere)
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# pre-filter----
#data_airports <- 
data_airports <- data_airports %>%
  .[!.$type %in% c("closed", "balloonport", "heliport", "seaplane_base"),] %>%
  .[nchar(.$ident) == 4,] %>%
  .[!is.na(.$elevation_ft),]

data_runways <- data_runways %>%
  .[.$airport_ident %in% data_airports$ident,] %>%
  .[.$closed == 0,] %>%
  .[grepl("^asp|^con", .$surface, ignore.case = T),] %>%
  .[!grepl("-", .$surface, ignore.case = T),] %>%
  .[.$lighted == 1,] %>%
  .[!is.na(.$le_longitude_deg) & 
      !is.na(.$le_latitude_deg) & 
      !is.na(.$he_longitude_deg) & 
      !is.na(.$he_latitude_deg),] %>%
  group_by(airport_ident) %>%
  slice_max(., order_by = length_ft, n = 1)

data_airports <- data_airports[data_airports$ident %in% data_runways$airport_ident,]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Title"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Maximum Plane Range
      sliderInput(inputId = "planeRange", 
                  "Max Plane Range (mi)", 
                  min = 0, 
                  max = 10000, 
                  value = 100),
      # elevation filter
      sliderInput(inputId = "runwayElev",
                  "Runway Elevation (ft)",
                  min = -1500,
                  max = 15000,
                  value = c(0,3000)),
      # runway length filter
      sliderInput(inputId = "runwayLen", 
                  "Runway Length (ft)", 
                  min = 0, 
                  max = 30000, 
                  value = c(100,4000)),
      # continent
      checkboxGroupInput(inputId = "sel_continents", 
                         label = "Continent", 
                         choices = NULL, 
                         selected = c("NA"), 
                         inline = F, 
                         width = NULL, 
                         choiceNames = c("North America", "South America", "Europe", 
                                         "Africa", "Asia", "Oceania", "Antarctica"), 
                         choiceValues = c("NA", "SA", "EU", "AF", "AS", "OC", "AN"))
      # start airport
      # end airport
      # click button to choose
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotOutput("flightPlot")
      ),
      fluidRow(
        plotOutput("airportsPlot")
      )
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # airports plot----
  output$airportsPlot <- renderPlot({
    ggplot() + 
      geom_sf(data = sf::st_as_sf(rnaturalearthdata::countries110), 
              fill = "white", color = "grey")+
      theme(panel.background = element_rect(fill = "powderblue"), 
            plot.background = element_rect(fill = "light grey"), 
            strip.background =  element_rect("red"), 
            axis.line = element_blank(), 
            panel.grid = element_line(color = "skyblue", linetype = 23))+
      geom_point(data = data_airports[data_airports$continent %in% input$sel_continents,], 
                 aes(x = longitude_deg, y = latitude_deg))+
      coord_sf()
      
  })
  
  # flight plot----
  output$flightPlot <- renderPlot({
    
    #some.apts <- c("KCVG", "KLAX")
    some.apts <- sample(data_airports$ident,2,replace=F)
    
    rw.metadata <- data_runways[data_runways$airport_ident %in% some.apts,] %>%
      group_by(airport_ident) %>%
      slice_max(., order_by = length_ft, n = 1) %>%
      group_by(ident = airport_ident, 
               surface, 
               length_ft, 
               elevation = le_elevation_ft) %>%
      summarise()
    
    out.plot <- ggplot(data = data_airports[data_airports$ident %in% some.apts,]) + 
      geom_sf(data = sf::st_as_sf(rnaturalearthdata::countries110), 
              fill = "white", color = "grey")+
      geom_path(aes(x = longitude_deg, y = latitude_deg)) +
      geom_label_repel(min.segment.length = 0,
                       aes(x = longitude_deg, y = latitude_deg, 
                           label = ident))+
      geom_point(aes(x = longitude_deg, y = latitude_deg))+
      coord_sf() +
      theme(panel.background = element_rect(fill = "powderblue"), 
            plot.background = element_rect(fill = "light grey"), 
            strip.background =  element_rect("red"), 
            axis.line = element_blank(), 
            panel.grid = element_line(color = "skyblue", linetype = 23))+
      labs(title = paste(data_airports[data_airports$ident %in% some.apts,]$name,
                         collapse = " >>> "),
           subtitle = glue("Elevation: {paste(scales::comma(rw.metadata$elevation), \"ft\",  collapse = \", \")}
  Runway Length: {paste(scales::comma(rw.metadata$length_ft),  \"ft\", collapse = \", \")}
  Runway Surface: {paste(rw.metadata$surface,collapse = \", \")}
  Flight Distance: {scales::comma(calculate_distance(lon1 = data_airports$longitude_deg[data_airports$ident == some.apts[1]],
                   lat1 = data_airports$latitude_deg[data_airports$ident == some.apts[1]],
                   lon2 = data_airports$longitude_deg[data_airports$ident == some.apts[2]],
                   lat2 = data_airports$latitude_deg[data_airports$ident == some.apts[2]])/1609.34)} miles\n"))
    
    print(out.plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
