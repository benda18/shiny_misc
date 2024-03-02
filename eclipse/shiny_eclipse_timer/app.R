#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(renv)
library(swephR)
library(lubridate)
library(dplyr)
library(ggplot2)
#library(tigris)
#library(data.table)
library(shiny)
library(censusxy)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("TITLE"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow("Census Geocoder Here"), 
        shiny::textInput(inputId = "addr_in", 
                         label = "Enter Address", 
                         value = "1060 W Addison St, Chicago, IL"),
        actionButton(inputId = "cxy_go", 
                     label   = "SEARCH")
      ),
      wellPanel(
        fluidRow("Lon/Lat:"
                 # column(5,"longitude:"),
                 # column(5,"latitude:")
        ), 
        fluidRow(shiny::textOutput("coord_lon")
                 # column(5, shiny::textOutput("coord_lon")), 
                 # column(5, shiny::textOutput("coord_lat"))
        ),
        
        # shiny::textInput(inputId = "lon_in",
        #                  label = "Longitude",
        #                  value = cordx),
      ),
      wellPanel(
        fluidRow("Eclipse Info"), 
        fluidRow(
          shiny::textOutput(outputId = "eclipse_info")
        )
      )
      # wellPanel(fluidRow("External Web Resources"),
      #           fluidRow("https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/where-when/"))
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  cen_lon <- eventReactive(eventExpr = input$cxy_go, {
    paste(round(censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.x", "coordinates.y")], 
                5), 
          sep = ", ", collapse = ", ")
  })
  # cen_lat <- eventReactive(eventExpr = input$cxy_go, {
  #   #censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.y")]
  # })
  output$coord_lon <- renderText({
    cen_lon()
  })
  # output$coord_lat <- renderText({
  #   cen_lat()
  # })
  
  output$eclipse_info <- renderText({
    "Foo"
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
