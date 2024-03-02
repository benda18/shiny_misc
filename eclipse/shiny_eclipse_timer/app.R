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
        fluidRow(
          column(5,"longitude:"),
          column(5,"latitude:")), 
        fluidRow(
          column(5, shiny::textOutput("coord_lon")), 
          column(5, shiny::textOutput("search_lonlat"))),
        
        # shiny::textInput(inputId = "lon_in",
        #                  label = "Longitude",
        #                  value = cordx),
      ),
      wellPanel(fluidRow("External Web Resources"),
                fluidRow("https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/where-when/"))
     
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  search_lonlat <- eventReactive(input$cxy_go, {
    renderText({
      #censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.x")]
      "foo"
      
    })
  })
  
  output$coord_lon <- renderText({
    search_lonlat
  })
    
  
    # output$coord_lon <- renderText({
    #   cordx <- search_lonlat
    # })
    # 
    # output$coord_lat <- renderText({
    #   cordy <- search_lonlat
    # })
     
    # output$coord_lon <- renderText({
    #     cordx <- censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.x")]
    # })
    # output$coord_lat <- renderText({
    #   cordy <- censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.y")]
    # })
   
 
  
  
   

}

# Run the application 
shinyApp(ui = ui, server = server)
