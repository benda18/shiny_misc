library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(tigris)



rm(list=ls());cat('\f');gc()


# UI----
ui <- fluidPage(
  
  # Application title
  titlePanel("[title]"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
)

# SERVER----
server <- function(input, output) {
  # load data
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
