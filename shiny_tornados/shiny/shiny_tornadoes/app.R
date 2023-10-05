library(shiny)
library(dplyr)
#library(readr)
library(lubridate)
#library(tigris)
library(ggplot2)



rm(list=ls());cat('\f');gc()


# UI----
ui <- fluidPage(
  
  # Application title
  titlePanel("[title]"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               shiny::sliderInput(inputId = "sel_year", 
                                   label   = "Filter Year(s)", 
                                   value   = c(min(allT$yr),max(allT$yr)), 
                                   min     = min(allT$yr), 
                                   max     = max(allT$yr), 
                                  sep = "")
      )
      
    )
    ),
    mainPanel(
      
    )
  )
)

# SERVER----
server <- function(input, output) {
  # load data
  
  output$plot01 <- shiny::renderPlot({
    load("tornado.RData")
    
    
  })
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
