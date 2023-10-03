#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Unnecessary Birthday Calculations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          shiny::dateInput(inputId = "my_bday", 
                           label = "Enter Your Birthday", 
                           value = Sys.Date() %m-% years(40), 
                           startview = "decade")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          shiny::fluidRow(
            shiny::wellPanel(
              h4(HTML(r'(Celebrate Your Birthday not every 365.25 days, but every 1,000 on the following dates:)')),
              fluidRow(column(4, 
                     
                     shiny::tableOutput(outputId = "bday_1000.p")
                     ),
                     column(4,
                            shiny::tableOutput(outputId = "bday_1000.f"))
              )
            )
          )
          
           #plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bday_1000.p <- shiny::renderTable({
    out <- as.character(as_date(as_date(input$my_bday) %m+% 
                                  days(seq(0,30000, by = 1000))))
    outprev <- out[(which(out <= Sys.Date()))]
    #outfut <- out[which(out > Sys.Date())]
    
    data.frame(previous_bdays = outprev)
    #data.frame(future_bdays = outfut)
    })
  output$bday_1000.f <- shiny::renderTable({
    out <- as.character(as_date(as_date(input$my_bday) %m+% 
                                  days(seq(0,30000, by = 1000))))
    #outprev <- out[(which(out <= Sys.Date()))]
    outfut <- out[which(out > Sys.Date())]
    
    #data.frame(previous_bdays = outprev)
    data.frame(future_bdays = outfut)
  })
  output$bday_n_leapyears <- renderText({
    
  })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
