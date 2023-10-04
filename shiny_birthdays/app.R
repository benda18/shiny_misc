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
library(dplyr)
#library(goeveg)  # for CV() coefficient of variation / 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Unnecessary Birthday Calculations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h3(HTML(r'(<b>Variable Inputs:)')),
          shiny::dateInput(inputId = "my_bday", 
                           label = "Enter Your Birthday", 
                           value = Sys.Date() %m-% years(40), 
                           startview = "decade"),
          h3(HTML(r'(<b>By The Numbers:)')),
          wellPanel(
            fluidRow(
              column(12,
                     fluidRow(
                       h4(HTML(r'(<u>Age in Years (by Planet)</u>)'))
                     ),
                     fluidRow(
                       tableOutput(
                         outputId = "planetary_ages01"
                       )
                     )
              )
            ),
            fluidRow(
              column(12,
                     fluidRow(
                       h4(HTML(r'(<u>Median Date of Conception for People Born on this Date:</u><h6><a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3777570/" target="_self">[link]</a></h6>)'))
                       ),
                     fluidRow(
                       textOutput(
                         outputId = "conception01"
                         )
                       )
                     )
              ),
            # next fluid row here for sidePanel
            fluidRow(
                h4(HTML(r'(<u>Zodiac Sign:</u>)')),
                fluidRow(column(4, 
                                textOutput(outputId = "zodiac")))
            ), 
            fluidRow(
              h4(HTML(r'(<u>Age In Days:</u>)')), 
              fluidRow(column(4,
                              textOutput(outputId = "ageindays_01")))
            ), 
            fluidRow(
              h4(HTML(r'(<u>Friday-the-13ths Survived:</u>)')), 
              fluidRow(column(4,
                              textOutput(outputId = "fri13_01")))
            )
          )
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
  require(scales)
  
  output$planetary_ages01 <- renderTable({
    
    get_planet.age <- function(yr_scalar = 1, 
                               a.date = input$my_bday){
      earth.age <- as.character(as.period(as_date(a.date) %--% Sys.Date(), "year")) %>%
        gsub(" 0H 0M 0S$", "", .) %>%
        strsplit(., " ") %>%
        unlist() %>%
        gsub("\\D", "", .) %>%
        as.numeric()
      earth.age <- round(earth.age[1] + earth.age[2]/12 + (earth.age[3]/30)/12,2)
      out.age <- earth.age / yr_scalar
      return(out.age)
    }
    
    
    
    df.earth2planet <- data.frame(planet = c("mercury", "venus", "earth",
                                                  "mars", "jupiter", "saturn", 
                                                  "uranus", "neptune", "moon"), 
                                  earth.yr.rat = c(0.240846, 0.615, 1,
                                                   1.881, 11.86, 29.46, 
                                                   84.01, 164.8, 0.0748), 
                                  age_on_planet = NA)
    
    df.earth2planet$age_on_planet <- signif(unlist(lapply(X = df.earth2planet$earth.yr.rat, 
           FUN = get_planet.age, 
           a.date = input$my_bday)), 
           digits = 3) %>% as.character()
    rm(get_planet.age)
    df.earth2planet$earth.yr.rat <- as.character(df.earth2planet$earth.yr.rat)
   
    df.earth2planet[,c(1,3)]
    
  })
  
  output$fri13_01 <- renderText({
    # number of f13ths survived
    sum(lubridate::mday(as_date(ymd(input$my_bday):Sys.Date())) == 13 & 
      lubridate::wday((as_date(ymd(input$my_bday):Sys.Date())), label = T, abbr = T) == "Fri")
  })
  
  output$ageindays_01 <- renderText({
    as.character(scales::comma(as.numeric(Sys.Date() - input$my_bday)))
  })
  
  output$conception01 <- renderText({
    ovulation.gd <- 268      #Ovulation (i.e. 268 days after ovulation)
    #lmp.gd <- 260 #LMP (i.e. 260 days after onset of last menstrual period)
    as.character(as_date(input$my_bday %m-% days(ovulation.gd)))
    
  })
  output$zodiac <- shiny::renderText({
    
    fun_get.zodiac <- function(a.date){
      a.month <- lubridate::month(a.date)
      a.mday  <- mday(a.date)
      
      if((a.month == 12 & a.mday %in% 22:31) |
         (a.month == 1  & a.mday %in% 1:19)){
        out.zod <- "Capricorn"
      }
      if((a.month == 1 & a.mday %in% 20:31) |
         (a.month == 2  & a.mday %in% 1:17)){
        out.zod <- "Aquarius"
      }
      if((a.month == 2 & a.mday %in% 18:31) |
         (a.month == 3  & a.mday %in% 1:19)){
        out.zod <- "Pisces"
      }
      if((a.month == 3 & a.mday %in% 20:31) |
         (a.month == 4  & a.mday %in% 1:19)){
        out.zod <- "Aries"
      }
      if((a.month == 4 & a.mday %in% 20:31) |
         (a.month == 5  & a.mday %in% 1:19)){
        out.zod <- "Taurus"
      }
      if((a.month == 5 & a.mday %in% 20:31) |
         (a.month == 6  & a.mday %in% 1:20)){
        out.zod <- "Gemini"
      }
      if((a.month == 6 & a.mday %in% 21:31) |
         (a.month == 7  & a.mday %in% 1:21)){
        out.zod <- "Cancer"
      }
      if((a.month == 7 & a.mday %in% 22:31) |
         (a.month == 8  & a.mday %in% 1:22)){
        out.zod <- "Leo"
      }
      if((a.month == 8 & a.mday %in% 23:31) |
         (a.month == 9  & a.mday %in% 1:21)){
        out.zod <- "Virgo"
      }
      if((a.month == 9  & a.mday %in% 22:31) |
         (a.month == 10  & a.mday %in% 1:22)){
        out.zod <- "Libra"
      }
      if((a.month == 10 & a.mday %in% 23:31) |
         (a.month == 11 & a.mday %in% 1:21)){
        out.zod <- "Scorpio"
      }
      if((a.month == 11 & a.mday %in% 22:31) |
         (a.month == 12 & a.mday %in% 1:21)){
        out.zod <- "Sagittarius"
      }
      return(out.zod)
    }
    
    fun_get.zodiac(ymd(input$my_bday))
    
  })
  
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
