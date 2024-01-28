#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(readr)
library(lubridate)
# library(readr)
 library(ggmap)
# library(lubridate)
 library(data.table)


#rsconnect::deployApp()


#stadiamap set api----
register_stadiamaps(key = "[STADIA MAP API KEY HERE]", write = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ERAS Tour"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "sliderDates",
                  "Select Tour Dates Range:",
                  min = ymd(20230317), #ymd(20180508),
                  max = ymd(20241123),
                  value = ymd(c(20230317,20241123)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "oaPlot"),
      plotOutput(outputId = "geoPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(readr)
  library(ggmap)
  library(lubridate)
  library(data.table)
  
  output$geoPlot <- renderPlot({
    #eras_date.geo <- read_csv("C:/Users/bende/Documents/R/play/eras_tour/data/eras_date.geo.csv")
    eras_date.geo <- read_csv("eras_date.geo.csv")
    #repu_date.geo <- read_csv("C:/Users/bende/Documents/R/play/eras_tour/data/reputation_date.geo.csv")
    #repu_date.geo <- read_csv("reputation_date.geo.csv")
    
    #eras_date.geo <- rbind(eras_date.geo, 
    #                      repu_date.geo)
    
    next_show.date <- min(eras_date.geo[eras_date.geo$date >= Sys.Date(),]$date)
    next_show.geo  <- eras_date.geo[eras_date.geo$date == next_show.date,]
    
    
    eras_date.geo2 <- eras_date.geo[data.table::between(x = eras_date.geo$date, 
                                                        lower = min(input$sliderDates), 
                                                        upper = max(input$sliderDates)),]
    edg.stamen <- get_stadiamap(bbox = c(left   = min(eras_date.geo2$long)*1.00,
                                         #edg.stamen <- get_stamenmap(bbox = c(left   = min(eras_date.geo2$long)*1.00, 
                                         bottom = min(eras_date.geo2$lat)*1.00, 
                                         top    = max(eras_date.geo2$lat)*1.00, 
                                         right  = max(eras_date.geo2$long)*1.00), 
                                zoom = 3, 
                                maptype = "stamen_terrain",                            
                                # maptype = "terrain", 
                                crop = F, 
                                color = "color",
                                force = T,
                                size = 1)
    
    ggmap(edg.stamen) +
      geom_point(data = next_show.geo, 
                 aes(x = long, y = lat, 
                     color = promo_city), 
                 size = 10) +
      scale_color_manual(name = "Next Show Location", 
                         values = "cyan")+
      geom_path(data = eras_date.geo2, 
                aes(x = long, y = lat), 
                color = "black", 
                linewidth = 1.2)+
      geom_point(data = eras_date.geo2,
                 shape = 19,
                 size = 6, color = "black",
                 aes(x = long, y = lat)) +
      geom_point(data = eras_date.geo2,
                 shape = 19,
                 size = 4, color = "orange",
                 aes(x = long, y = lat)) +
      
      labs(title = "Tour Locations")+
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            text = element_text(size = 15))+
      theme(plot.background = element_rect(color = "black"))
  })
  
  output$slPlot <- renderPlot({
    #setwd("~/R/play/eras_tour/data")
    #sl.files <- list.files(pattern = "^sl_\\d{8,8}\\.csv$")
    sl.files <- list.files(pattern = "^sl_\\d{8,8}\\.csv$")
    eras_sl <- NULL
    for(i in sl.files){
      eras_sl <- rbind(eras_sl, 
                       read_csv(i))
    }
    
    eras_sl %>%
      group_by(album) %>%
      summarise(n_songs = n_distinct(song))
    
    eras_sl %>%
      group_by(song) %>%
      summarise(n_shows = n()) %>%
      .[order(.$song),]
    
    adist(unique(eras_sl$song), ignore.case = T)
    
    
    which(data.table::between(adist(unique(eras_sl$song), ignore.case = T), 
                              lower = 1, upper = 3))
    
  })
  
  output$oaPlot <- renderPlot({
    #eras_date.oa <- read_csv("C:/Users/bende/Documents/R/play/eras_tour/data/eras_date.oa.csv")
    eras_date.oa <- read_csv("eras_date.oa.csv")
    eras_date.oa$opening_acts2_f <- factor(eras_date.oa$opening_acts2, 
                                           levels = unique(eras_date.oa$opening_acts2[order(eras_date.oa$join_tour,decreasing = T)]), 
                                           exclude = c("no_opening_acts_scheduled", 
                                                       "opening_acts_cancelled_weather"))
    
    ggplot() + 
      geom_segment(data = eras_date.oa[!is.na(eras_date.oa$opening_acts2_f),], 
                   aes(x = join_tour.did, xend = leave_tour.did, 
                       y = opening_acts2_f, yend = opening_acts2_f, 
                       linewidth = n_dates_performing)) + 
      labs(title = "Opening Acts")+
      theme(plot.background = element_rect(color = "black"), 
            text = element_text(size = 15))
  })
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #        xlab = 'Waiting time to next eruption (in mins)',
  #        main = 'Histogram of waiting times')
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
