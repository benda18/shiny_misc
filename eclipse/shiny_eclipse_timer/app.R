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
library(tigris)
#library(data.table)
library(shiny)
library(censusxy)
library(scales)

okistates <- readRDS("okistates.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("2024 Eclipse Planning Tool -
             Find out if a specific location will see totality, how long, and at what times."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow("Enter Lon/Lat Coordinates Below:"),
        fluidRow("On the right side of the page you can find Lon/Lat coordinates by entering a street address or clicking on the map.  You must copy these values into the boxes below however."),
        fluidRow(shiny::numericInput(inputId = "lon_in", 
                                     label = "Enter Longitude (x)", 
                                     value = -83.8119), 
                 shiny::numericInput(inputId = "lat_in", 
                                     label = "Enter Latitude (y)", 
                                     value = 39.92953)),
        fluidRow(shiny::textOutput("total_TF"))
      ),
      wellPanel(
        fluidRow("Eclipse Info"),
        fluidRow(shiny::tableOutput(outputId = "eclipse_info"))),
      wellPanel(
        fluidRow("Maximum Sun Blocked (%):"),
        fluidRow(shiny::textOutput(outputId = "totality_dur"))
      )
      
      
    ),
    
    
    # Map----
    mainPanel(
      wellPanel(
        fluidRow("Don't Know Coordinates? Enter the Address Here. Lon/Lat coordinates will be displayed below - just type them into the box at left"), 
        shiny::textInput(inputId = "addr_in", 
                         label = "Enter Address", 
                         value = "107 Cliff Park Rd, Springfield, OH 45504"),
        actionButton(inputId = "cxy_go", 
                     label   = "SEARCH"), 
        # actionButton(inputId = "upd_go", 
        #              label = "UPDATE TABLE")
        fluidRow("Lon/Lat:"), 
        fluidRow(shiny::textOutput("coord_lon"))
      ),
      wellPanel(fluidRow("Click on Map Below to find the corresponding Lon(x), Lat(y) coordinates:"),
                textOutput(outputId = "plot_hover")),
      plotOutput("okiMap", 
                 click = clickOpts(id = "plot_hover",
                                   #delay = 800, 
                                   #delayType = "debounce", 
                                   clip = T))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot_hover <- renderText({
    paste(round(as.numeric(unlist(input$plot_hover)[1:2]),5), 
          sep = ", ", collapse = ", ")
  })
  
  output$okiMap <- renderPlot(width = 800, 
                              height = "auto", 
                              {
                                
                                try(ggplot() + 
                                  geom_sf(data = okistates)+
                                  geom_point(aes(x = input$lon_in, 
                                                 y = input$lat_in), 
                                             color = "red", shape = "x", size = 8, fill = "red")+
                                  theme(plot.background = element_rect(fill = "cyan"), 
                                        text = element_text(size = 18))+
                                  scale_x_continuous(name = NULL)+
                                  scale_y_continuous(name = NULL))
                              })
  
  cen_lon <- eventReactive(eventExpr = input$cxy_go, {
    paste(round(censusxy::cxy_oneline(address = input$addr_in)[,c("coordinates.x", "coordinates.y")], 
                5), 
          sep = ", ", collapse = ", ")
  })
  output$coord_lon <- renderText({
    cen_lon()
  })
  
  # # get totality duration----
  # output$totality_dur <- renderText({
  #   # ECLIPSE MATH----
  #   
  #   lon_in <- input$lon_in #-81.44067000
  #   lat_in <- input$lat_in # 41.24006000
  #   
  #   # set date_time of eclipse (ideally before solar eclipse begins)----
  #   
  #   greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
  #   tz.local      <- tz(greg_dt.local)
  #   
  #   # do the time conversions----
  #   # convert to utc
  #   greg_dt.utc <- with_tz(greg_dt.local, tz = "UTC")
  #   jul_dt.utc  <- swephR::swe_julday(year  = year(greg_dt.utc), 
  #                                     month = lubridate::month(greg_dt.utc, label = F), 
  #                                     day   = mday(greg_dt.utc), 
  #                                     hourd = hour(greg_dt.utc) + 
  #                                       (minute(greg_dt.utc)/60) + 
  #                                       (second(greg_dt.utc)/60/60), 
  #                                     gregflag = 1)
  #   
  #   ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
  #                                                   ephe_flag = 4, 
  #                                                   geopos    = c(x = lon_in, 
  #                                                                 y = lat_in, 
  #                                                                 z = 10), 
  #                                                   backward = F)
  #   
  #   ewl_out$tret <- ewl_out$tret[c(2,4)]
  #   
  #   out.times <- data.frame(time_val.jul     = ewl_out$tret, 
  #                           local_time       = NA)
  #   
  #   for(i in 1:nrow(out.times)){
  #     out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
  #                             gregflag = 1) %>%
  #       paste(., 
  #             sep = "-", 
  #             collapse = "-") %>%
  #       ymd_hms()
  #   }
  #   
  #   ewl_out$attr <- ewl_out$attr[1+c(2)]
  #   
  #   as.character(max(out.times$local_time) - min(out.times$local_time))
  #   
  # })
  
  # get eclipse times----
  output$eclipse_info <- renderTable({
    # ECLIPSE MATH----
    
    lon_in <- input$lon_in #-81.44067000
    lat_in <- input$lat_in # 41.24006000
    
    # set date_time of eclipse (ideally before solar eclipse begins)----
    
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # do the time conversions----
    # convert to utc
    greg_dt.utc <- with_tz(greg_dt.local, tz = "UTC")
    jul_dt.utc  <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                      month = lubridate::month(greg_dt.utc, label = F), 
                                      day   = mday(greg_dt.utc), 
                                      hourd = hour(greg_dt.utc) + 
                                        (minute(greg_dt.utc)/60) + 
                                        (second(greg_dt.utc)/60/60), 
                                      gregflag = 1)
    
    ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in, 
                                                                  y = lat_in, 
                                                                  z = 10), 
                                                    backward = F)
    
    ewl_out$tret <- ewl_out$tret[1:5]
    
    
    ewl_out$attr <- ewl_out$attr[1+c(2)]
    #names(ewl_out$attr) <- c("pct_obscuration")
    
    # out_times----
    out.times <- data.frame(time_val.jul     = ewl_out$tret, 
                            local_time       = NA)
    
    for(i in 1:nrow(out.times)){
      out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
                                                         gregflag = 1) %>%
        paste(., 
              sep = "-", 
              collapse = "-") %>%
        ymd_hms() %>%
        with_tz(., tz.local) %>%
        strftime(., 
                 format = "%m-%d-%y %I:%M:%S%p %Z", 
                 #format = "%I:%M:%S%p %Z",
                 tz = tz.local) %>%
        as.character() #%>%
        #tolower() %>%
        # gsub("^0", "", .) %>%
        # gsub("AM ", "am ", .) %>%
        # gsub("PM ", "pm ", .)
    }
    
    out.times <- out.times[order(out.times$local_time),]
    out.times$eclipse_event <- c("begin_partial_eclipse", 
                                 "begin_total_eclipse", 
                                 "maximum_total_eclipse", 
                                 "end_total_eclipse", 
                                 "end_partial_eclipse")
    rownames(out.times) <- 1:nrow(out.times)
    out.times <- out.times[,c("eclipse_event", "local_time")]
    
    # out_attributes----
    out.attr <- data.frame(longitude = lon_in, 
                           latitude  = lat_in,
                           total_ecl_at_loc = ewl_out$attr > 1)
    rownames(out.attr) <- 1:nrow(out.attr)
    out.attr
    # out.times$lon <- out.attr$longitude
    # out.times$lat <- out.attr$latitude
    # out.times$t_ecl <- out.attr$total_ecl_at_loc
    
    if(!out.attr$total_ecl_at_loc){
      #out.times$local_time[2:4] <- NA
      out.times$local_time <- "partial eclipse only"
    }else{
      out.times$duration_minutes <- scales::comma(c(0,
                                   diff(ymd_hms(out.times$local_time))),0.1)
      out.times$local_time <- out.times$local_time %>%
        gsub("^.*-\\d{2,2} ", "", .) %>%
        gsub("^0", "", .) %>%
        gsub("AM ", "am ", .) %>%
        gsub("PM ", "pm ", .)
    }
    
    out.times
  })
  
  # get totality duration----
  output$totality_dur <- renderText({
    # ECLIPSE MATH----
    
    lon_in <- input$lon_in #-81.44067000
    lat_in <- input$lat_in # 41.24006000
    
    # set date_time of eclipse (ideally before solar eclipse begins)----
    
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # do the time conversions----
    # convert to utc
    greg_dt.utc <- with_tz(greg_dt.local, tz = "UTC")
    jul_dt.utc  <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                      month = lubridate::month(greg_dt.utc, label = F), 
                                      day   = mday(greg_dt.utc), 
                                      hourd = hour(greg_dt.utc) + 
                                        (minute(greg_dt.utc)/60) + 
                                        (second(greg_dt.utc)/60/60), 
                                      gregflag = 1)
    
    ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in, 
                                                                  y = lat_in, 
                                                                  z = 10), 
                                                    backward = F)$attr[3]
    scales::percent(ewl_out,0.00000001)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
