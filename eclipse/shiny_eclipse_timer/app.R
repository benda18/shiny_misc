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
                         value = "107 Cliff Park Rd, Springfield, OH 45504"),
        actionButton(inputId = "cxy_go", 
                     label   = "SEARCH"), 
        # actionButton(inputId = "upd_go", 
        #              label = "UPDATE TABLE")
        fluidRow("Lon/Lat:"
                 # column(5,"longitude:"),
                 # column(5,"latitude:")
        ), 
        fluidRow(shiny::textOutput("coord_lon")
                 # column(5, shiny::textOutput("coord_lon")), 
                 # column(5, shiny::textOutput("coord_lat"))
        )
      ),
      wellPanel(
        #column(10, 
               
               fluidRow(shiny::numericInput(inputId = "lon_in", 
                                         label = "Enter Longitude (x)", 
                                         value = -83.8119), 
                        shiny::numericInput(inputId = "lat_in", 
                                         label = "Enter Latitude (y)", 
                                         value = 39.92953)),
               fluidRow(shiny::textOutput("total_TF"))
               #),
        
        # shiny::textInput(inputId = "lon_in",
        #                  label = "Longitude",
        #                  value = cordx),
      ),
      wellPanel(
        fluidRow("Eclipse Info"), 
        fluidRow(
          shiny::tableOutput(outputId = "eclipse_info")
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
  output$coord_lon <- renderText({
    cen_lon()
  })
  
  #update_tbl <- eventReactive(eventExpr = input$upd_go, {
    #output$eclipse_info <- renderTable({
      
      
      
      
    #})
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
                   tz = tz.local)
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
      
      # if(!out.attr$total_ecl_at_loc){
      #   #out.times$local_time[2:4] <- NA
      #   out.times$local_time <- "partial eclipse only"
      # }
      
      out.times
    })
  #})
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
