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

#okistates <- readRDS("okistates.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("2024 Eclipse Planning Tool -
             Find out when and if a specific location will see totality."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    # Map----
    sidebarPanel(
      wellPanel(
        #fluidRow("Enter the Address Here. Lon/Lat coordinates will be displayed below - just type them into the box at left"), 
        shiny::textInput(inputId = "addr_in", 
                         label = "Enter Address", 
                         value = sample(c("107 Cliff Park Rd, Springfield, OH 45504", 
                                          "1060 W Addison St, Chicago, IL 60613"),1)),
        actionButton(inputId = "cxy_go", 
                     label   = "SEARCH ADDRESS"), 
        fluidRow(
          uiOutput("tab")
        )
        #fluidRow("Lon/Lat:"), 
        # cen_lon button----
        #fluidRow(shiny::textOutput("coord_lon")),
        #fluidRow(shiny::textOutput("rando_letter"))
        # /cen_lon button----
      )#,
      # wellPanel(fluidRow("Click on Map Below to find the corresponding Lon(x), Lat(y) coordinates:"),
      #           textOutput(outputId = "plot_hover")),
      # plotOutput("okiMap", 
      #            click = clickOpts(id = "plot_hover",clip = T))
      
    ),
    mainPanel(
      wellPanel(
        fluidRow("See Eclipse Info Below:"),
        fluidRow(shiny::tableOutput(outputId = "return_eclipsego"))
      ), 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  url <- a("link to interactive map from National Solar Observatory", href="https://nso.edu/for-public/eclipse-map-2024/", 
           target="_blank")
  output$tab <- renderUI({
    tagList("See Also:", url)
  })
  # get address----
  # cen_lon <- eventReactive(eventExpr = input$cxy_go, {
  #   paste(
  #     round(
  #       cxy_oneline(address = input$addr_in)[,c("coordinates.x", "coordinates.y")], 
  #       5), 
  #     sep = ", ", collapse = ", ")
  # })
  # output$coord_lon <- renderText({
  #   cen_lon()
  # })
  
  
  
  
  # get eclipse times----
  submit_coords <- eventReactive(eventExpr = input$cxy_go, {
    
    
    temp <- censusxy::cxy_oneline(address = input$addr_in)
    
    #lon_in <- input$lon_in #-81.44067000
    #lat_in <- input$lat_in # 41.24006000
    
    lon_in <- temp$coordinates.x
    lat_in <- temp$coordinates.y
    
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
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
    
    # out_times
    out.times <- data.frame(time_val.jul     = ewl_out$tret, 
                            local_time       = NA)
    
    for(i in 1:nrow(out.times)){
      out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
                                                         gregflag = 1) %>%
        paste(., sep = "-", collapse = "-") %>%
        ymd_hms() %>%
        with_tz(., tz.local) %>%
        strftime(., 
                 format = "%m-%d-%y %I:%M:%S%p %Z", 
                 tz = tz.local) %>%
        as.character() 
    }
    
    out.times <- out.times[order(out.times$local_time),]
    out.times$eclipse_event <- c("start", 
                                 "start_totality", 
                                 "max_eclipse", 
                                 "end_totality", 
                                 "end")
    rownames(out.times) <- 1:nrow(out.times)
    out.times <- out.times[,c("eclipse_event", "local_time")]
    
    # out_attributes
    out.attr <- data.frame(longitude = lon_in, 
                           latitude  = lat_in,
                           total_ecl_at_loc = ewl_out$attr > 1)
    rownames(out.attr) <- 1:nrow(out.attr)
    out.attr
    
    if(!out.attr$total_ecl_at_loc){
      out.times$local_time <- "partial eclipse only"
    }else{
      out.times$local_time <- out.times$local_time %>%
        gsub("^.*-\\d{2,2} ", "", .) %>%
        gsub("^0", "", .) %>%
        gsub("AM ", "am ", .) %>%
        gsub("PM ", "pm ", .)
    }
    
    out.times
  })
  
  output$return_eclipsego <- renderTable({
    submit_coords()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
