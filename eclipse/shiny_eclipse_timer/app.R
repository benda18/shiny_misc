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

# circle_xy = function(n, r, close_loop = FALSE) {
#   theta = seq(0, 2 * pi, length.out = n + 1)
#   if(!close_loop) theta = theta[-(n + 1)]
#   cbind(x = r * cos(theta), y = r * sin(theta))
# }
# 
# shape_sun   <- as.data.frame(circle_xy(n = 25, r = 10, close_loop=T))
# shape_moon  <- as.data.frame(circle_xy(n = 25, r = 10.5, close_loop=T))
# 
# 
# 
# eclipse.plot <- ggplot() + 
#   geom_polygon(data = shape_sun, fill = "darkorange", color = NA, linewidth = 2,
#                aes(x = x, y = y)) +
#   geom_polygon(data = shape_moon, alpha = 0.6, fill = "black", color = "black", 
#                aes(x = (x-2), y = (y-3))) +
#   
#   geom_polygon(data = shape_sun, fill = "darkorange", color = NA, linewidth = 2,
#                aes(x = x+25, y = y+0)) +
#   geom_polygon(data = shape_moon, alpha = 0.6, fill = "black", color = "black", 
#                aes(x = x+25, 
#                    y = y)) +
#   geom_text(data = data.frame(x = mean(shape_sun$x), 
#                               y = mean(shape_sun$y)), size = 6,
#             aes(x = x-1.5, y = y+0), label = "Partial\nEclipse")+
#   geom_text(data = data.frame(x = mean(shape_sun$x), 
#                               y = mean(shape_sun$y)), size = 6,
#             aes(x = x-1.5+25, y = y+0), label = "Total\nEclipse")+
#   coord_quickmap()+
#   #theme_minimal()+
#   theme_void()+
#   theme(legend.position = "none")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("2024 Eclipse Planning Tool -
             Find out when and if a specific location will see totality."),
  
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
        fluidRow(actionButton(inputId = "eclipse_go",
                              label   = "SUBMIT"), ),
        fluidRow(shiny::textOutput("total_TF"))
      ),
      wellPanel(
        fluidRow("Eclipse Info"),
        #fluidRow(shiny::tableOutput(outputId = "eclipse_info"))
        fluidRow(shiny::tableOutput(outputId = "return_eclipsego"))
      ), 
      # wellPanel(
      #   fluidRow("Maximum Sun Blocked (%):"),
      #   fluidRow(shiny::textOutput(outputId = "totality_dur"))
      # )
    ),
    
    # Map----
    mainPanel(
      wellPanel(
        fluidRow("Don't Know Coordinates? Enter the Address Here. Lon/Lat coordinates will be displayed below - just type them into the box at left"), 
        shiny::textInput(inputId = "addr_in", 
                         label = "Enter Address", 
                         value = "107 Cliff Park Rd, Springfield, OH 45504"),
        actionButton(inputId = "cxy_go", 
                     label   = "SEARCH ADDRESS"), 
        fluidRow("Lon/Lat:"), 
        # cen_lon button----
        fluidRow(shiny::textOutput("coord_lon")),
        fluidRow(shiny::textOutput("rando_letter"))
        # /cen_lon button----
      ),
      wellPanel(fluidRow("Click on Map Below to find the corresponding Lon(x), Lat(y) coordinates:"),
                textOutput(outputId = "plot_hover")),
      plotOutput("okiMap", 
                 click = clickOpts(id = "plot_hover",clip = T))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot_hover <- renderText({
    paste(round(as.numeric(unlist(input$plot_hover)[1:2]),5), 
          sep = ", ", collapse = ", ")
  })
  
  # lon/lat input box error handling----
  
  # reactive({
  #   if(is.na(input$lon_in)){
  #     input$lon_in <- -83.8119
  #   }
  #   if(is.na(input$lon_in)){
  #     input$lon_in <- 39.92953
  #   }
  # })
  
  # NEW MAP STUFF----
  try(gen_map <- eventReactive(eventExpr = input$eclipse_go, {
    #try(
    ggplot() + 
      geom_sf(data = okistates)+
      geom_point(aes(x = input$lon_in, 
                     y = input$lat_in), 
                 color = "red", shape = "x", size = 8, fill = "red")+
      theme(plot.background = element_rect(fill = "cyan"), 
            text = element_text(size = 18))+
      scale_x_continuous(name = NULL)+
      scale_y_continuous(name = NULL)
    #)
  }))
  try(output$okiMap <- renderPlot(width = 800, 
                                  height = "auto", 
                                  {
                                    gen_map()
                                  }))
  # /NEW MAP STUFF----
  
  
  # # OLD MAP STUFF----
  # output$okiMap <- renderPlot(width = 800, 
  #                             height = "auto", 
  #                             {
  #                               #try(
  #                               ggplot() + 
  #                                 geom_sf(data = okistates)+
  #                                 geom_point(aes(x = input$lon_in, 
  #                                                y = input$lat_in), 
  #                                            color = "red", shape = "x", size = 8, fill = "red")+
  #                                 theme(plot.background = element_rect(fill = "cyan"), 
  #                                       text = element_text(size = 18))+
  #                                 scale_x_continuous(name = NULL)+
  #                                 scale_y_continuous(name = NULL)
  #                               #)
  #                             })
  # # /OLD MAP STUFF----
  
  
  # cen_lon button----
  ## SHOW WHAT HAPPENS WHEN YOU add another thing reacting to the same button
  # cen_rando <- eventReactive(eventExpr = input$cxy_go, {
  #   sample(letters, 1)
  # })
  # output$rando_letter <- renderText({
  #   cen_rando()
  # })
  ## /
  cen_lon <- eventReactive(eventExpr = input$cxy_go, {
    paste(
      round(
        cxy_oneline(address = input$addr_in)[,c("coordinates.x", "coordinates.y")], 
        5), 
      sep = ", ", collapse = ", ")
  })
  output$coord_lon <- renderText({
    cen_lon()
  })
  # /cen_lon button----
  
  
  # get eclipse times----
  
  # ADDED TO REPLACE BELOW----
  submit_coords <- eventReactive(eventExpr = input$eclipse_go, {
    lon_in <- input$lon_in #-81.44067000
    lat_in <- input$lat_in # 41.24006000
    
    # set date_time of eclipse (ideally before solar eclipse begins)
    
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # do the time conversions
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
                                                    geopos    = c(#x = lon_in, 
                                                      x = ifelse(is.na(lon_in) | is.null(lon_in), 
                                                                 -81.44067000, lon_in),
                                                      #y = lat_in, 
                                                      y = ifelse(is.na(lat_in) | is.null(lat_in), 
                                                                 41.24006000, lat_in),
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
    
    # out_attributes
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
      # out.times$duration_minutes <- scales::comma(c(0,
      #                                               diff(ymd_hms(out.times$local_time))),0.1)
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
  # /ADDED TO REPLACE BELOW----
  
  # # TO COMMENT OUT ----
  # output$eclipse_info <- renderTable({
  #   # ECLIPSE MATH
  #   lon_in <- input$lon_in #-81.44067000
  #   lat_in <- input$lat_in # 41.24006000
  #   
  #   # set date_time of eclipse (ideally before solar eclipse begins)
  #   
  #   greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
  #   tz.local      <- tz(greg_dt.local)
  #   
  #   # do the time conversions
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
  #                                                   geopos    = c(#x = lon_in, 
  #                                                                 x = ifelse(is.na(lon_in) | is.null(lon_in), 
  #                                                                            -81.44067000, lon_in),
  #                                                                 #y = lat_in, 
  #                                                                 y = ifelse(is.na(lat_in) | is.null(lat_in), 
  #                                                                            41.24006000, lat_in),
  #                                                                 z = 10), 
  #                                                   backward = F)
  #   
  #   ewl_out$tret <- ewl_out$tret[1:5]
  #   ewl_out$attr <- ewl_out$attr[1+c(2)]
  #   
  #   # out_times
  #   out.times <- data.frame(time_val.jul     = ewl_out$tret, 
  #                           local_time       = NA)
  #   
  #   for(i in 1:nrow(out.times)){
  #     out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
  #                                                        gregflag = 1) %>%
  #       paste(., sep = "-", collapse = "-") %>%
  #       ymd_hms() %>%
  #       with_tz(., tz.local) %>%
  #       strftime(., 
  #                format = "%m-%d-%y %I:%M:%S%p %Z", 
  #                #format = "%I:%M:%S%p %Z",
  #                tz = tz.local) %>%
  #       as.character() #%>%
  #       #tolower() %>%
  #       # gsub("^0", "", .) %>%
  #       # gsub("AM ", "am ", .) %>%
  #       # gsub("PM ", "pm ", .)
  #   }
  #   
  #   out.times <- out.times[order(out.times$local_time),]
  #   out.times$eclipse_event <- c("begin_partial_eclipse", 
  #                                "begin_total_eclipse", 
  #                                "maximum_total_eclipse", 
  #                                "end_total_eclipse", 
  #                                "end_partial_eclipse")
  #   rownames(out.times) <- 1:nrow(out.times)
  #   out.times <- out.times[,c("eclipse_event", "local_time")]
  #   
  #   # out_attributes
  #   out.attr <- data.frame(longitude = lon_in, 
  #                          latitude  = lat_in,
  #                          total_ecl_at_loc = ewl_out$attr > 1)
  #   rownames(out.attr) <- 1:nrow(out.attr)
  #   out.attr
  #   # out.times$lon <- out.attr$longitude
  #   # out.times$lat <- out.attr$latitude
  #   # out.times$t_ecl <- out.attr$total_ecl_at_loc
  #   
  #   if(!out.attr$total_ecl_at_loc){
  #     #out.times$local_time[2:4] <- NA
  #     out.times$local_time <- "partial eclipse only"
  #   }else{
  #     out.times$duration_minutes <- scales::comma(c(0,
  #                                  diff(ymd_hms(out.times$local_time))),0.1)
  #     out.times$local_time <- out.times$local_time %>%
  #       gsub("^.*-\\d{2,2} ", "", .) %>%
  #       gsub("^0", "", .) %>%
  #       gsub("AM ", "am ", .) %>%
  #       gsub("PM ", "pm ", .)
  #   }
  #   
  #   out.times
  # })
  ## /TO COMMENT OUT ----
  
  
  # # get totality duration----
  # output$totality_dur <- renderText({
  #   # ECLIPSE MATH----
  #   
  #   lon_in <- input$lon_in #-81.44067000
  #   lat_in <- input$lat_in # 41.24006000
  #   
  #   # set date_time of eclipse (ideally before solar eclipse begins)----
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
  #                                                   backward = F)$attr[3]
  #   scales::percent(ewl_out,0.00000001)
  #   
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
