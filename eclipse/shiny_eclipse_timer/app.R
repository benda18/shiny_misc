#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# live link: https://tim-bender.shinyapps.io/shiny_eclipse_planner/


#library(renv)
library(swephR)
library(lubridate)
#library(dplyr)
#library(tigris)
library(shiny)
library(censusxy)
library(scales)
#library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("April 8th, 2024 Eclipse Planning Tool -
             Find out when and if a specific location will see totality."),
  sidebarLayout(
    sidebarPanel(
      shiny::textInput(inputId = "addr_in", 
                       label = "Enter Address", 
                       value = sample(c("107 Cliff Park Rd, Springfield, OH 45504", 
                                        "1060 W Addison St, Chicago, IL 60613",
                                        "1600 Pennsylvania Avenue, Washington DC",
                                        "1301 Western Ave, Cincinnati OH", 
                                        "8525 Garland Rd, Dallas, TX 75218", 
                                        "1116 W Troy Ave, Indianapolis, IN 46225", 
                                        # "1490 Bethel Rd, Columbus, OH 43220", 
                                        # "4093 Trueman Blvd, Hilliard, OH 43026", 
                                        "130 N Main St, Hudson, OH 44236"),1)),
      actionButton(inputId = "cxy_go", 
                   label   = "SEARCH ADDRESS"), 
      wellPanel(
        fluidRow(
          uiOutput("tab")
        )
      ),
      # wellPanel(
      #   fluidRow("See Eclipse Info Below:"),
      #   fluidRow(shiny::tableOutput(outputId = "return_eclips.times"))
      # ), 
      wellPanel(
        fluidRow(" "),
        fluidRow("ACKNOWLEDGEMENTS"),
        wellPanel(
          fluidRow("The geocoding utility relies on a library develped by and described in a 2021 paper in \"Transactions in GIS\" by Prener and Fox, and uses the US Census Bureau's Geocoder API")
        ),
        fluidRow("LINKS"),
        wellPanel(
          fluidRow(uiOutput("tab.res")),
          fluidRow(uiOutput("tab.api")),
          fluidRow(uiOutput("tab.cxy")),
          fluidRow( uiOutput("tab.src"))
        )
      )
    ),
    mainPanel(
      wellPanel(
        fluidRow("See Eclipse Info Below:"),
        fluidRow(shiny::tableOutput(outputId = "return_eclips.times"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  url <- a("link to interactive map from National Solar Observatory", 
           href="https://nso.edu/for-public/eclipse-map-2024/", 
           target="_blank")
  output$tab <- renderUI({
    tagList("See Also:", url)
  })
  
  url.source <- a("Source Code on GitHub", 
                  href = "https://github.com/benda18/shiny_misc/blob/main/eclipse/shiny_eclipse_timer/app.R", 
                  target = "_blank")
  output$tab.src <- renderUI({
    tagList(url.source)
  })
  
  url.cxy <- a("censusxy Library for R on Github", 
               href = "https://github.com/chris-prener/censusxy", 
               target = "_blank")
  output$tab.cxy <- renderUI({
    tagList(url.cxy)
  })
  
  url.api <- a("US Census Bureau's Geocoder API", 
               href = "https://geocoding.geo.census.gov/geocoder/", 
               target = "_blank")
  output$tab.api <- renderUI({
    tagList(url.api)
  })
  
  url.res <- a("Creating open source composite geocoders: Pitfalls and opportunities (Prener & Fox)", 
               href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/tgis.12741", 
               target = "_blank")
  output$tab.res <- renderUI({
    tagList(url.res)
  })
  
  # get eclipse times----
  get_times <- eventReactive(eventExpr = input$cxy_go, {
    temp          <- censusxy::cxy_oneline(address = input$addr_in)
    lon_in        <- temp$coordinates.x
    lat_in        <- temp$coordinates.y
    greg_dt.local <- ymd_hm("2024-04-07 08:30AM", tz = "America/New_York")
    tz.local      <- tz(greg_dt.local)
    
    # convert to utc
    greg_dt.utc   <- with_tz(greg_dt.local, tz = "UTC")
    jul_dt.utc    <- swephR::swe_julday(year  = year(greg_dt.utc), 
                                        month = lubridate::month(greg_dt.utc, label = F), 
                                        day   = mday(greg_dt.utc), 
                                        hourd = hour(greg_dt.utc) + 
                                          (minute(greg_dt.utc)/60) + 
                                          (second(greg_dt.utc)/60/60), 
                                        gregflag = 1)
    
    # do eclipse math
    ewl_out     <- swephR::swe_sol_eclipse_when_loc(jd_start  = jul_dt.utc, 
                                                    ephe_flag = 4, 
                                                    geopos    = c(x = lon_in,
                                                                  y = lat_in,
                                                                  z = 10), 
                                                    backward = F)
    # extract needed values
    ewl_out$tret <- ewl_out$tret[1:5]
    ewl_out$attr <- ewl_out$attr[1+c(2)]
    
    # build output table to be displayed on shiny app
    out.times <- data.frame(time_val.jul     = ewl_out$tret, 
                            local_time       = NA)
    
    # convert julian times to gregorian
    for(i in 1:nrow(out.times)){
      out.times$local_time[i] <- swephR::swe_jdet_to_utc(jd_et = ewl_out$tret[i], 
                                                         gregflag = 1) |>
        paste(sep = "-", collapse = "-") |>
        ymd_hms() |>
        with_tz(tzone = tz.local) |>
        strftime(format = "%m-%d-%y %I:%M:%S%p %Z", 
                 tz = tz.local) |>
        as.character() 
    }
    
    out.times <- out.times[order(out.times$local_time),]
    
    # times labels
    out.times$eclipse_event <- c("start", 
                                 "start_totality", 
                                 "max_eclipse", 
                                 "end_totality", 
                                 "end")
    rownames(out.times) <- 1:nrow(out.times)
    
    # filter down to needed columns only
    out.times <- out.times[,c("eclipse_event", "local_time")]
    
    # out_attributes
    out.attr <- data.frame(longitude = lon_in, 
                           latitude  = lat_in,
                           total_ecl_at_loc = ewl_out$attr > 1)
    rownames(out.attr) <- 1:nrow(out.attr)
    out.attr
    
    
    
    # total vs partial eclipse
    if(!out.attr$total_ecl_at_loc){
      out.times$local_time[1:5] <- gsub("^.*-\\d{2,2} ", "", out.times$local_time[1:5])
      out.times$local_time[1:5] <- gsub("^0", "", out.times$local_time[1:5])
      out.times$local_time[1:5] <- gsub("AM ", "am ", out.times$local_time[1:5])
      out.times$local_time[1:5] <- gsub("PM ", "pm ", out.times$local_time[1:5])
      
      # manual fixes because when partial eclipse the order is different
      out.times$local_time[1]  #  no change needed
      out.times$local_time[5] <- out.times$local_time[3] 
      out.times$local_time[3] <- out.times$local_time[2]
      out.times$local_time[2] <- "<<<partial eclipse only>>>" #  not seen
      out.times$local_time[4] <- "<<<partial eclipse only>>>" #  not seen 
      
      out.times <- out.times[c(1,3,5),]
      out.times$eclipse_type <- c("Partial")
    }else{
      out.times$local_time <- gsub("^.*-\\d{2,2} ", "", out.times$local_time)
      out.times$local_time <- gsub("^0", "", out.times$local_time)
      out.times$local_time <- gsub("AM ", "am ", out.times$local_time)
      out.times$local_time <- gsub("PM ", "pm ", out.times$local_time)
      out.times$eclipse_type <- c("Total")
    }
    
    out.times$max_sun_obscured <- scales::percent(ifelse(ewl_out$attr > 1, 1, ewl_out$attr), 
                                                  accuracy = 0.1)
    if(ewl_out$attr < 1 & 
       grepl("^100", out.times$max_sun_obscured[median(1:nrow(out.times))])){
      out.times$max_sun_obscured <- "99.9%"
    }
    out.times$max_sun_obscured[c(1:nrow(out.times) != median(1:nrow(out.times)))] <- NA
    out.times
    
  })
  
  output$return_eclips.times <- renderTable({
    get_times()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
