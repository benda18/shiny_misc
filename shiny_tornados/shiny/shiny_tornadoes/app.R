library(shiny)
library(dplyr)
#library(readr)
library(lubridate)
#library(tigris)
library(ggplot2)



rm(list=ls());cat('\f');gc()
load("tornado.RData")
# cw_co_fips <- cw_co_fips %>% as_tibble() %>%
#   mutate(., 
#          COUNTYFP = as.numeric(COUNTYFP))

actualT <- left_join(actualT, cw_magnitude) %>% as_tibble()
rm(cw_magnitude)

# TODO - need to somehow join FIPS code for county to actualT and shapefile so
# we can map select filter

actualT_segs <- actualT %>%
  .[.$slon != 0 & .$elon != 0,] %>%
  mutate(., uid_om.yr.st = paste(om,yr,st, sep = "-")) 
actualT_pts <- actualT %>%
  .[.$slon == 0 | .$elon == 0,] %>%
  mutate(., uid_om.yr.st = paste(om,yr,st, sep = "-"))


# vars----
var.pointsize <- 0.1
var.pointcolor <- "black"


# UI----
ui <- fluidPage(
  
  # Application title
  titlePanel("Tornadoes of the Lower-48 States"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # FILTERS----
      shiny::headerPanel("User Selections:"),
      # Select State(s)
      fluidRow(column(12),
               shiny::selectInput(inputId = "sel_states", 
                                  label = "Select State(s)", 
                                  choices = reg_st_list,
                                  selected = c("OH", "IN", "KY"),
                                  multiple = T)),
      # Select Counties
      fluidRow(column(12),
               shiny::selectInput(inputId = "sel_counties", 
                                  label = "Select Count(y/ies)", 
                                  choices = 'N/A',
                                  #selected = c("OH", "IN", "KY"),
                                  multiple = T)),
      # Filter years
      fluidRow(column(12,
                      shiny::sliderInput(inputId = "sel_year", 
                                            label   = "Select Year(s)", 
                                            value   = c(min(actualT$yr),max(actualT$yr)), 
                                            min     = min(actualT$yr), 
                                            max     = max(actualT$yr), 
                                            sep = "")
                      )),
      # Filter Magnitude
      fluidRow(column(12), 
               shiny::sliderInput(inputId = "sel_magnitude", 
                                  label   = "Select Magnitude Range", 
                                  value   = c(min(actualT$def),max(actualT$def)), 
                                  min     = min(actualT$def), 
                                  max     = max(actualT$def), 
                                  sep = "", 
                                  ticks = T))
    ),
    mainPanel(
      plotOutput("plot01")
    )
  )
)

# SERVER----
server <- function(input, output, session) {
  
  observe({
    
    # STATES
    # observe what states are selected
    obs_sel.states <- input$sel_states
    
    # if only one state is selected
    if(length(obs_sel.states) == 1){
      # get the list of counties
      #show.counties <- cw_co_fips[cw_co_fips$STUSPS == obs_sel.states,]$COUNTY
      show.counties.choices <- tor.st_co_yr[tor.st_co_yr$st == obs_sel.states,]$COUNTY
    }else{
      show.counties.choices <- 'N/A'
    }
    
    updateSelectInput(session = session, 
                      inputId = "sel_counties", 
                      label   = "Select Count(y/ies)", 
                      choices = show.counties.choices, 
                      selected = first(show.counties.choices,4)) #show.counties.choices)
    
  })
  
  output$plot01 <- shiny::renderPlot({
    
    var_sel.uids   <- tor.st_co_yr[tor.st_co_yr$st %in% input$sel_states & 
                                     tor.st_co_yr$COUNTY %in% input$sel_counties,]$uid_om.yr.st %>%
      unique()
    
    
    a.plot <- ggplot() + 
      theme(axis.ticks = element_blank(), 
            axis.text = element_blank(), 
            plot.background = element_blank(),  # image bg
            panel.background = element_blank(), # map bg
            axis.line = element_blank(), 
            panel.grid = element_blank())+
      scale_x_continuous(name = "") +
      scale_y_continuous(name = "")
                       
    
    if(length(input$sel_states) == 1){
      a.plot <- a.plot +
        geom_sf(data = tigris_co_geo[tigris_co_geo$STUSPS %in% input$sel_states & 
                                       tigris_co_geo$NAME %in% input$sel_counties,],
                fill = "grey", color = "white")+
        geom_sf_text(data = tigris_co_geo[tigris_co_geo$STUSPS %in% input$sel_states & 
                                            tigris_co_geo$NAME %in% input$sel_counties,], 
                     aes(label = NAME), size = 2, color = "white")+
        geom_segment(data = actualT_segs[actualT_segs$uid_om.yr.st %in% var_sel.uids & 
                                           between(x = actualT_segs$yr, 
                                                   min(input$sel_year), 
                                                   max(input$sel_year)),],
                     aes(x = slon, xend = elon, y = slat,  yend = elat)) +
        geom_point(data = actualT_pts[actualT_pts$uid_om.yr.st %in% var_sel.uids & 
                                        between(x = actualT_pts$yr,
                                                min(input$sel_year),
                                                max(input$sel_year))&
                                        actualT_pts$slon != 0 & 
                                        actualT_pts$slat != 0,],
                   aes(x = slon, 
                       y = slat), 
                   size = var.pointsize, 
                   color = var.pointcolor)+
        geom_point(data = actualT_pts[actualT_pts$uid_om.yr.st %in% var_sel.uids & 
                                        between(x = actualT_pts$yr, 
                                                min(input$sel_year), 
                                                max(input$sel_year))& 
                                        actualT_pts$elon != 0 & 
                                        actualT_pts$elat != 0,],
                   aes(x = elon, 
                       y = elat), 
                   size = var.pointsize, 
                   color = var.pointcolor)
    }else{
      a.plot <- a.plot + 
        geom_sf(data = tigris_st_geo[tigris_st_geo$STUSPS %in% input$sel_states,], 
                color = "white", fill = "grey", linewidth = 1)+
        geom_sf_label(data = tigris_st_geo[tigris_st_geo$STUSPS %in% input$sel_states,], 
                     aes(label = STUSPS), size = 4, color = "darkblue")+
        geom_segment(data = actualT_segs[actualT_segs$st %in% input$sel_states & 
                                           between(x = actualT_segs$yr, 
                                                   min(input$sel_year), 
                                                   max(input$sel_year)),], 
                     aes(x = slon, xend = elon, y = slat,  yend = elat)) +
        geom_point(data = actualT_pts[actualT_pts$st %in% input$sel_states & 
                                        between(x = actualT_pts$yr, 
                                                min(input$sel_year), 
                                                max(input$sel_year))& 
                                        actualT_pts$slon != 0 & 
                                        actualT_pts$slat != 0,],
                   aes(x = slon, 
                       y = slat),
                   size = var.pointsize, 
                   color = var.pointcolor)+
        geom_point(data = actualT_pts[actualT_pts$st %in% input$sel_states & 
                                        between(x = actualT_pts$yr, 
                                                min(input$sel_year), 
                                                max(input$sel_year))& 
                                        actualT_pts$elon != 0 & 
                                        actualT_pts$elat != 0,],
                   aes(x = elon, 
                       y = elat), 
                   size = var.pointsize, 
                   color = var.pointcolor)
        
    }
    
    a.plot
    
  })
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
