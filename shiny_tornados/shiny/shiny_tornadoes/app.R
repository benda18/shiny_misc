library(shiny)
library(dplyr)
#library(readr)
library(lubridate)
#library(tigris)
library(ggplot2)



rm(list=ls());cat('\f');gc()
load("tornado.RData")
st_co_df <- st_co_df %>% as_tibble() %>%
  mutate(., 
         COUNTYFP = as.numeric(COUNTYFP))

actualT <- left_join(actualT, cw_magnitude) %>% as_tibble()

left_join(actualT, 
          st_co_df, 
          by = c("f1" = "COUNTYFP")) %>%
  left_join(., 
            st_co_df, 
            by = c("f2" = "COUNTYFP"))

# TODO - need to somehow join FIPS code for county to actualT and shapefile so we can map select filter


actualT_segs <- left_join(actualT, cw_magnitude) %>%
  .[.$slon != 0 & .$elon != 0,]
actualT_pts <- left_join(actualT, cw_magnitude)
rm(cw_magnitude)

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
    # observe what states are selected
    obs_sel.states <- input$sel_states
    
    # if only one state is selected
    if(length(obs_sel.states) == 1){
      show.counties <- st_co_df[st_co_df$STUSPS == obs_sel.states,]$COUNTY
    }else{
      show.counties <- 'N/A'
    }
    
    updateSelectInput(session = session, 
                      inputId = "sel_counties", 
                      label   = "Select Count(y/ies)", 
                      choices = show.counties, 
                      selected = show.counties)
    
  })
  
  output$plot01 <- shiny::renderPlot({
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
        geom_segment(data = actualT_segs[actualT_segs$st %in% input$sel_states & 
                                           
                                           between(x = actualT_segs$yr, 
                                                   min(input$sel_year), 
                                                   max(input$sel_year)),], 
                     aes(x = slon, xend = elon, y = slat,  yend = elat))
    }else{
      a.plot <- a.plot + 
        geom_sf(data = tigris_st_geo[tigris_st_geo$STUSPS %in% input$sel_states,], 
                color = "white", fill = "grey", linewidth = 1)+
        geom_sf_text(data = tigris_st_geo[tigris_st_geo$STUSPS %in% input$sel_states,], 
                     aes(label = STUSPS), size = 4, color = "darkblue")+
        geom_segment(data = actualT_segs[actualT_segs$st %in% input$sel_states & 
                                           between(x = actualT_segs$yr, 
                                                   min(input$sel_year), 
                                                   max(input$sel_year)),], 
                     aes(x = slon, xend = elon, y = slat,  yend = elat))
        
    }
    
    a.plot
    
  })
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
