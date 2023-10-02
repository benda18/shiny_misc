#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Collatz Conjecture Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          inputPanel(
            fluidRow(
              column(12,h4(HTML(r'(<b><i>Input Starting Values:</b></i>)'))),
            ),
            wellPanel(
              numericInput(inputId = "input_n", 
                      label   = "n (#1)", 
                      value   = 4),
              numericInput(inputId = "input_n2", 
                      label   = "n (#2)", 
                      value   = 14)
            ),
            ),
          wellPanel(
            radioButtons(inputId = "radio_facet", 
                         label = h4(HTML(r'(<b><i>Show on Single Chart?:</b></i><br>)')),
                         choices = list("Single Chart" = "single", 
                                        "Two Charts" = "two"), 
                         selected = "single"),
            radioButtons(inputId = "radio_y.scale", 
                         label = h4(HTML(r'(<b><i>How to Display Y-Scale if Multiple Charts:</b></i><br>)')),
                         choices = list("Identical Y-Scales" = "fixed", 
                                        "Free Y-Scales" = "free_y"), 
                         selected = "fixed")
            
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            h3(HTML(r'(<b>[[Plot Title]]</b><br>)'))
          ),
           plotOutput("collatz_plot"),
          fluidRow(
            column(6,
                   h3(HTML(r'(<br><b>[[Table Title]]</b><br>)')),
                   tableOutput('table01')
            ),
            column(6,
                   h3(HTML(r'(<br><b>[[Table Title]]</b><br>)')),
                   tableOutput('table02')
            )
          ),
           
        )
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  collatz_seq <- function(x, type.a.divby = 2, 
                          type.b.multiply = 3, 
                          type.b.plus = 1, 
                          max.loops = 10000){
    out <- as.numeric(x)
    n.stop <- 0
    while(n.stop < max.loops){
      n.stop <- n.stop + 1
      # determine type
      if( (x/type.a.divby) == floor(x/type.a.divby) ){
        # if type.a
        x <- x / type.a.divby
      }else{
        # if type.b
        x <- (x * type.b.multiply) + type.b.plus
        #x <- abs((x*type.b.multiply) + type.b.plus)
      }
      out <- c(out, x)
      if(x == 1){
        break
      }
      if(n.stop == max.loops){
        print( "max loops reached")
      }
      #print(x)
      #Sys.sleep(0.1)
    }
    return(out)
  }
  
  output$radio_facet <- renderPrint({ input$radio_facet })
 
  output$table01 <- renderTable({
    v.out1 <- collatz_seq(x = input$input_n)
    v.out2 <- collatz_seq(x = input$input_n2)
    
    df.out1 <- rbind(data.frame(n_in = input$input_n, 
                                n_out = v.out1, 
                                c_seq = 1:length(v.out1)), 
                     data.frame(n_in = input$input_n2, 
                                n_out = v.out2, 
                                c_seq = 1:length(v.out2))) %>%
      group_by(n_in) %>%
      summarise(stopping_time = scales::comma(as.integer(n())), 
                max_n_out = scales::comma(as.integer(max(n_out))))
    
  })
  
  output$table02 <- renderTable({
    v.out2 <- collatz_seq(x = input$input_n2)
    
    df.out2 <- data.frame(n_in = input$input_n2, 
                          n_out = v.out2, 
                          c_seq = 1:length(v.out2))
    
  })
  
  
  output$collatz_plot <- renderPlot({
    # function
    c.out1.v  <- collatz_seq(x = input$input_n)
    c.out1.df <- data.frame(n = input$input_n,
                            cum_min = cummin(c.out1.v),
                            cum_max = cummax(c.out1.v),
                            cum_avg = cummean(c.out1.v),
                            x = 1:length(c.out1.v), 
                            y = c.out1.v)
    
    c.out2.v  <- collatz_seq(x = input$input_n2)
    c.out2.df <- data.frame(n = input$input_n2,
                            cum_min = cummin(c.out2.v),
                            cum_max = cummax(c.out2.v),
                            cum_avg = cummean(c.out2.v),
                            x = 1:length(c.out2.v), 
                            y = c.out2.v)
    
    plot.out <- ggplot() + 
      geom_rug(data = c.out1.df, 
               aes(x = x, y = y, color = factor(n)), 
               sides = "l") +
      geom_rug(data = c.out2.df, 
               aes(x = x, y = y, color = factor(n)), 
               sides = "l") +
      # geom_ribbon(data = c.out1.df, 
      #             alpha = 0.2,
      #             aes(x = x, 
      #                 ymin = cum_min, ymax = cum_max, 
      #                 fill = factor(n)))+
      # geom_ribbon(data = c.out2.df, 
      #             alpha = 0.2,
      #             aes(x = x, 
      #                 ymin = cum_min, ymax = cum_max, 
      #                 fill = factor(n)))+
      # geom_line(data = c.out1.df, linetype = 2232,
      #           aes(x = x, y = cum_avg, color = factor(n)))+
      # geom_line(data = c.out2.df, linetype = 2232,
      #           aes(x = x, y = cum_avg, color = factor(n)))+
      geom_line(data = c.out1.df, 
                aes(x = x, y = y, color = factor(n)))+
      geom_line(data = c.out2.df, 
                aes(x = x, y = y, color = factor(n)))+
      # geom_point(data = c.out1.df, size = 2,
      #           aes(x = x, y = y, color = factor(n)))+
      # geom_point(data = c.out2.df, size = 2,
      #           aes(x = x, y = y, color = factor(n)))+
      scale_x_continuous(name = "stopping time (steps)", 
                         labels = scales::comma, limits = c(1,NA))+
      scale_y_continuous(name = "value (n)", 
                         labels = scales::comma, limits = c(0,NA))+
      scale_color_discrete(name = "Starting Value")+
      theme(text = element_text(size = 15), 
            #axis.text.x = element_blank(), 
            axis.ticks.x = element_blank())
    
    if(input$radio_facet != "single"){
      plot.out <- plot.out + 
        facet_grid(n~., scales = input$radio_y.scale)  # fixed vs free vs free_(x/y)
    }
      
    print(plot.out)
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
