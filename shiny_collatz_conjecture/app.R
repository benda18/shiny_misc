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

source("C:/Users/bende/Documents/R/play/shiny_misc/shiny_collatz_conjecture/module.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Collatz Conjecture Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          column(12,h2(HTML(r'(<b><i>Collatz Conjecture:</b></i><br>)'))),
          column(12,h4(HTML(r'(y=3n+1 (if y is odd) | y=n/2 (if y is even)<br> )'))),
          inputPanel(
            fluidRow(
            column(12,h4(HTML(r'(<b><i>Input Starting Values:</b></i>)'))),
            ),
            # fluidRow(
            #   "foo"
            # ),
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
            
          ), 
          # column(12,h4(HTML(r'(<b><i>Build your own Conjecture:</b></i>)'))),
          # wellPanel(
          #   "foo"
          # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          wellPanel(
          fluidRow(
            h3(HTML(r'(<b>[[Plot Title]]</b><br>)'))
          ),
           plotOutput("collatz_plot"),
          fluidRow(
            column(12,
                   h3(HTML(r'(<br><b>[[Table Title]]</b><br>)')),
                   tableOutput('table01')
            )
            # column(6,
            #        h3(HTML(r'(<br><b>[[Table Title]]</b><br>)')),
            #        tableOutput('table02')
            # )
          ),
           
        )
    )
    ), # /wellPanel
    wellPanel(
      shiny::titlePanel("Test Your Own Conjecture"),
      fluidRow(column(12, h4(HTML(r'(Collatz Conjecture of y = 3n+1 can be rewritten as follows:<br>y = A*n+B | y = n/C)')))),
      
      fluidRow(column(2,
                      wellPanel(
                      fluidRow(column(12, h4(HTML(r'(Use the inputs below to test out other variables for A, B, & C to see how they perform and if you can find one where the input values always return to 1)')))),
        numericInput(inputId = "input_A", 
                     label   = "A", 
                     value   = 3),
        numericInput(inputId = "input_B", 
                     label   = "B", 
                     value   = 1),
        numericInput(inputId = "input_C", 
                     label   = "C", 
                     value   = 2) 
      )), 
      column(4, 
             wellPanel(
               h4(HTML(r'(<b>Your Current Conjecture Formula:</b>)')),
               shiny::textOutput(outputId = "built_conjecture")
               ),
             wellPanel(
               h4(HTML(r'(<b>Input New 'n' Values:</b>)')),
               numericInput(inputId = "input_n3", 
                            label   = "n (#3)", 
                            value   = 4),
               numericInput(inputId = "input_n4", 
                            label   = "n (#4)", 
                            value   = 14)
             ), 
             wellPanel(
               h4(HTML(r'(<b>Summary Table</b>)')),
               tableOutput("table03")
             )
             ),
      column(6, 
             #h4(HTML(r'(<b>Your Current Conjecture Formula:</b>)')),
             wellPanel(plotOutput("build_a_plot"))))
      #shiny::textOutput(outputId = "built_conjecture")
      # fluidRow(
      #   #column(5, 
      #          renderText({input$input_A})
      #   #)
      # )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  collatz_seq <- function(x, type.a.divby = 2, 
                          type.b.multiply = 3, 
                          type.b.plus = 1, 
                          max.loops = 1000){
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
  
  # build-a-conjecture vars
  output$var_A <- renderText({input$input_A})
  output$var_B <- renderText({input$input_B})
  output$var_C <- renderText({input$input_C})
  output$built_conjecture <- renderText({
    paste("y = ", 
          input$input_A, 
          " x + ", 
          input$input_B, 
          " (if odd); y = ",
          " x / ", 
          input$input_C, 
          " (if evem)")
  })
  
  output$build_a_plot <- renderPlot({
    
    df.bas3 <- data.frame(y = buildaseq(n=input$input_n3, 
                                        A=input$input_A,
                                        B=input$input_B,
                                        D=input$input_C, 
                                        #max.loops = 250, 
                                        print.formula = F), 
                          x = NA, 
                          grp = input$input_n3)
    df.bas3$x <- 1:nrow(df.bas3)
    
    df.bas4 <- data.frame(y = buildaseq(n=input$input_n4, 
                                        A=input$input_A,
                                        B=input$input_B,
                                        D=input$input_C, 
                                        #max.loops = 250, 
                                        print.formula = F), 
                          x = NA, 
                          grp = input$input_n4)
    df.bas4$x <- 1:nrow(df.bas4)
    
    df.bas34 <- rbind(df.bas3,df.bas4)
    
    ggplot() + 
      geom_line(data = df.bas34, 
                aes(x = x, y = y, color = factor(grp)))+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.background = element_rect(color = "black"), 
            text = element_text(size = 15))
  })
  
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
                max_n_out = scales::comma(as.integer(max(n_out))),
                end_at_1 = as.integer(last(n_out))==1) 
    
  })
  
  # output$table02 <- renderTable({
  #   v.out2 <- collatz_seq(x = input$input_n2)
  #   
  #   df.out2 <- data.frame(n_in = input$input_n2, 
  #                         n_out = v.out2, 
  #                         c_seq = 1:length(v.out2))
  #   
  # })
  
  output$table03 <- renderTable({
    v.out3 <- buildaseq(n=input$input_n3, 
                        A=input$input_A,
                        B=input$input_B,
                        D=input$input_C, 
                        #max.loops = 250, 
                        print.formula = F)
    v.out4 <- buildaseq(n=input$input_n4, 
                        A=input$input_A,
                        B=input$input_B,
                        D=input$input_C, 
                        #max.loops = 250, 
                        print.formula = F)
    
    df.out3 <- rbind(data.frame(n_in = input$input_n3, 
                                n_out = v.out3, 
                                c_seq = 1:length(v.out3)), 
                     data.frame(n_in = input$input_n4, 
                                n_out = v.out4, 
                                c_seq = 1:length(v.out4))) %>%
      group_by(n_in) %>%
      summarise(stopping_time = scales::comma(as.integer(n())), 
                max_n_out = scales::comma(as.integer(max(n_out))),
                end_at_1 = as.integer(last(n_out))==1) 
    
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
