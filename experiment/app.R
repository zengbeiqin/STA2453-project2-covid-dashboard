#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)
#load the data--------------------
df <- read_csv('data/icu.csv')
# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = bs_theme(bootswatch = "minty"),
    # Application title
    titlePanel("hospital and icu"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "district",
                        label = "select district", 
                        selected = "TORONTO", choices = c("TORONTO", "CENTRAL","EAST","WEST","NORTH")),
            checkboxInput(inputId = "vented",
                          label = "On ventilators", value = F),
            dateRangeInput(inputId = 'date_range',
                           label = "Select date range",
                           start = min(df$date),
                           end = max(df$date)),
            actionButton(inputId = "show", 
                         label = "Show Instructions")
        ),

        # Show a plot of the generated distribution
        mainPanel(plotlyOutput('distPlot'),
                  plotlyOutput('daily_outcomes'))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      
      filter_data <- df %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               oh_region==input$district)
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
      

        # draw the histogram with the specified number of bins
        #our_plot<-hist( as.numeric(x), breaks = bins, col = 'darkgray', border = 'white')
        
        our_plot<-ggplot(filter_data, aes(x=icu_current_covid)) +
          geom_bar()
        our_plotly_plot <- ggplotly(our_plot)
        
    })
    
    output$daily_outcomes<- renderPlotly({
      
      filter_data <- df %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               oh_region==input$district)
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      our_plot<-ggplot(filter_data, aes(x=date, y=icu_current_covid)) +
        geom_line() + 
        xlab("")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    observeEvent(input$show, {
      
      print(input$date_range)
      
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
