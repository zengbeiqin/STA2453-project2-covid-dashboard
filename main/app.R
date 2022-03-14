#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      data <- data.frame(
        category=c("A", "B", "C"),
        count=c(10, 60, 30)
      )
      print(data)
      # Compute percentages
      data$fraction = data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax = cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin = c(0, head(data$ymax, n=-1))
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
        xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
