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
library(reshape2)
library(ggpubr)
#load the data--------------------
df <- read_csv('data/icu.csv')
df_hosp_breakdown<-read_csv('data/hosp_icu_c19_breakdown.csv')
df_icu_beds<-read_csv('data/icu_beds.csv')
a<-1
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
            dateInput(inputId='show_date',
                      label="select the date to show",
                      value='2022-01-01',
                      min='2021-11-11',
                      max='2022-03-11'
                      ),
            actionButton(inputId = "show", 
                         label = "Show Instructions")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput('perc_adult'),
                  plotOutput('perc_child'),
                  plotlyOutput('distPlot'),
                  plotlyOutput('daily_outcomes'),
                  plotlyOutput('breakdown'),
                  plotlyOutput('icu_breakdown'),
                  plotlyOutput('adult_beds'),
                  plotlyOutput('child_beds')
                  
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$perc_adult<- renderPlot({
    #choose the data from right time
    
    filter_data_icu_beds <- df_icu_beds %>% 
      filter(date == input$show_date
      )
    
    # draw the histogram with the specified number of bins
    adult_icu<- data.frame(melt(filter_data_icu_beds[,c(4,2,3)],variable.name="type",value.name="pop"))
    adult_icu$fraction=adult_icu$pop/sum(adult_icu$pop)
    adult_icu$ymax=cumsum(adult_icu$fraction)
    adult_icu$ymin=c(0,head(adult_icu$ymax,n=-1))
    
    # Make the plot
    ggplot(adult_icu, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
      geom_rect() +
      coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
      xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
  
  })
  
  output$perc_child<- renderPlot({
    #choose the data from right time
    
    filter_data_icu_beds <- df_icu_beds %>% 
      filter(date == input$show_date
      )
    
    # draw the histogram with the specified number of bins
    child_icu<- data.frame(melt(filter_data_icu_beds[,c(9,7,8)],variable.name="type",value.name="pop"))
    child_icu$fraction=child_icu$pop/sum(child_icu$pop)
    child_icu$ymax=cumsum(child_icu$fraction)
    child_icu$ymin=c(0,head(child_icu$ymax,n=-1))
    
    # Make the plot
    ggplot(child_icu, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
      geom_rect() +
      coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
      xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
    
  })

    output$distPlot <- renderPlotly({
      
      filter_data <- df %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               oh_region==input$district)
      filter_data_hosp_breakdown <- df_hosp_breakdown %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
      

        # draw the histogram with the specified number of bins
        #our_plot<-hist( as.numeric(x), breaks = bins, col = 'darkgray', border = 'white')
        
        our_plot<-ggplot(filter_data, aes(x=icu_current_covid)) +
          geom_bar()
        our_plotly_plot <- ggplotly(our_plot)
        return(our_plotly_plot)
        
    })
    
    output$daily_outcomes<- renderPlotly({
      #choose the data from right time
      filter_data <- df %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               oh_region==input$district)
      filter_data_hosp_breakdown <- df_hosp_breakdown %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               )
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
               )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      
      
      our_plot<-ggplot(filter_data)+
        geom_line(aes(x=date, y=icu_crci_total,colour='total icu of crci')) +
        geom_line( aes(x=date, y=icu_crci_total_vented, colour='total vented icu of crci'))+
        scale_colour_manual("",values = c("total icu of crci" = "red","total vented icu of crci" = "green"))
      
      
      our_plot<-our_plot+
        labs(x="时间", y="故障数", title="时间序列预测图")+
        theme(legend.position = "bottom") 
     
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$breakdown<- renderPlotly({
      #choose the data from right time
      
      filter_data_hosp_breakdown <- df_hosp_breakdown %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      hosp_breakdown<- melt(filter_data_hosp_breakdown[,c(1,2,3)],id.vars="date",variable.name="type",value.name="pop")
      
      our_plot<-ggplot(hosp_breakdown)+
        geom_bar(stat = 'identity',aes(x=date, y=pop,fill =type),position='stack')
      
      
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="住院人数比例随时间变化")+
        theme(legend.position = "bottom") 
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$icu_breakdown<- renderPlotly({
      #choose the data from right time
      
      filter_data_hosp_breakdown <- df_hosp_breakdown %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      hosp_breakdown<- melt(filter_data_hosp_breakdown[,c(1,4,5)],id.vars="date",variable.name="type",value.name="pop")
      
      our_plot<-ggplot(hosp_breakdown)+
        #geom_bar(stat = 'identity',aes(x=date, y=hosp_for_covid,colour='red'),position='stack') +
        #geom_bar(stat = 'identity',aes(x=date, y=hosp_other_conditions,colour='blue'),position='stack')
        geom_bar(stat = 'identity',aes(x=date, y=pop,fill =type),position='stack')
      
      
      our_plot<-our_plot+
        labs(x="时间", y="比例", title="icu人数比例随时间变化")+
        theme(legend.position = "bottom") 
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$adult_beds<- renderPlotly({
      #choose the data from right time
      
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      
      
      hosp_breakdown<- melt(filter_data_icu_beds[,c(1,4,2,3)],id.vars="date",variable.name="type",value.name="pop")
      
      our_plot<-ggplot(hosp_breakdown)+
        #geom_bar(stat = 'identity',aes(x=date, y=hosp_for_covid,colour='red'),position='stack') +
        #geom_bar(stat = 'identity',aes(x=date, y=hosp_other_conditions,colour='blue'),position='stack')
        geom_bar(stat = 'identity',aes(x=date, y=pop,fill =type),position='stack')
      
      
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="成人床位情况")
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$child_beds<- renderPlotly({
      #choose the data from right time
      
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= input$date_range[1],
               date <= input$date_range[2],
        )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      
      
      hosp_breakdown<- melt(filter_data_icu_beds[,c(1,9,7,8)],id.vars="date",variable.name="type",value.name="pop")
      
      our_plot<-ggplot(hosp_breakdown)+
        geom_bar(stat = 'identity',aes(x=date, y=pop,fill =type),position='stack')
        
      
      
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="幼儿床位情况")
      
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
