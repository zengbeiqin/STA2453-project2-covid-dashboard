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


df_summary <- read_csv('data/covidtesting.csv')
vac_df_summary<-read_csv('data/vaccines_by_age.csv')
df_summary$dailydeath=  c(NA,diff(df_summary$Deaths))
df_summary$dailyalpha=c(NA,diff(df_summary$Total_Lineage_B.1.1.7_Alpha))
df_summary$dailybeta=c(NA,diff(df_summary$Total_Lineage_B.1.351_Beta))
df_summary$dailygamma=c(NA,diff(df_summary$Total_Lineage_P.1_Gamma))
df_summary$dailydelta=c(NA,diff(df_summary$Total_Lineage_B.1.617.2_Delta))
print(colnames(df_summary))
print(min(df_summary$ReportedDate))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          dateRangeInput(inputId = 'date_range_sum',
                         label = "Select date range",
                         start = min(df_summary$ReportedDate),
                         end = max(df_summary$ReportedDate)),
          dateInput(inputId='show_date_sum',
                    label="select the date to show",
                    value='2022-01-01',
                    min='2021-11-11',
                    max='2022-03-11')
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("dailycomfirmed"),
           plotlyOutput("totalcase"),
           plotlyOutput("totaldeath"),
           plotlyOutput("dailydeath"),
           plotlyOutput("covidtype"),
           plotlyOutput("vac")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$dailycomfirmed <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=ConfirmedPositive))
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="确定是阳性/每日")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$totalcase <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=TotalCases))
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="确诊总案例")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$totaldeath <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=Deaths))
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="死亡总数")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$dailydeath <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=dailydeath))
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="每日死亡")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    output$covidtype <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= '2021-01-01',
               ReportedDate <= '2021-12-31')
      our_plot<-ggplot(filter_df_summary)+
        geom_line(aes(x=ReportedDate, y=dailyalpha,color="alpha"))+
        geom_line(aes(x=ReportedDate, y=dailybeta,color="beta"))+
        geom_line(aes(x=ReportedDate, y=dailygamma,color='gamma'))+
        geom_line(aes(x=ReportedDate, y=dailydelta,color='delta'))
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="type")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$vac<- renderPlotly({
      #choose the data from right time
      
      filter_vac <- vac_df_summary %>% 
        filter(Date == input$show_date_sum,
               Agegroup!="Adults_18plus"&Agegroup!="Ontario_12plus"&Agegroup!="Undisclosed_or_missing"
        )
      
      
      our_plot<-ggplot(filter_vac)+
        geom_bar(stat = 'identity',aes(x=Agegroup, y=Percent_fully_vaccinated,fill=Agegroup))
      
      
      our_plot<-our_plot+
        labs(x="时间", y="数量", title="住院人数比例随时间变化")+
        theme(legend.position = "bottom") 
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
