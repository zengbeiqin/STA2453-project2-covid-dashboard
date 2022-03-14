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
library(plotly)
library(tidyverse)
library(DT)
library(bslib)

my_theme <- bs_theme(bootswatch = "cosmo")
theme_set(theme_classic())

#thematic_shiny(font = "auto")

data_cases <- read.csv("raw_data/covidtesting.csv") %>%
  mutate(Reported.Date = as.Date(Reported.Date)) 

data_vaccine <- read.csv("raw_data/vaccine_doses.csv") %>%
  mutate(report_date = as.Date(report_date))

data_vaccine_rate_latest <- read.csv("raw_data/vaccines_by_age.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date == max(Date)) %>%
  select(Agegroup, Percent_fully_vaccinated, Percent_3doses) %>%
  .[1:12, ] %>%
  DT::datatable(colnames=c("Age Group","Fully Vaccinated Percent","3 Doses Percent"),
                options = list(pageLength = 13))

data_vac_status_hosp_icu <- read.csv("raw_data/vac_status_hosp_icu.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(date == max(date))

data_vac_status_icu <- data_vac_status_hosp_icu %>%
  select(date, icu_unvac, icu_partial_vac, icu_full_vac) %>%
  pivot_longer(cols = !date, names_to = "Group", values_to = "value")

data_vac_status_hosp <- data_vac_status_hosp_icu %>%
  select(date, hospitalnonicu_unvac, hospitalnonicu_partial_vac, hospitalnonicu_full_vac) %>%
  pivot_longer(cols = !date, names_to = "Group", values_to = "value")


# Define UI for vaccination page
vaccine_ui <- fluidPage(
  # page title
  titlePanel("Vaccine Rate by Age"), 
  br(),
  fluidRow(
    column(4, DT::dataTableOutput("vaccine_rate_table")),
    column(8)
  ),
  
  br(),
  titlePanel("Vaccination Progress over Time"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "vaccine_total_or_daily",
                  label = "Total or daily",
                  selected = "Daily", choices = c("Total", "Daily")),
      dateRangeInput(inputId = 'vaccine_date_range',
                     label = "Select date range (Past 180 days by default)",
                     start = max(data_vaccine$report_date) - 180,
                     end = max(data_vaccine$report_date),
                     min = min(data_vaccine$report_date),
                     max = max(data_vaccine$report_date))
    ),
    mainPanel(plotlyOutput("vaccine")),
  ),
  
  br(),
  titlePanel("Hospitalization/ICU by vaccine status"),
  br(),
  fluidRow(
    column(6, plotOutput("hosp_by_vaccine_status")),
    column(6, plotOutput("icu_by_vaccine_status")),
  )
) 


# Define UI for cases page
cases_ui <- fluidPage(
  titlePanel("Cases"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cases_total_or_daily",
                  label = "Total or daily",
                  selected = "Daily", choices = c("Total", "Daily")),
      dateRangeInput(inputId = "cases_date_range",
                     label = "Select date range (Past 180 days by default)",
                     start = max(data_cases$Reported.Date)-180,
                     end = max(data_cases$Reported.Date),
                     min = min(data_cases$Reported.Date),
                     max = max(data_cases$Reported.Date),
                     ),      
    ),
    mainPanel(plotlyOutput("cases")),
  ),
  
  br(),
  titlePanel("Deaths"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "deaths_total_or_daily",
                  label = "Total or daily",
                  selected = "Daily", choices = c("Total", "Daily")),
      dateRangeInput(inputId = "deaths_date_range",
                     label = "Select date range (Past 180 days by default)",
                     start = max(data_cases$Reported.Date)-180,
                     end = max(data_cases$Reported.Date),
                     min = min(data_cases$Reported.Date),
                     max = max(data_cases$Reported.Date),
      ), 
    ),
    mainPanel(plotlyOutput("deaths")),
  ),
  
  br(),
  titlePanel("Active Cases"),
  br(),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = 'active_cases_date_range',
                     label = "Select date range (Past 180 days by default)",
                     start = max(data_cases$Reported.Date)-180,
                     end = max(data_cases$Reported.Date),
                     min = min(data_cases$Reported.Date),
                     max = max(data_cases$Reported.Date),
      )
    ),
    mainPanel(plotlyOutput("active_cases")),
  ),
)

ui <- navbarPage(
  "Covid-Dashboard",
  theme = my_theme,
  tabPanel("Summary"),
  tabPanel("Cases", cases_ui),
  tabPanel("Vaccinations", vaccine_ui),
  tabPanel("Hospitalization/ICU"),
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$vaccine_rate_table = DT::renderDataTable({
      data_vaccine_rate_latest
    })
    
    output$vaccine <- renderPlotly({
      
      if(input$vaccine_total_or_daily == "Total") {
        
        filter_data <- data_vaccine %>% 
          filter(report_date >= input$vaccine_date_range[1],
                 report_date <= input$vaccine_date_range[2]) %>%
          dplyr::select(c("report_date", "total_individuals_at_least_one", "total_individuals_fully_vaccinated",
                        "total_individuals_3doses")) %>%
          pivot_longer(!report_date, names_to = "vaccination_status", values_to = "count")
  
        our_plot <- filter_data %>% 
            ggplot(aes(x = report_date, y = count,fill = vaccination_status)) +
            geom_bar(stat = "identity") + 
            labs(title = "Total vaccinations number over time",
                 x = "Date",
                 y = "Number of Total vaccination",
                 fill = "Vaccination Status")
          our_plotly_plot <- ggplotly(our_plot)
        } else {
          filter_data <- data_vaccine %>% 
            filter(report_date >= input$vaccine_date_range[1],
                   report_date <= input$vaccine_date_range[2]) %>%
            dplyr::select(c("report_date", "previous_day_at_least_one", "previous_day_fully_vaccinated",
                          "previous_day_3doses")) %>%
            pivot_longer(!report_date, names_to = "vaccination_status", values_to = "count")
          
          our_plot <- filter_data %>% 
            ggplot(aes(report_date, count, fill = vaccination_status)) +
            geom_bar(stat = "identity") + 
            # scale_fill_discrete(labels = c("3 doses", "At least one dose ", "Fully vaccinated")) + 
            labs(title = "Daily vaccinations number over time",
                 x = "Date",
                 y = "Number of daily vaccine",
                 fill = "Vaccination Status")
          our_plotly_plot <- ggplotly(our_plot)
      }
      
      return(our_plotly_plot)
    })
    
    output$hosp_by_vaccine_status <- renderPlot({
      our_plot <- ggplot(data_vac_status_hosp, aes(x = "", y = value, fill = Group)) +
        geom_bar(stat = "identity") + coord_polar("y", start=0)
      return(our_plot)
    })
    
    output$icu_by_vaccine_status <- renderPlot({
      our_plot <- ggplot(data_vac_status_icu, aes(x = "", y = value, fill = Group)) +
        geom_bar(stat = "identity") + coord_polar("y", start=0)
      return(our_plot)
    })
    
    output$cases <- renderPlotly({
      filter_data <- data_cases %>% 
        filter(Reported.Date >= input$cases_date_range[1],
               Reported.Date <= input$cases_date_range[2])
      if(input$cases_total_or_daily == "Daily") {
        our_plot <- filter_data %>% 
          ggplot(aes(x = Reported.Date, y = Total.Cases - lag(Total.Cases, 1))) +
          geom_bar(stat = "identity", fill = "#C7CCB9") +  
          labs(title = "Daily cases number over time",
               x = "Date",
               y = "Number of Daily Cases")
        our_plotly_plot <- ggplotly(our_plot)
      } else {
        our_plot <- filter_data %>% 
          ggplot(aes(x = Reported.Date, y = Total.Cases)) +
          geom_bar(stat = "identity", fill = "#C7CCB9") + 
          labs(title = "Total cases over time",
               x = "Date",
               y = "Number of Total Cases")
        our_plotly_plot <- ggplotly(our_plot)
      }
      return(our_plotly_plot)
    })
    
    output$deaths <- renderPlotly({
      filter_data <- data_cases %>% 
        filter(Reported.Date >= input$deaths_date_range[1],
               Reported.Date <= input$deaths_date_range[2])
      if(input$deaths_total_or_daily == "Daily") {
        our_plot <- filter_data %>% 
          ggplot(aes(x = Reported.Date, y = Deaths - lag(Deaths, 1))) +
          geom_bar(stat = "identity", fill = "#C7CCB9") + 
          labs(title = "Daily deaths number over time",
               x = "Date",
               y = "Number of Daily Cases")
        our_plotly_plot <- ggplotly(our_plot)
      } else {
        our_plot <- filter_data %>% 
          ggplot(aes(x = Reported.Date, y = Deaths)) +
          geom_bar(stat = "identity", fill = "#C7CCB9") + 
          labs(title = "Total Deaths over time",
               x = "Date",
               y = "Number of Total Deaths")
        our_plotly_plot <- ggplotly(our_plot)
      }
      return(our_plotly_plot)
    })
    
    output$active_cases <- renderPlotly({
      filter_data <- data_cases %>% 
        filter(Reported.Date >= input$deaths_date_range[1],
               Reported.Date <= input$deaths_date_range[2])
      our_plot <- filter_data %>% 
        ggplot(aes(x = Reported.Date, y = Confirmed.Positive)) +
        geom_bar(stat = "identity", fill = "#C7CCB9") + 
        labs(title = "Active cases over time",
              x = "Date",
              y = "Number of Active Cases")
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
