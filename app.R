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
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(ggpubr)

my_theme <- bs_theme(bootswatch = "cosmo")
#theme_set(theme_light())

data_cases <- read.csv("raw_data/covidtesting.csv") %>%
  mutate(Reported.Date = as.Date(Reported.Date)) 

data_vaccine <- read.csv("raw_data/vaccine_doses.csv") %>%
  mutate(report_date = as.Date(report_date))

data_vaccine_rate <- read.csv("raw_data/vaccines_by_age.csv") %>%
  mutate(Date = as.Date(Date))

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

# data for hospital page
df <- read_csv('experiment/data/icu.csv')
df_hosp_breakdown<-read_csv('experiment/data/hosp_icu_c19_breakdown.csv')
df_icu_beds<-read_csv('experiment/data/icu_beds.csv')

#data for summary page
df_summary <- read_csv('main/data/covidtesting.csv')
vac_df_summary<-read_csv('main/data/vaccines_by_age.csv')
df_summary$dailydeath=  c(NA,diff(df_summary$Deaths))
df_summary$dailyalpha=c(NA,diff(df_summary$Total_Lineage_B.1.1.7_Alpha))
df_summary$dailybeta=c(NA,diff(df_summary$Total_Lineage_B.1.351_Beta))
df_summary$dailygamma=c(NA,diff(df_summary$Total_Lineage_P.1_Gamma))
df_summary$dailydelta=c(NA,diff(df_summary$Total_Lineage_B.1.617.2_Delta))

# ui for summary page
sum_ui <- fluidPage(
    
  # Application title
  titlePanel(h1("Data Summary", align="center",
                style ="font-family: 'times'; font-size: 32pt ")),
  fluidRow(column(8, offset = 2,
                  p("In this section, we summarize all the important data from the dashboard",align="center")
  )   ),
  tags$hr(),
  fluidRow(column(3,
                  br(),
                  br(),
                  wellPanel(
                    
                    dateRangeInput(inputId = 'date_range_sum',
                                   label = "Select date range for confirmed cases",
                                   start = min(df_summary$ReportedDate),
                                   end = max(df_summary$ReportedDate)),
                    actionButton(inputId = "show_sum", 
                                 label = "Show Instructions")
                    
                  )       
                  
  ),
  
  column(5,
         plotlyOutput("dailycomfirmed")
         
  ),
  
  column(4,plotlyOutput("totalcase"))
  ),
  tags$hr(),
  fluidRow(
    column(3,
           wellPanel(
             
             dateRangeInput(inputId = 'date_range_sum_death',
                            label = "Select date range for death",
                            start = min(df_summary$ReportedDate),
                            end = max(df_summary$ReportedDate)))),
    column(5,plotlyOutput("dailydeath")),
    column(4,plotlyOutput("totaldeath"))
  ),
  tags$hr(),
  fluidRow(
    column(
      2,
      wellPanel(dateInput(inputId='show_date_sum',
                label="select the date to show",
                value='2022-01-01',
                min='2021-11-11',
                max='2022-03-11'))
    ),
    column(
      4,
      plotlyOutput("vac")
    ),
    column(
      3,
      plotOutput('perc_adult_sum')
    ),
    column(
      3,
      plotOutput('perc_child_sum')
    )
  ),
  tags$hr(),
  fluidRow(
    plotlyOutput("covidtype")
  )
  
)
#ui for hospital and icu
hos_ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "cosmo"),
  # Application title
  titlePanel(h1("Hospital and ICU", align="center",
                style ="font-family: 'times'; font-size: 32pt ")),
  tags$hr(),
  h2("Bed Ratio for Selected Dates"),
  fluidRow(
    column(
      2,
      wellPanel(dateInput(inputId='show_date',
                  label="select the date to show",
                  value='2022-03-11',
                  min='2021-11-11',
                  max='2022-03-11'
                ),
       "Shown on the right is the proportion of adult ICU and pediatric ICU use. 'avaliable' stands for vacant beds, 'crci' stands for ICU admission due to covid-related illness, and 'other' stands for ICU admission for other illnesses"         
    )
    ),
    column(
      5,plotOutput('perc_adult')
      
    ),
    column(
      5,plotOutput('perc_child')
    )
  ),
  tags$hr(),
  h2("Number of people with Covid-19 in ICU"),
  fluidRow(
    column(
      2,
      wellPanel(dateRangeInput(inputId = 'date_range_icu_line',
                               label = "Select date range",
                               start = min(df$date),
                               end = max(df$date)),
                selectInput(inputId = "district",
                            label = "select district", 
                            selected = "TORONTO", choices = c("TORONTO", "CENTRAL","EAST","WEST","NORTH")),
                actionButton(inputId = "show", 
                             label = "Show Instructions")
                )
    ),
    column(
      10,plotlyOutput('daily_outcomes')
      
    )
  ),
  tags$hr(),
  fluidRow(
    column(
      2,
      wellPanel(dateRangeInput(inputId = 'date_range',
                               label = "Select date range",
                               start = min(df$date),
                               end = max(df$date))
      ),
      "In this section we provide four graphs, the two on the left show how many of the existing ICU and hospitalized patients are caused by covid. The two graphs on the right are the change in the number of beds over time"
    ),
    column(
      5,
      plotlyOutput('breakdown'),
      plotlyOutput('icu_breakdown')
      
      
    ),
    column(
      5,
      plotlyOutput('adult_beds'),
      plotlyOutput('child_beds')
    )
  ),
  
)

# Define UI for vaccination page
vaccine_ui <- fluidPage(
  # page title
  titlePanel(h1("Vaccine", align="center",
                style ="font-family: 'times'; font-size: 32pt ")),
  tags$hr(),
  titlePanel("Vaccine Rate by Age"), 
  br(),
  fluidRow(
    column(4, DT::dataTableOutput("vaccine_rate_table")),
    column(8,
           selectInput(inputId = "fully_or_3doses",
                       label = "Fully vaccinated or 3 doses",
                       selected = "Fully Vaccinated", choices = c("Fully Vaccinated", "Three Doses")),
           dateRangeInput(inputId = 'vaccine_rate_date_range',
                          label = "Select date range (Past 90 days by default)",
                          start = max(data_vaccine_rate$Date)-90,
                          end = max(data_vaccine_rate$Date),
                          min = min(data_vaccine_rate$Date),
                          max = max(data_vaccine_rate$Date)),   
           plotlyOutput("vaccine_rate"))
  ),
  
  br(),
  tags$hr(),
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
  tags$hr(),
  titlePanel("Hospitalization/ICU by vaccine status"),
  br(),
  fluidRow(
    column(6, plotOutput("hosp_by_vaccine_status")),
    column(6, plotOutput("icu_by_vaccine_status")),
  )
) 


# Define UI for cases page
cases_ui <- fluidPage(
  titlePanel(h1("Basic Cases Information", align="center",
                style ="font-family: 'times'; font-size: 32pt ")),
  tags$hr(),
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
  tags$hr(),
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
  tags$hr(),
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
  tabPanel("Summary",sum_ui),
  tabPanel("Cases", cases_ui),
  tabPanel("Vaccinations", vaccine_ui),
  tabPanel("Hospitalization/ICU",hos_ui),
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$vaccine_rate_table = DT::renderDataTable({
      data_vaccine_rate_latest
    })
    
    output$vaccine_rate <- renderPlotly({
      if (input$fully_or_3doses == "Fully Vaccinated") {
        filter_data <- data_vaccine_rate %>%
          filter(Date >= input$vaccine_rate_date_range[1],
                 Date <= input$vaccine_rate_date_range[2]) %>%
          select(Date, Agegroup, Percent_fully_vaccinated)
        
        our_plot <- filter_data %>%
          ggplot(aes(x = Date, y = Percent_fully_vaccinated, color = Agegroup)) +
          geom_line() +
          ylim(0, 1)+
          labs(title = "Vaccination Rate over Time",
               x = "Date",
               y = "Vaccination Rate")
      } else {
        filter_data <- data_vaccine_rate %>%
          filter(Date >= input$vaccine_rate_date_range[1],
                 Date <= input$vaccine_rate_date_range[2]) %>%
          select(Date, Agegroup, Percent_3doses)
        
        our_plot <- filter_data %>%
          ggplot(aes(x = Date, y = Percent_3doses, color = Agegroup)) +
          geom_line() +
          ylim(0, 1)+
          labs(title = "Vaccination Rate over Time",
               x = "Date",
               y = "Vaccination Rate")
      }
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
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
      our_plot <- ggplot(data_vac_status_hosp, aes(x="", y = value, fill = Group)) +
        geom_bar(stat = "identity") + coord_polar("y", start=0)+
        guides(fill=guide_legend(title = NULL))+ 
        theme(legend.position = 'top',axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        scale_fill_discrete(labels=c("full vac", "part vac", "unvac"))+
        labs(title = "Hospitalization (Non-ICU) Vaccine Status")
      return(our_plot)
    })
    
    output$icu_by_vaccine_status <- renderPlot({
      our_plot <- ggplot(data_vac_status_icu, aes(x="",y = value, fill = Group))+
      geom_bar(stat = "identity") + coord_polar("y", start=0)+
        guides(fill=guide_legend(title = NULL))+ 
        theme(legend.position = 'top',axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        scale_fill_discrete(labels=c("full vac", "part vac", "unvac"))+
        labs(title = "ICU Vaccine Status")
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
    
    output$perc_adult_sum<- renderPlot({
      #choose the data from right time
      
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date == input$show_date_sum
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
        xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
        theme(axis.ticks.y = element_blank(),legend.position = 'top' ,axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        guides(fill=guide_legend(title = NULL))+ 
        scale_fill_discrete(labels=c("available", "crci", "other reason"))+
        labs(title = "Adult ICU Ratio")
      
    })
    
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
        xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
        theme(axis.ticks.y = element_blank(),legend.position = 'top' ,axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        guides(fill=guide_legend(title = NULL))+ 
        scale_fill_discrete(labels=c("available", "crci", "other reason"))+
        labs(title = "Adult ICU Ratio")
        
    })
    
    output$perc_child_sum<- renderPlot({
      #choose the data from right time
      
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date == input$show_date_sum
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
        xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
        theme(axis.ticks.y = element_blank(),legend.position = 'top',axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        guides(fill=guide_legend(title = NULL))+ 
        scale_fill_discrete(labels=c("available", "crci", "other reason"))+
        labs(title = "Child ICU Ratio")
      
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
        xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
        theme(axis.ticks.y = element_blank(),legend.position = 'top',axis.line=element_blank(),axis.text=element_blank(), axis.title = element_blank(),legend.text=element_text(size=15))+
        guides(fill=guide_legend(title = NULL))+ 
        scale_fill_discrete(labels=c("available", "crci", "other reason"))+
        labs(title = "Child ICU Ratio")
  
    })
    
    
    output$daily_outcomes<- renderPlotly({
      #choose the data from right time
      filter_data <- df %>% 
        filter(date >= input$date_range_icu_line[1],
               date <= input$date_range_icu_line[2],
               oh_region==input$district)
      filter_data_hosp_breakdown <- df_hosp_breakdown %>% 
        filter(date >= input$date_range_icu_line[1],
               date <= input$date_range_icu_line[2],
        )
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= input$date_range_icu_line[1],
               date <= input$date_range_icu_line[2],
        )
      
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      
      
      # draw the histogram with the specified number of bins
      
      
      our_plot<-ggplot(filter_data)+
        geom_line(aes(x=date, y=icu_crci_total,colour='total icu of crci')) +
        geom_line( aes(x=date, y=icu_crci_total_vented, colour='total vented icu of crci'))+
        geom_line( aes(x=date, y=icu_current_covid_vented, colour='patient vented and covid positive'))+
        geom_line( aes(x=date, y=icu_current_covid, colour='patinet in icu and covid positive'))+
        scale_colour_manual("",values = c(
          "total icu of crci" = "dark red",
          "total vented icu of crci" = "dark green",
          'patient vented and covid positive'="dark blue",
          'patinet in icu and covid positive'='yellow'
          ))
      
      
      our_plot<-our_plot+
        labs(x="date", y="number", title="ICU COVID-19 population")+
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
      
      # draw the histogram with the specified number of bins
      hosp_breakdown<- melt(filter_data_hosp_breakdown[,c(1,2,3)],id.vars="date",variable.name="type",value.name="pop")
      
      our_plot<-ggplot(hosp_breakdown)+
        geom_bar(stat = 'identity',aes(x=date, y=pop,fill =type),position='stack')+
        guides(fill=guide_legend(title = NULL,label.position='top'))
      
      
      our_plot<-our_plot+
        labs(x="date", y="ratio", title="Covid-19 hospitalization rate changes over time")
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plot)
      
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
        labs(x="date", y="ratio", title="Covid-19 ICU rate changes over time")+
        theme(legend.position = "bottom") 
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$adult_beds<- renderPlotly({
      #choose the data from right time
      
      filter_data_icu_beds <- df_icu_beds %>% 
        filter(date >= '2022-01-10'
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
        labs(x="date", y="number", title="Adult ICU beds")
      
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
        labs(x="date", y="number", title="Number of ICU beds for children")
        
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    observeEvent(input$show, {
      
      print(input$date_range)
      
      showModal(modalDialog(
        title = "Input Instructions",
        "Please select the region and time to be displayed"
      ))
    })
    
    output$dailycomfirmed <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=ConfirmedPositive,fill=ConfirmedPositive))
      our_plot<-our_plot+
        scale_fill_gradient(low = "dark green", high =  "dark red")+
        labs(x="date", y="number of people", title="Daily new confirmed cases")+
        theme(plot.title = element_text(hjust=0.5))
      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$totalcase <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum[1],
               ReportedDate <= input$date_range_sum[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_line(stat='identity',aes(x=ReportedDate, y=TotalCases))
      our_plot<-our_plot+
        labs(x="date", y="number", title="Total Confirmed Cases")+
        theme(plot.title = element_text(hjust=0.5))
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$totaldeath <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum_death[1],
               ReportedDate <= input$date_range_sum_death[2])
      our_plot<-ggplot(filter_df_summary)+
        geom_line(stat='identity',aes(x=ReportedDate, y=Deaths))
      our_plot<-our_plot+
        labs(x="date", y="number", title="total number of deaths")+
        theme(plot.title = element_text(hjust=0.5))
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
    })
    
    output$dailydeath <- renderPlotly({
      filter_df_summary <- df_summary %>% 
        filter(ReportedDate >= input$date_range_sum_death[1],
               ReportedDate <= input$date_range_sum_death[2])
      
      our_plot<-ggplot(filter_df_summary)+
        geom_bar(stat='identity',aes(x=ReportedDate, y=dailydeath,fill=dailydeath))+
        scale_fill_gradient(low = "dark green", high =  "dark red")
      our_plot<-our_plot+
        labs(x="date", y="number", title="Daily new deaths")+
        theme(plot.title = element_text(hjust=0.5))
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
        labs(x="date", y="number of cases", title="New cases of different types of viruses (partial data)")+
        theme(plot.title = element_text(hjust=0.5))
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    
    output$vac<- renderPlotly({
      #choose the data from right time
      
      filter_vac <- vac_df_summary %>% 
        filter(Date == input$show_date_sum,
               Agegroup!="Adults_18plus"&Agegroup!="Ontario_12plus"&Agegroup!="Undisclosed_or_missing"&Agegroup!="Ontario_5plus"
        )
      filter_vac$novac=1-filter_vac$Percent_fully_vaccinated
      #child_icu<- data.frame(melt(filter_data_icu_beds[,c(9,7,8)],variable.name="type",value.name="pop"))
      filter_vac_melt<-data.frame(melt(filter_vac[,c(2,9,11)],id.vars=c('Agegroup'),variable.name="type",value.name="val"))
      our_plot<-ggplot(filter_vac_melt)+
        geom_bar(stat = 'identity',aes(x=Agegroup, 
                                       y=val,
                                       fill=type),
                 position='stack'
        )+
        #scale_fill_gradient(name="%",low = "dark green", high =  "dark red")+
        #coord_cartesian(ylim = c(0, 1))+
        coord_flip()
      
      
      our_plot<-our_plot+
        labs(x="age", y="percentages", title="Proportion of fully vaccinated by age group")+
        theme(legend.position = "bottom")

      
      our_plotly_plot <- ggplotly(our_plot)
      return(our_plotly_plot)
      
    })
    observeEvent(input$show_sum, {
      
      print(input$date_range)
      
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
