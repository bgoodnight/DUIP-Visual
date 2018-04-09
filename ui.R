library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Opioid Trends"),
    dashboardSidebar(
      sidebarMenu(
        selectInput("indicatorInput", "Indicator", c("All Drug Overdose Deaths" = 1,
                                                     "Drug Overdose Deaths Involving Opioids" = 2,
                                                     "Drug overdose deaths involving natural, semi-synthetic, and synthetic opioids" = 3,
                                                     "Drug overdose deaths involving prescription opioid pain relievers: Natural and semi-synthetic opioids and methadone" = 4,
                                                     "Drug overdose deaths involving natural and semi-synthetic opioids" = 5,
                                                     "Drug overdose deaths involving synthetic opioids other than methadone" = 6,
                                                     "Drug overdose deaths involving methadone" = 7,
                                                     "Drug overdose deaths involving heroin" = 8
                                                     # "All drug overdose emergency department visits" = 9,
                                                     # "Emergency department visits involving all opioid overdose excluding heroin" = 10,
                                                     # "Emergency department visits involving heroin overdose" = 11,
                                                     # "All drug overdose hospitalizations" = 12,
                                                     # "Hospitalizations involving all opioid overdose excluding heroin" = 13,
                                                     # "Hospitalizations involving heroin overdose" = 14
        ), selected = 1),
        menuItem("Heat Map", icon = icon("map"), tabName = "heatmap"),
        menuItem("Forecast", icon = icon("line-chart"), tabName = "projection"),
        menuItem("Raw Data", icon = icon("file-text"), tabName = "data")
      )
    ),
    dashboardBody(
        tabItems(
          tabItem(
            tabName = "heatmap",
                  fluidRow(
                    box(plotOutput("coolplot", height=600), width=9),
                    box(
                      sliderInput("yearInput","Year",
                                  min = 2013,
                                  max = 2016,
                                  sep = "",
                                  value = 2013,
                                  animate = 
                                    animationOptions(interval=3000)),
                      radioButtons('format', 'Export report as', c('Word', 'HTML')), downloadButton('downloadReport'),
                      width=3
                    )
                  )
          ),
          tabItem(
            tabName = "projection",
            fluidRow(
              box(plotOutput("projectionPlot", height=600), width=9),
              box(
                sliderInput("projectionInput","Prediction Year",
                            min = 2017,
                            max = 2020,
                            sep = "",
                            value = 2017,
                            animate = 
                              animationOptions(interval=3000)),
                width=3
              ),
              box(plotOutput("trendplot", height=200), width=9)
            )
            
          ),
          tabItem(
            tabName = "data",
            fluidRow(
              box(tableOutput("results"),
                  downloadButton("downloadData", "Download CSV"), width=9)
              
            )
          )
          
      )
    )
  )
