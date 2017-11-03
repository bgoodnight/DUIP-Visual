#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "bootstrap.css",
  titlePanel("Opioid Mortality and Morbidity Trends", windowTitle = "Opioid Mortality and Morbidity Trends"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput","Year",
                  min = 2013,
                  max = 2015,
                  sep = "",
                  value = 2013,
                  animate = 
                    animationOptions(interval=3000)),
      selectInput("indicatorInput", "Indicator", c("All Drug Overdose Deaths" = 1,
                                                   "Drug Overdose Deaths Involving Opioids" = 2,
                                                   "Drug overdose deaths involving natural, semi-synthetic, and synthetic opioids" = 3,
                                                   "Drug overdose deaths involving prescription opioid pain relievers: Natural and semi-synthetic opioids and methadone" = 4,
                                                   "Drug overdose deaths involving natural and semi-synthetic opioids" = 5,
                                                   "Drug overdose deaths involving synthetic opioids other than methadone" = 6,
                                                   "Drug overdose deaths involving methadone" = 7,
                                                   "Drug overdose deaths involving heroin" = 8,
                                                   "All drug overdose emergency department visits" = 9,
                                                   "Emergency department visits involving all opioid overdose excluding heroin" = 10,
                                                   "Emergency department visits involving heroin overdose" = 11,
                                                   "All drug overdose hospitalizations" = 12,
                                                   "Hospitalizations involving all opioid overdose excluding heroin" = 13,
                                                   "Hospitalizations involving heroin overdose" = 14
      ), selected = 8)
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Map", plotOutput("coolplot", height = 600)),
                  tabPanel("Trend", plotOutput("trendplot", height = 200)),
                  tabPanel("Data", tableOutput("results"))
                  )
      
    )
  )
)
