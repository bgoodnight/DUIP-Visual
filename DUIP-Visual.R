library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)

#Read in data
short <- read.csv("~/alldata.csv")

#select variables for analysis
myvars <- c("Awardee","Program","Category","Indicator.Number","Indicator.Name",
            "X2013.Age.Adjusted.Rate","X2014.Age.Adjusted.Rate","X2015.Age.Adjusted.Rate","X2016.Age.Adjusted.Rate","Beta")
short <- short[myvars]

#set all zeros to NA
short[short == 0] <- NA

#change column names to years
colnames(short)[6:9] <- 2013:2016

#create long data frame with with years as new variable
data = melt(short, measure.vars = c("2013","2014","2015","2016"))

#change state names to lower-case for merge
data["Awardee"] <- mutate_all(data["Awardee"], funs(tolower))

#remove rows with missing data to reduce data frame size
data <- data[complete.cases(data[ , "value"]),]

#change indicator values and years to numeric data
data$value <- as.double(data$value)
data$variable <- as.double(data$variable)

data %>%
  group_by(Awardee, Indicator.Number) %>%
  mutate(number = n()) -> data

data[data$number > 2,] %>%
  group_by(Awardee, Indicator.Number) %>% # You can add here additional grouping variables if your real data set enables it
  do(mod = lm(value ~ variable, data = .)) %>%
  mutate(Slope = summary(mod)$coeff[2]) %>%
  select(-mod) %>%
  inner_join(data, by = c("Awardee", "Indicator.Number")) -> data

#adjust years to range from 2012 to 2016
data$variable <- data$variable + 2012

#import shape data for states
states <- map_data("state")

#change state name column in state shape data frame to match indicator data
colnames(states)[5] <- "Awardee"

#merge state shape data with indicator data
states <- inner_join(states, data, by = "Awardee")

#create new data frame with center points (long and lat) for each state
cnames <- aggregate(cbind(long, lat) ~ Awardee, data=states, 
                    FUN=function(x)mean(range(x)))

#change variable names for long and lat in center point data frame to differentiate with other frame
colnames(cnames)[2:3] <- c("clong","clat")

#merge center point data frame with state shape and indicator data
states <- inner_join(cnames, states, by = "Awardee")

#create new variable sign that indicates whether beta coefficient is positive or negative
states[["sign"]] = ifelse(states[["Slope"]] >= 0, "positive", "negative")

#create UI
ui <- fluidPage(
  theme = "bootstrap.css",
  titlePanel("Opioid Mortality and Morbidity Trends", windowTitle = "Opioid Mortality and Morbidity Trends"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput","Year",
                  min = 2013,
                  max = 2020,
                  sep = "",
                  value = c(2013,2015)),
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
      plotOutput("coolplot", height = 600),
      br(), br(),
      plotOutput("trendplot", height = 200),
      br(), br(),
      tableOutput("results")
      )
)
)

server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      states %>%
      filter(
        Indicator.Number == input$indicatorInput
        )
    
    ggplot(data = filtered) + 
      geom_polygon(data = map_data("state"), aes(x=long, y = lat, group = group), fill = "grey", color = "white") +
      geom_polygon(aes(x = long, y = lat, fill = value, group = group), color = "black") + 
      scale_fill_gradient(low='lightblue', high='black') +
      geom_point(aes(clong, clat, size = (Slope), color = sign, shape = sign), fill = "white") +
      scale_shape_manual(values=c(25, 24)) +
      scale_color_manual(values=c("darkgreen", "red")) +
      coord_fixed(1.3) +
      theme(legend.position="none")
      
  })
  
  output$trendplot <- renderPlot({
    filtered <-
      data %>%
      filter(
        Indicator.Number == input$indicatorInput
      )
    
    ggplot(filtered,aes(variable,value)) +
      stat_summary(fun.data = "mean_se", color = "red", size = 2) + 
      geom_smooth(method='lm',fullrange=TRUE) + 
      xlim(input$yearInput[1], input$yearInput[2])
    
  })
  
  output$results <- renderTable({
    filtered <-
      short %>%
      filter(
        Indicator.Number == input$indicatorInput
      ) %>%
      select(Awardee, "2013", "2014", "2015", "2016", "Beta")
    filtered
  })

}

shinyApp(ui = ui, server = server)