# data incubator
# author: "Karim Hammoud"


# Project Description

# I have provided with data about mortality from all 50 states and the District of Columbia.

# Please access it at https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data.

#For more info about the project you can visit the CDC WONDER system, at https://wonder.cdc.gov/ucd-icd10.html.

#+ I am using the 'shiny' package.and I am using R package that supports interactive graphing such as plotly.


# As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.

## Gathering info

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)
library(DT)

df <-  read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)


# Need to Rank
df$Rank <-  df$Crude.Rate %>%
  rank() %>%
  round(0)

df$Rank <- max(df$Rank+1) - df$Rank


# Creating a National Average Summary ----
national_average_summary <- df %>%
  group_by(ICD.Chapter, Year) %>%
  summarise(
    'Deaths' = sum(Deaths), 
    'Population' = sum(Population), 
    'NationalAverage' = round(100000*sum(Deaths)/sum(Population),1)) %>%
  arrange(desc(NationalAverage))

#a1 <- select(filter(df2_summary, State == 'NY' & Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths, NationalAverage))

#a <- select(filter(my.mortality.data, Year == '1999'),c(State,Year,ICD.Chapter,Population,Deaths))
#sum(a$Deaths)


### About the data

# **Crude Rates**

# Crude Rates are expressed as the number of deaths reported each calendar year per the factor you select. The default factor is per 100,000 population, reporting the death rate per 100,000 persons.

# More information can be seeing here: https://wonder.cdc.gov/wonder/help/ucd.html#



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Define UI for application that draws a histogram

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter REACTIVE Data by Year and ICD.Chapter  ----
  selectedData <- reactive({
    dfSlice <- df %>%
      filter((Year == input$year) , (ICD.Chapter == input$cause) ) %>%
      mutate(Rank=min_rank(Rank)) %>%
      arrange(Rank)
  })
  
  
  # Plot using plot.ly  ----
  output$plot1 <- renderPlotly({
    
    plot_ly(selectedData(), x = ~State, y = ~Crude.Rate, type=input$option_Graph) %>%
      layout(title = paste(input$year, input$cause, sep = " - "), #"Mortality Rates by State",
             xaxis = list(categoryorder = "array",
                          categoryarray = ~State,
                          type = "category"),
             yaxis = list(title = "Crude Mortality Rate"))
  })
  
  # Generate a summary of the data ----
  output$MyRanks <- renderTable({
    head(selectedData()[,c(-1,-3)],5) # Generate top 5 Ranks of selected columns
  })
  
  # Downloadable csv of selected dataset ----
  #output$downloadData <- downloadHandler(
  #  filename = function() {
  #    paste(input$dataset, ".csv", sep = "")
  #  },
  #  content = function(file) {
  #    write.csv(datasetInput(), file, row.names = FALSE)
  #  })
  
  #output$txt <- renderText({
  #  paste("Rendering: ", input$cause)
  #})
  
}




