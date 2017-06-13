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
library(dplyr)
library(tidyr)
library(readr)
library(plotly)

library(knitr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Atividade 2 - Checkpoint 2"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("select", label = h3("Séries"), choices = sort(unique(series$series_name)))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cresc = series %>%
    group_by(series_name,season) %>%
    summarise(Mediana = median(UserRating))
  
   output$distPlot <- renderPlotly({
     plot_ly(data = cresc %>% filter(series_name %in% c(input$select)),x = ~season ,y = ~Mediana) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

