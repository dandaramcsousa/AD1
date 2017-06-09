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

library(knitr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Atividade 2 - Checkpoint 2"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         selectInput("select", label = h3("Séries"), choices = sort(unique(series_antigas$series_name)))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cresc = series_antigas %>%
    group_by(series_name,season) %>%
    summarise(Mediana = median(UserRating))
  
   output$distPlot <- renderPlot({
     ggplot(data = cresc %>% filter(series_name %in% c(input$select)),aes(x = season ,y = Mediana)) + 
       geom_point(alpha=.8) + 
       geom_line() +
       scale_x_continuous(breaks=seq(0, 13, 1)) +
       scale_y_continuous(breaks=seq(6, 10, .5)) +
       labs(x = "Temporada",y = "Mediana", title = "Notas por temporada") +
       theme_bw() +
       theme(legend.position = "bottom")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

