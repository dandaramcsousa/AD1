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

series = read_csv("https://raw.githubusercontent.com/nazareno/imdb-series/master/data/series_from_imdb.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Desempenho de uma série ao longo das temporadas"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         h2("Séries IMDB"),
         p("Aqui vamos observar como se comporta uma série ao longo das temporadas. Abaixo escolha a série
           de sua preferência:"),
         selectInput("select", label = ("Nome da série"), choices = sort(unique(series$series_name))),
         p("Para interagir com o gráfico basta clicar no número da temporada na direita. Selecionando as temporadas que você deseja ver. Se não houver a opção é porque a série só possui uma temporada."),
         p("É possível observar o quanto a série cresce ou decresce de acordo com as temporadas. Uma série com várias temporadas ainda alcançam notas altas?")
      ),
      # Show a plot of the generated distribution
      mainPanel(
         h1("Nota do usuário para cada episódio"),
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 names(series)[names(series) == "season_ep"]<- "Episódio"
  names(series)[names(series) == "UserRating"]<- "Nota"
  
   output$distPlot <- renderPlotly({
     plot_ly(data = series %>% filter(series_name %in% c(input$select)),x = ~Episódio ,y = ~Nota) %>%
       add_lines(hoverinfo = 'text', text=~paste('Episódio: ', Episódio, '<br>Temporada: ', season, '<br> Nome do episódio: ', Episode, '<br> Nota: ', Nota), color=~as.character(season))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

