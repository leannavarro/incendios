library(shiny)
library(ggplot2)
library(wordcloud2)
library(tidyverse)

options(scipen = 20)

ui <- fluidPage(
  
  titlePanel("Incendios forestales según los medios de comunicación"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(h2('Filtros'))
    ,
    selectInput('alcance',
                label=h3('Seleccione alcance del medio'),
                choices = unique(data_token$scope_medio)
                ),
    
    uiOutput("medio_nombre")
    ),
    
    mainPanel( 
      wordcloud2Output(outputId = 'graficoWord'),
      fluidRow(column(6, plotOutput('graficoTf_idf')))
      )
    
    )
)

server <- function(input, output) {
  
  df <- reactive({
    df = data_token[data_token$name_medio==input$output_medio,]
    df
  })
  
  ds <- reactive({
    ds <- data_token_group[data_token_group$name_medio==input$output_medio,]
    ds
  })
  
  
  output$medio_nombre <- renderUI({
    selectInput(inputId="output_medio", h3("Seleccionar el medio"), 
                choices = unique(data_token[data_token$scope_medio==input$alcance,'name_medio']), 
                selected = 0
    )
  })
  
  output$graficoWord <- renderWordcloud2({
    
 nube <- count(df(), word, sort = T) %>% 
      wordcloud2(size = 1, color = "random-dark", shape = "circle")
 nube
  })
  
  output$graficoTf_idf = renderPlot({
  
  tf <- ds() %>% top_n(2) %>% ggplot(aes(word, tf_idf, fill = name_medio)) +
    geom_col(show.legend = FALSE) +
    theme_classic() +
    coord_flip()
  tf
  })

}

shinyApp(ui = ui, server = server)
