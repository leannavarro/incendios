library(shiny)
library(ggplot2)
library(wordcloud2)
library(tidyverse)

options(scipen = 20)

ui <- fluidPage(
  
  titlePanel("Incendios forestales según los medios de comunicación"),
  sidebarLayout(
    sidebarPanel(
    selectInput('output_medio',
                label=h3('Seleccione alcance de los medios'),
                choices = unique(data_token$scope_medio)
                ),
    
    helpText("Medios:"),
    verbatimTextOutput('medios_names')
    
    ),
    
    mainPanel( 
      wordcloud2Output('graficoWord'),
      br(),
      fluidRow(column(6, plotOutput('graficoTf_idf')),
               column(6, plotOutput('graficoBigrams'))),
      fluidRow(column(4, plotOutput('graficoSentiments')),
               column(8, plotOutput('graficoSentimentsMedios')))
      )
    )
)

server <- function(input, output) {
  
  token <- reactive({
    token = data_token[data_token$scope_medio==input$output_medio,]
    token
  })
  
  tf_df <- reactive({
    tf_df = data_token_group[data_token_group$scope_medio==input$output_medio,]
    tf_df
  })
  
  bigrams <- reactive({
    bigrams = bigrams_united[bigrams_united$scope_medio==input$output_medio,]
    bigrams
  })
  
  nrc <- reactive({
    nrc = sentiments_nrc[sentiments_nrc$scope_medio==input$output_medio,]
    nrc
  })
  
  bing <- reactive({
    bing = sentiments_bing[sentiments_bing$scope_medio==input$output_medio,]
    bing
  })
  
  output$medios_names <- renderText({
    paste0("\n",unique(token()$name_medio))
  })
  
  output$graficoWord <- renderWordcloud2({
    count(token(), word, sort = T) %>% 
      wordcloud2(size = 1, color = "random-dark")
  })
  
  output$graficoTf_idf <- renderPlot({
    tf_df() %>% top_n(5) %>% 
      ggplot(aes(x= word, y= tf_idf, fill = scope_medio)) +
      geom_col(show.legend = FALSE) +
      theme_classic() + 
      coord_flip()
  
  })
  
  output$graficoBigrams <- renderPlot({
    bigrams() %>%  group_by(scope_medio) %>% 
      count(bigram, sort = TRUE) %>% 
      top_n(10) %>% 
      filter(n > 2) %>% 
      ggplot(aes(x = reorder(bigram,n), y = n, fill = scope_medio)) +
      geom_col(show.legend = F) +
      theme_classic() +
      coord_flip()
  })
  
  output$graficoSentiments <- renderPlot({
   bing() %>% 
      ggplot(aes(reorder(sentimiento, n), n, fill = sentimiento)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_brewer(palette = "Spectral") +
      theme_minimal()
  })
  
  output$graficoSentimentsMedios <- renderPlot({
    nrc() %>% 
      ggplot(aes(reorder(sentimiento, n), n, fill = sentimiento)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_brewer(palette = "Spectral") +
      theme_minimal() +
      facet_wrap(~ name_medio, ncol = 2, scales = "free_x") +
      coord_flip()
  })

}

shinyApp(ui = ui, server = server)
