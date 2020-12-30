library(shiny)
library(ggplot2)
library(wordcloud2)
library(tidyverse)
library(plotly)

options(scipen = 20)

ui <- fluidPage(
  theme = shinytheme("readable"),
  
  titlePanel("Incendios forestales según los medios de comunicación", windowTitle = "ShinyApp: Incendios forestales"),
  sidebarLayout(
    sidebarPanel(
    
      helpText("Análisis de artículos publicados entre el 27 de agosto y el 26 de octubre de 2020."),
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
      fluidRow(column(6, plotlyOutput('graficoTf_idf')),
               column(6, plotlyOutput('graficoBigrams'))),
      
    fluidRow(column(6, helpText("El TF-IDF mide la importancia de una palabra, 
    teniendo en cuenta la frecuencia con la que esa misma 
             palabra aparece en los artículos del resto de los medios."))),
      br(),
      fluidRow(column(4, plotlyOutput('graficoSentiments')),
               column(8, plotlyOutput('graficoSentimentsMedios')))
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
  
  output$graficoTf_idf <- renderPlotly({
   tf_p <-  tf_df() %>% top_n(5) %>% 
      ggplot(aes(x= word, y= tf_idf, fill = scope_medio)) +
      geom_col() +
      labs(x = "Palabra", y = "TF-IDF", title = "Índice de TF-IDF") +
      theme_minimal() + 
      coord_flip()
   ggplotly(tf_p) %>%  layout(showlegend = FALSE)
  
  })
  
  output$graficoBigrams <- renderPlotly({
    bigram_p <- bigrams() %>%  group_by(scope_medio) %>% 
      count(bigram, sort = TRUE) %>% 
      top_n(10) %>% 
      filter(n > 2) %>% 
      ggplot(aes(x = reorder(bigram,n), y = n, fill = scope_medio)) +
      geom_col() +
      labs(x = "Bigramas", y = "Cantidad de apariciones", title = "Bigramas con mayor presencia") +
      theme_minimal() +
      coord_flip()
    ggplotly(bigram_p) %>%  layout(showlegend = FALSE)
  })
  
  output$graficoSentiments <- renderPlotly({
   bing_p <- ggplot(bing(), aes(sentimiento, n, fill = sentimiento)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = "Sentimiento", y = "Intensidad", title = "Sentimiento por alcance de medios") +
      theme_minimal()
   ggplotly(bing_p) %>%  layout(showlegend = FALSE)
  })
  
  output$graficoSentimentsMedios <- renderPlotly({
    nrc_p <- ggplot(nrc(), aes(reorder(sentimiento, n), n, fill = sentimiento)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Spectral") +
      theme_minimal() +
      labs(x= NULL, y = "Intensidad", title = "Presencia de sentimientos por medio") +
      facet_wrap(~ name_medio, ncol = 2, scales = "free_x") +
      coord_flip()
    ggplotly(nrc_p) %>%  layout(showlegend = FALSE)
  })

}

shinyApp(ui = ui, server = server)
