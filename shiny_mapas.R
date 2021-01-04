library(plotly)
library(shinythemes)
library(treemapify)
library(ggthemes)
library(highcharter)
library(tidyverse)
library(data.table)
library(readxl)
library(sf)

options(scipen = 999)


############################## UI #######################################


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel(title= 'Incendios'),
  
tabsetPanel(
  tabPanel('Mapas',
           navlistPanel('Análisis Por:',
                        tabPanel('Cantidad de incendios',
                                 selectizeInput(inputId = "anio", 
                                             label = "Año:",
                                             choices = unique(cantidad_mapa$anio),
                                             selected = unique(cantidad_mapa$anio)[1],
                                             multiple = FALSE),
                                 plotOutput('cantidad_incendios'),
      
                        
                        ),
                        
                        tabPanel('Superficie afectada',
                                 selectizeInput(inputId = "anio", 
                                                label = "Año:",
                                                choices = unique(superficie_mapa$anio),
                                                selected = unique(superficie_mapa$anio)[1],
                                                multiple = FALSE),
                                 plotOutput('superficie_incendios'),
                        
           )
           )
  ),
           
    tabPanel('Cobertura mediática',
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
                 fluidRow(column(3, plotlyOutput('graficoSentiments')),
                          column(9, highchartOutput('graficoSentimentsMedios'))),
                 br()
               )
             )
    )
)
)


  
############################## SERVER #######################################
  
server <- function(input,output){
  df_filt_cant <- reactive({
    df_filt_cant <- cantidad_mapa %>% 
      filter(anio == input$anio)
  }
    
  )
  
    output$cantidad_incendios <- renderPlot({
      ggplot(df_filt_cant(), mapping =  aes(fill = incendio_total_numero))+
        geom_sf(data = df_filt_cant()) +
        coord_sf(xlim = c(-74, -52), ylim = c(-56, -20))+ # sacamos la antartida que deforma el mapa
        theme_void()+
        scale_fill_viridis_c()
      
    })
    
    
    df_filt_sup <- reactive({
      df_filt_sup <- superficie_mapa %>% 
        filter(anio == input$anio)
    }
    
    )
    
    output$superficie_incendios <- renderPlot({
      ggplot(df_filt_sup(), mapping =  aes(fill = sup_prop))+
        geom_sf(data = df_filt_sup()) +
        coord_sf(xlim = c(-74, -52), ylim = c(-56, -20))+ # sacamos la antartida que deforma el mapa
        theme_void()+
        scale_fill_viridis_c()
      
    })    
    
    
    
    
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
        ggplot(aes(x= word, y= tf_idf)) +
        geom_segment(aes(x=word, xend=word, y=0, yend=tf_idf)) +
        geom_point(size=4, color="red", fill=alpha("orange", 0.4), alpha=0.7, shape=21, stroke=2) +
        labs(x = "Palabra", y = "TF-IDF", title = "Índice de TF-IDF") +
        theme_minimal() +
        coord_flip()
      ggplotly(tf_p) %>% layout(showlegend = FALSE)
      
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
      ggplotly(bigram_p) %>%layout(showlegend = FALSE)
    })
    
    output$graficoSentiments <- renderPlotly({
      bing_p <- ggplot(bing(), aes(sentimiento, n, fill = sentimiento)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Spectral") +
        labs(x = "Sentimiento", y = "Intensidad", title = "Análisis sentimental") +
        theme_minimal()
      ggplotly(bing_p) %>% layout(showlegend = FALSE)
    })
    
    output$graficoSentimentsMedios <- renderHighchart({
      
      nrc_p <- nrc() %>%  hchart("treemap", hcaes(x = sentimiento, value = n, color = n)) %>% 
        hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, begin = 0.2)), 
                     type = "logarithmic")
      nrc_p 
    })
    

}
    
############################## APP #######################################
shinyApp(ui = ui, server = server)