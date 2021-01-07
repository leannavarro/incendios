library(plotly)
library(shinythemes)
library(treemapify)
library(ggthemes)
library(highcharter)
library(tidyverse)
library(data.table)
library(readxl)
library(sf)
library(wordcloud2)
library(viridis)
library(DT)


options(scipen = 999)


############################## UI #######################################


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel(title= 'Análisis de incendios en Argentina'),
  
  tabsetPanel(
    tabPanel('Mapas',
             navlistPanel('Análisis Por',
                          
                          
                          
                          tabPanel('Cantidad de incendios',
                                   selectInput(inputId = "anio1", 
                                               label = h3("Año:"),
                                               choices = unique(cantidad_mapa$anio),
                                               selected = unique(cantidad_mapa$anio)[1],
                                               multiple = FALSE),
                                   plotOutput('cantidad_incendios', width = 800, height = 700),
                                   helpText(h3("Incendios por provincia y causa")),
                                   dataTableOutput("tabla_cantidad"),
                                   helpText("*Los datos del año 2020 corresponden únicamente a los meses de septiembre y octubre. 
                                            No se cuenta con información relativa a las causas de los mismos.")
                                   
                          ),
                          
                          tabPanel('Superficie afectada',
                                   selectInput(inputId = "anio2", 
                                               label = h3("Año:"),
                                               choices = unique(superficie_mapa$anio),
                                               selected = unique(superficie_mapa$anio)[1],
                                               multiple = FALSE),
                                   plotOutput('superficie_incendios', width = 800, height = 700),
                                   helpText(h3("Incendios por superficie y tipo de vegetación afectada, en hectáreas")),
                                   dataTableOutput("tabla_superficie"),
                                   helpText("*Los datos del año 2020 corresponden únicamente a los meses de septiembre y octubre. 
                                            No se cuenta con información relativa al tipo de vegetación afectada en los mismos.")
                          )
             )
             
    ),
    
    tabPanel('Cobertura mediática',
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Análisis de artículos publicados entre el 27 de agosto y el 26 de octubre de 2020."),
                 selectInput('output_medio',
                             label='Seleccione alcance de los medios',
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
      filter(anio == input$anio1)
  })
  
  
  output$cantidad_incendios <- renderPlot({
    ggplot(df_filt_cant(), mapping =  aes(fill = incendio_total_numero))+
      geom_sf(data = df_filt_cant()) +
      coord_sf(xlim = c(-74, -52), ylim = c(-56, -20))+ # sacamos la antartida que deforma el mapa
      theme_void()+
      scale_fill_viridis(option = "inferno", begin = 0.1, direction = 1)+
      labs(fill = "Cantidad de incendios")
    
  })
  
  
  df_filt_sup <- reactive({
    df_filt_sup <- superficie_mapa %>% 
      filter(anio == input$anio2)
  })
  
  
  
  output$superficie_incendios <- renderPlot({
   ggplot(df_filt_sup(), mapping =  aes(fill = sup_prop))+
      geom_sf(data = df_filt_sup()) +
      coord_sf(xlim = c(-74, -52), ylim = c(-56, -20))+ # sacamos la antartida que deforma el mapa
      theme_void()+
      scale_fill_viridis(option = "inferno", begin = 0.1, direction = 1)+
      labs(fill = "%  de superficie afectada") 

    
  })  
  
  
  df_filt_cant_tabla <- reactive({
    df_filt_cant_tabla <- cantidad %>% 
      filter(anio == input$anio1) 
  })
  
  output$tabla_cantidad <- renderDataTable({
    df_filt_cant_tabla() %>% 
      select(provincia, incendio_total_numero, incendio_negligencia_numero, 
             incendio_intencional_numero, incendio_natural_numero, incendio_desconocida_numero) %>% 
      filter(!incendio_total_numero == 0) %>% 
      rename("Provincia" = provincia, 
             "Total de incendios" = incendio_total_numero, 
             "Negligencia" = incendio_negligencia_numero, 
             "Intencionales" = incendio_intencional_numero,
             "Naturales" = incendio_natural_numero,
             "Desconocida" = incendio_desconocida_numero)
  })
  
  df_filt_sup_tabla <- reactive({
    df_filt_sup_tabla <- superficie %>% 
      filter(anio == input$anio2) 
  })
  
  output$tabla_superficie <- renderDataTable({
    df_filt_sup_tabla() %>% 
      select(provincia, superficie_afectada_por_incendios_total_hectareas,  
             superficie_afectada_por_incendios_bosque_nativo_hectareas,  
             superficie_afectada_por_incendios_bosque_cultivado_hectareas,  
             superficie_afectada_por_incendios_arbustal_hectareas,  
             superficie_afectada_por_incendios_pastizal_hectareas,  
             superficie_afectada_por_incendios_sin_determinar_hectareas,
             sup_prop) %>% 
      filter(!superficie_afectada_por_incendios_total_hectareas == 0) %>% 
      rename("Provincia" = provincia, 
             "Superficie total afectada por incendios (ha)" = superficie_afectada_por_incendios_total_hectareas, 
             "Bosque nativo" = superficie_afectada_por_incendios_bosque_nativo_hectareas, 
             "Bosque cultivado" = superficie_afectada_por_incendios_bosque_cultivado_hectareas,
             "Arbustal" = superficie_afectada_por_incendios_arbustal_hectareas,
             "Pastizal" = superficie_afectada_por_incendios_pastizal_hectareas,
             "Sin determinar" = superficie_afectada_por_incendios_sin_determinar_hectareas,
             "% de superficie afectada" = sup_prop)
    
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
      labs(x = NULL, y = "TF-IDF", title = "Palabras con mayor TF-IDF") +
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
      labs(x = NULL, y = "Cantidad de apariciones", title = "Bigramas con mayor presencia") +
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