library(shiny)
source("source.R")



ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "cyborg"),
  
  tabsetPanel(
    
    # início tabsePanel1----
    tabPanel(
      "Qnt/corr-Palavras",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          selectInput(inputId = "empresa_palavra", 
                      label = "Opt - grupo palavras", 
                      choices = menu_empresas,
                      width = "100%"
          ),
          tableOutput(
            "infos_b"
          )
          
        ), # fim sidebarpanel/tabset1----
        
        # início mainPanel/taset1----
        mainPanel(
          width = 9,
          wordcloud2Output(
            'wordcloud2',
            width = "100%"
          ),
          hr(style = "border-top: 8px solid #E3EEF4FF;"),
          h4("Tabela n° palavras"),
         reactableOutput(
           outputId = "palavras_table",
            width = "auto"
          ),
         hr(style = "border-top: 8px solid #E3EEF4FF;"),
         h4("Correlação entre palavras"),
        plotOutput(
           outputId = "corr_palavras",
           width = "100%"
           )
        # fim do mainpaneil/tabset01----
      )
      )# fim do sidebarlayout/tabset1----
    ),# fim do tabpanel 1----
    tabPanel("Comparação geral",# segunda aba/tabpanel02----
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 #o comando selectInput ir? definir o nosso menu de m?ltipla escolha (v?rias op??es e s? uma ser? escolhida)
                 selectInput(inputId = "comparacao_v1", 
                             label = "Quais resultados você quer comparar", 
                             choices = menu_comparacao,
                             width = "100%"
                             ),
                 selectInput(inputId = "comparacao_v2", 
                             label = "Quais resultados você quer comparar?", 
                             choices = menu_comparacao,
                             selected = menu_comparacao[2],
                             width = "100%"
                             ),
                 
                 hr(style = "border-top: 8px solid #E3EEF4FF;"),
                 
                 selectInput(inputId = "relacao_empresa", 
                             label = "Qual empresa você deseja avaliar?", 
                             choices = info_basicas$empresa,
                             width = "100%")
               ), # fim sidebar/início main panel segunda aba----
               mainPanel(
                 width = 9,
                 # chamando o output do gráfico de comparação entre as empresas
                 h2("Comparação empresas"),
                 plotly::plotlyOutput(outputId = "comparacao_graf", width = "100%"),
                 
                 h2("Pontos fortes e fracos"),
                 plotly::plotlyOutput(outputId = "relacao_graf_maiores", width = "100%"),
                 plotly::plotlyOutput(outputId = "relacao_graf_menores", width = "100%"),
                 
                 h2("Palavras que mais aparecem"),
                 plotly::plotlyOutput(outputId = "mais_aparecem_graf", width = "100%")
               )
             )# fim do sidebarlayout segunda aba----
      
    ),# fim do tabpanel2----
    
    tabPanel("Quantidade rec por região e genero",
             sidebarLayout(
               sidebarPanel(width = 3,
                 selectInput(inputId = "empresa_por_local", 
                             label = "Qual empresa você deseja avaliar?", 
                             choices = menu_empresas,
                             width = "100%"
                 ),
                 selectInput(inputId = "gender", 
                             label = "Escolha o Gênero", 
                             choices = c(
                               "Total" = "total",
                               "Homem" = "Male",
                               "Mulher" = "Female"
                               ),
                             width = "100%"
                 ),
                 
                 hr(style = "border-top: 8px solid #E3EEF4FF;"),
                 
                 selectInput(inputId = "local", 
                             label = "Escolha o estado", 
                             choices = estado2,
                             width = "100%"
                 )
               ), # fim do sidebar/terceira aba----
               mainPanel(
                 width = 9,
                 h2("Localização das reclamações"),
                leaflet::leafletOutput(
                   "mapa",
                    width = "100%"
                   )
               )
             )
             
             )# fim do tabpanel 3----
    
  )# fim do tabSetpanel
  
)# fim do fluidpage


server <- function(input, output) {
  
  # Primeira aba----
  
  # Tabela info sidebar----
info_b_emp <-  reactive({
  info_basicas %>% 
    filter(empresa == input$empresa_palavra) %>% 
    select(1,2,6,7,9,10) %>%
    mutate(
      qtn_problema = paste(qtn_problema, "%", sep = "")
    ) %>% 
    rename(
      "% Problema" = qtn_problema
    ) %>% 
    as_tibble()
  })
  
# plot Tabela info sidebar
  output$infos_b <- renderTable(
    
  t(info_b_emp()) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    as_tibble(),
  colnames = FALSE
    
  )
  
  # GRÁFICO - NuVEM DE PALAVRAS----
  # dados para o gráfico
  wordcloud_dados <- reactive({
    
    if(input$empresa_palavra == "Total"){
      
      analise_geral_palavra %>%
        select(word,qnt_palavra) %>% 
        dplyr::rename(freq = qnt_palavra)
      
    } else {
      
      analise_empresa_palavras %>%
        filter(empresa == input$empresa_palavra) %>% 
        select(word,qnt_palavra) %>% 
        dplyr::rename(freq = qnt_palavra)
    }
    
  })
  
  #criação do gráfico
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(data = wordcloud_dados())
  })
  
  # Tabela com as quantidades de palavras----
  output$palavras_table <- renderReactable({
    
    reactable(
      wordcloud_dados() %>% 
        arrange(-freq) %>% 
        as_tibble(),
      searchable = TRUE
    )
    
  })
  
 # Gráfico com a corrlação entre as palavras----
 output$corr_palavras <- renderPlot({
   
   
   if(input$empresa_palavra == "Total"){
     
     correlacao <- avaliadas_token %>%
       group_by(word) %>% 
       dplyr::filter(n() > 5) %>%
       pairwise_cor(word, reclamacao_id, sort = TRUE,upper=F)%>%
       ungroup()
     
   } else{
     
     correlacao <- avaliadas_token_empresa %>%
       group_by(word) %>% 
       dplyr::filter(empresa == input$empresa_palavra & n() > 5) %>% 
       #dplyr::filter(n() > 5) %>%
       pairwise_cor(word, reclamacao_id, sort = TRUE,upper=F)%>%
       ungroup()
     
   }
   
   
   correlacao <- correlacao %>% 
     filter(correlation >0.3)
  
    
   correlacao %>%
      arrange(-correlation) %>%
      top_n(30) %>% #Filtrar as 10 maiores
      graph_from_data_frame() %>%
      ggraph(layout = 'fr') + 
      guides(edge_alpha = "none", edge_width = "none") +
      scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
      geom_node_point(color = 'lightblue', size = 5) + 
      geom_node_text(aes(label = name), repel = TRUE) + 
      theme_graph() +
      labs(title = "Palavras que geralmente apareceram juntas")
  
  
})
 
 # Grafico de comparação entre as empresas----
 output$comparacao_graf <- plotly::renderPlotly({
   
   #definindo detalhes do gráfico
   ay <- list(
     overlaying = "y",
     side = "right"
   )
   
   # Criando o gráfico
   plot_ly(data = info_basicas) %>%
     ### Vari?vel 1
     add_lines(x = ~empresa, y = ~get(input$comparacao_v1), name = names(menu_comparacao)[menu_comparacao==input$comparacao_v1]) %>%
     ### vari?vel 2
     add_lines(x = ~empresa, y = ~get(input$comparacao_v2), name = names(menu_comparacao)[menu_comparacao==input$comparacao_v2], yaxis = "y2") %>%
     layout(
       font = list(color = "black"),
       title = "Comparação entre empresas", 
       yaxis = list(title=""),
       yaxis2 = ay,
       xaxis = list(title="", tickangle = 60)
     )
 })
 
# Relação entre palavras e solução da reclamação----
 
 relacao_dados <- reactive({
   #filtrar pela empresa escolhida
   relacao_palavra_empresa <- analise_empresa_palavras %>%
     filter(empresa == input$relacao_empresa & qnt_palavra >3) %>% 
     select(-qnt_palavra)
     
   #filtrar pelos MAIORES % de solução
   relacao_palavra_empresa_maiores <- relacao_palavra_empresa %>%
     slice_max(n = 5,order_by = percentual_solucao,with_ties=F)
   
   #filtrar pelos MENORES % de solução
   relacao_palavra_empresa_menores <- relacao_palavra_empresa %>%
     slice_min(n = 5,order_by = percentual_solucao,with_ties=F)
   
   #Considerando todas as empresas
   relacao_palavra_geral<- analise_geral_palavra %>%
     filter(qnt_palavra >5) %>% 
     mutate(empresa = "geral") %>%
     select(-qnt_palavra)
   
   #filtrando as mesmas palavras no relacao_palavra_geral e relacao_palavra_empresa_maiores
   relacao_palavra_geral_maiores <-
     relacao_palavra_geral[relacao_palavra_geral$word %in%
                             relacao_palavra_empresa_maiores$word,]
   
   #filtrando as mesmas palavras no relacao_palavra_geral e relacao_palavra_empresa_menores
   relacao_palavra_geral_menores <-
     relacao_palavra_geral[relacao_palavra_geral$word %in%
                             relacao_palavra_empresa_menores$word,]
   
   
   ### resultado final: juntar o resultado geral + empresa
   concatenado_maiores <-  data.frame(rbind(relacao_palavra_empresa_maiores,relacao_palavra_geral_maiores))
   
   # Transformando os dados para o formato wide
   # veja explica??o da transforma??o long -> wide no final dessa p?gina
   concatenado_maiores <- reshape(concatenado_maiores, 
                                  idvar = "word", timevar = "empresa", direction = "wide")
   
   # Transformando os dados para o formato wide
   # veja explica??o da transforma??o long -> wide no final dessa p?gina
   concatenado_menores <- data.frame(rbind(relacao_palavra_empresa_menores,relacao_palavra_geral_menores))
   concatenado_menores <- reshape(concatenado_menores, 
                                  idvar = "word", timevar = "empresa", direction = "wide")
   
   
   #colocando nomes genéricos para as colunas
   names(concatenado_maiores) <-c("palavra",
                                  "percentual_loja",
                                  "percentual_geral")
   names(concatenado_menores) <-c("palavra",
                                  "percentual_loja",
                                  "percentual_geral")
   #output da função
   list(concatenado_maiores,concatenado_menores)
 })
 
 ## Gráfico relação entre palavras do título e solução
 ## cria??o do gr?fico com os MAIORES %
 
 output$relacao_graf_maiores <- plotly::renderPlotly({
   plot_ly(data = relacao_dados()[[1]],x = ~palavra, y = ~percentual_loja, type = 'bar', name = '% Empresa') %>%
     add_trace(y = ~percentual_geral, name = '% geral') %>%
     layout(title = "Pontos Fortes da Empresa",
            yaxis = list(title = '% Casos Resolvidos'),
            xaxis = list(title = ''),barmode = 'group')
   
 })
 
 ## criação do gráfico com os MENORES %
 
 output$relacao_graf_menores <- plotly::renderPlotly({
   
   plot_ly(data = relacao_dados()[[2]],x = ~palavra, y = ~percentual_loja, type = 'bar', name = '% Empresa') %>%
     add_trace(y = ~percentual_geral, name = '% geral') %>%
     layout(title = "Pontos Fracos da Empresa",
            yaxis = list(title = '% Casos Resolvidos'),
            xaxis = list(title = ''),barmode = 'group')        
 })
 
 
 ####### Dados - Palavras que mais aparecem
 mais_aparecem <-reactive({
   # filtrar por empresa que o usu?rio escolheu
   qnt_palavras_empresa_filtrado <- qnt_palavras_empresa %>%
     filter(empresa == input$relacao_empresa & qnt_palavra >5) %>%
     slice_max(n = 10,order_by = qnt_palavra,with_ties=F) 
   
   # Filtrar as mesmas palavras em qnt_palavras_geral e qnt_palavras_empresa_filtrado
   qnt_palavras_geral <- 
     qnt_palavras_geral[qnt_palavras_geral$word %in%
                          qnt_palavras_empresa_filtrado$word,]
   qnt_palavras_geral$empresa<-"geral"
   
   concatenado_palavras <-  data.frame(rbind(qnt_palavras_empresa_filtrado,qnt_palavras_geral))
   
   # Transformando os dados para o formato wide
   
   concatenado_palavras <- reshape(concatenado_palavras, 
                                   idvar = "word", timevar = "empresa", direction = "wide")
   names(concatenado_palavras)<-c("palavra","qnt_loja","qnt_geral")
   concatenado_palavras
 })
 
 ### Criação do gráfico - Palavras mais usadas nas reclamações da Empresa
 output$mais_aparecem_graf <- plotly::renderPlotly({
   
   plot_ly(data = mais_aparecem(),x = ~qnt_loja, y = ~palavra, 
           type = 'bar', name = 'Quantidade Empresa', orientation = 'h') %>%
     add_trace(x = ~qnt_geral, name = 'Quantidade geral') %>%
     layout(title = "Palavras mais usadas nas reclamações da Empresa",
            yaxis = list(title = ''),
            xaxis = list(title = 'Quantidade de vezes'),barmode = 'group')        
 })
 
 # Mapas----
 
 states<- reactive({
   
   if(input$local == "Brasil"){
   
   if(input$empresa_por_local == "Total" & input$gender == "total"){
   
 df <- info_reclamacoes_avaliadas %>%
     select(1,4,6,7) %>% 
     group_by(estado) %>%
     filter(estado != "--") %>%  
     summarise(
       total = n()
     ) %>% 
     arrange(-total) %>% 
     left_join(.,states_pop, by = "estado") %>% 
     mutate(
       proporcao = (total/pop)*100000
     ) %>% 
     rename(
       abbrev_state = estado
     )
     
   } else if(input$empresa_por_local != "Total" & input$gender == "total") {
     
  df  <-  info_reclamacoes_avaliadas %>%
       select(1,4,6,7) %>% 
       group_by(estado,empresa) %>%
       filter(estado != "--" & empresa == input$empresa_por_local) %>%  
       summarise(
         total = n()
       ) %>% 
       arrange(-total) %>% 
       left_join(.,states_pop, by = "estado") %>% 
       mutate(
         proporcao = (total/pop)*100000
       ) %>% 
       rename(
         abbrev_state = estado
       )
     
   } else if(input$empresa_por_local == "Total" & input$gender != "total"){
     
  df  <-  info_reclamacoes_avaliadas %>%
       select(1,4,6,7) %>% 
       group_by(estado,gender) %>%
       filter(estado != "--" & gender == input$gender) %>%  
       summarise(
         total = n()
       ) %>% 
       arrange(-total) %>% 
       left_join(.,states_pop, by = "estado") %>% 
       mutate(
         proporcao = (total/pop)*100000
       ) %>% 
       rename(
         abbrev_state = estado
       )
   }else{
     
  df  <-  info_reclamacoes_avaliadas %>%
       select(1,4,6,7) %>% 
       group_by(estado,empresa,gender) %>%
       filter(estado != "--") %>%  
       summarise(
         total = n()
       ) %>% 
       filter(gender == input$gender & empresa == input$empresa_por_local & gender == input$gender) %>% 
       arrange(-total) %>% 
       left_join(.,states_pop, by = "estado") %>% 
       mutate(
         proporcao = (total/pop)*100000
       ) %>% 
       rename(
         abbrev_state = estado
       )
   } 
     
     states<-states.df%>% dplyr::left_join(x = ., 
                                           y = df, 
                                           by = "abbrev_state")
     
     states
   }else{ #fim do if mais externo
     
     
     if(input$empresa_por_local == "Total" & input$gender == "total"){
       
       df <- info_reclamacoes_avaliadas %>%
         select(1,4,6,7) %>% 
         group_by(estado,cidade) %>%
         filter(estado != "--" & estado == input$local) %>%  
         summarise(
           total = n()
         ) %>% 
         arrange(-total) %>% 
         rename(
           name_muni = cidade
         )
       
     } else if(input$empresa_por_local != "Total" & input$gender == "total") {
       
       df  <-  info_reclamacoes_avaliadas %>%
         select(1,4,6,7) %>% 
         group_by(estado,cidade,empresa) %>%
         filter(estado != "--" & estado == input$local & empresa == input$empresa_por_local) %>%  
         summarise(
           total = n()
         ) %>% 
         arrange(-total) %>% 
         rename(
           name_muni = cidade
         )
       
     } else if(input$empresa_por_local == "Total" & input$gender != "total"){
       
       df  <-  info_reclamacoes_avaliadas %>%
         select(1,4,6,7) %>% 
         group_by(estado,cidade,gender) %>%
         filter(estado != "--" & estado == input$local & gender == input$gender) %>%  
         summarise(
           total = n()
         ) %>% 
         arrange(-total) %>% 
         rename(
           name_muni = cidade
         )
     }else{
       
       df  <-  info_reclamacoes_avaliadas %>%
         select(1,4,6,7) %>% 
         group_by(estado,cidade,empresa,gender) %>%
         filter(estado != "--" & estado == input$local) %>%  
         summarise(
           total = n()
         ) %>% 
         filter(gender == input$gender & empresa == input$empresa_por_local & gender == input$gender) %>% 
         arrange(-total) %>% 
         rename(
           name_muni = cidade
         )
     } 
     
     
    muni.df <- read_municipality(code_muni = input$local)
     
    states<-muni.df%>% dplyr::left_join(x = ., 
                                          y = df, 
                                          by = "name_muni")
     
   }
   
  states
 })
 


output$mapa <- renderLeaflet({
  

if(input$local == "Brasil"){
  pal <- colorFactor(palette = scico(10, 
                                         palette = "lajolla")[3:7],
                     domain =states()$proporcao )
  #pal <- colorBin("viridis", domain = states()$proporcao, bins = 7)
  #pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  
   labels <- sprintf(
     "%s %f",
     states()$abbrev_state, states()$proporcao) %>% 
     lapply(HTML)
  
  leaflet(states()) %>%
     addPolygons(
       # fill
       fillColor   = ~pal(proporcao),
       fillOpacity = 0.7,
       # line
       dashArray   = "3",
       weight      = 2,
       color       = "white",
       opacity     = 1,
       # interaction
       highlight = highlightOptions(
         weight = 5,
         color = "#666",
         dashArray = "",
         fillOpacity = 0.7,
         bringToFront = TRUE),
       label = labels,
       labelOptions = labelOptions(
         style = list("font-weight" = "normal", padding = "3px 8px"),
         textsize = "15px",
         direction = "auto")) %>%
     addLegend(
       pal = pal, values = ~proporcao, opacity = 0.7, title = HTML("Proporção"),
       position = "bottomright") 
    
  } else {
    
    
    pal <- colorFactor(palette = scico(10, 
                                       palette = "lajolla")[3:7],
                       domain =states()$proporcao )
    #pal <- colorBin("viridis", domain = states()$total, bins = 7)
    #pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
    
    labels <- sprintf(
      "%s %f",
      states()$name_muni, states()$total) %>% 
      lapply(HTML)
    
    leaflet(states()) %>%
      addPolygons(
        # fill
        fillColor   = ~pal(total),
        fillOpacity = 0.7,
        # line
        dashArray   = "3",
        weight      = 2,
        color       = "white",
        opacity     = 1,
        # interaction
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        pal = pal, values = ~total, opacity = 0.7, title = HTML("Total"),
        position = "bottomright")
    
    
    
    
  }
  
}) 
  

 
}


shinyApp(ui = ui, server = server)