extract_info_by_page <- function(page_content, link_loja){
  
  codigo<- read_html(page_content)
  
  # Busca o nome da empresa
  empresa <- codigo %>% 
    html_nodes(".short-name") %>%
    html_text()
  
  # Busca a nota da empresa   
  nota <- codigo %>% 
    html_nodes(".score b") %>%
    html_text()
  
  # Busca a quantidade de reclama??es recebidas pela empresa
  qnt_reclamacoes <- codigo %>% 
    html_nodes("a:nth-child(1) .stats") %>%
    html_text()
  
  # Busca o % de reclama??es respondidas pela empresa
  percent_resp <- codigo %>% 
    html_nodes(".bar-container:nth-child(2) .label") %>%
    html_text()
  
  # Busca o % de clientes que voltariam a fazer neg?cio com a empresa
  percent_voltariam <- codigo %>% 
    html_nodes(".bar-container:nth-child(4) .label") %>%
    html_text()
  
  # Busca o ?ndice de reclama??es solucionadas pela empresa
  indice_solucao <- codigo %>% 
    html_nodes(".bar-container:nth-child(6) .label") %>%
    html_text()
  
  # Busca a nota do consumidor para a empresa
  nota_consumidor <- codigo %>% 
    html_nodes(".bar-container:nth-child(8) .label") %>%
    html_text()
  
  # Busca a quantidade de reclama??es n?o respondidas
  nao_respondidas <- codigo %>% 
    html_nodes(".col-sm-6:nth-child(1) b") %>%
    html_text()
  
  # Busca a quantidade de reclama??es avaliadas
  avaliadas <- codigo %>% 
    html_nodes(".col-sm-6+ .col-sm-6 b") %>%
    html_text()
  
  percent_problema <- codigo %>% 
    html_node("#oi+ .false a") %>% 
    html_text()
  
 separa <-  str_split(percent_problema, "%") 
  
 qtn_problema <- separa[[1]][1] %>% 
   as.numeric()
 
  principal_problema <- separa[[1]][2] %>% 
    str_trim(side = "both")
 
  tabela_empresa <- data.frame(empresa,indice_solucao,nao_respondidas,
                               avaliadas,nota,nota_consumidor,percent_voltariam,
                               qnt_reclamacoes,principal_problema,qtn_problema,link_loja)
  
  return(tabela_empresa)
  
}

###############################################################
extract_info_complaint <- function(page_content2, link_pagina){
  
  
  #ler todo o c?digo da p?gina acessada
  codigo_reclamacoes<- read_html(page_content2)
  
  #buscar o nome da empresa
  empresa<-codigo_reclamacoes %>% 
    html_nodes(".short-name") %>%
    html_text()
  
  #buscar o t?tulo da recla??o
  titulo_reclamacao<-codigo_reclamacoes %>% 
    html_nodes(".fTrwHU") %>%
    html_text()
  
  #buscar o status da reclama??o
  status_reclamacao<-codigo_reclamacoes %>% 
    html_nodes(".sc-1pe7b5t-4") %>%
    html_text()
  
   #buscar h? quanto tempo a reclama??o foi feita
  tempo_reclamacao<-codigo_reclamacoes %>% 
    html_nodes(".hIOzx") %>%
    html_text()
  
  local_links<- codigo_reclamacoes %>%
    html_nodes(".bJdtis a")%>%
    html_attr("href")%>%  
    paste('https://www.reclameaqui.com.br/empresa',.,sep= "")
  
  
   #teste l?gico para o looping n?o parar quando der algum bug
   teste_logico <- length(empresa)>0 & length(titulo_reclamacao)>0 &
     length(status_reclamacao)>0 & length(tempo_reclamacao)>0 & length(local_links)>0


   info_pagina<-data.frame(empresa,titulo_reclamacao,
                        status_reclamacao,tempo_reclamacao,
                        link_pagina,local_links)

  return(c(info_pagina, teste_logico))
}


################################################################
library(tidyverse)
library(RSelenium)
library(stringr)
library(rvest)
library(xml2)

shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
Sys.sleep(5)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
Sys.sleep(15)
remDr$open()


link_lojas<-c("https://www.reclameaqui.com.br/empresa/banco-pan/",
              "https://www.reclameaqui.com.br/empresa/safra-financiamento-de-veiculos/",
              "https://www.reclameaqui.com.br/empresa/banco-bmg/",
              "https://www.reclameaqui.com.br/empresa/meu-tudo-financeira/",
              "https://www.reclameaqui.com.br/empresa/inter/",
             # "https://www.reclameaqui.com.br/empresa/facilita-credito/",
              "https://www.reclameaqui.com.br/empresa/creditas/",
              "https://www.reclameaqui.com.br/empresa/bv-financeira/",
              #"https://www.reclameaqui.com.br/empresa/martins-assessoria/",
              "https://www.reclameaqui.com.br/empresa/finanzero/")

info_basicas<-data.frame()


for(link_loja in link_lojas){ # O objeto link_loja ir? assumir os valores do objeto link_lojas (cada vez ir? assumir 1 deles)
  
  remDr$navigate(link_loja) #Acessar o link
  page_content <- remDr$getPageSource()[[1]]
  tabela_empresas <- extract_info_by_page(page_content, link_loja)
  
    # Junta todas as informa??es em uma tabela
  
  
  # Junta a tabela acima com os dados j? salvos de outras empresas
  info_basicas <-rbind(tabela_empresas,info_basicas)
  
}

################################################


link_reclamacoes<-paste0(link_lojas,"lista-reclamacoes/")

info_reclamacoes_avaliadas <-data.frame()

# looping para mudar a loja
for(link_pagina in link_reclamacoes){
  
  #looping para mudar a p?gina
  for(pagina in 1:10){
     teste_logico <-FALSE
     while(teste_logico == FALSE){
      
      #entrar na p?gina de reclama??es definida pelo objeto 'pagina'.
      remDr$navigate(paste0(link_pagina,"?pagina=",pagina,"&status=EVALUATED"))
      
      page_content2  <- remDr$getPageSource()[[1]]
      
      info_pagina <- extract_info_complaint(page_content2,link_pagina)
      
      teste_logico <- info_pagina[7]
    
      info_reclamacoes_avaliadas<-rbind(info_pagina[c(1:6)],info_reclamacoes_avaliadas)
   }
  }

}
