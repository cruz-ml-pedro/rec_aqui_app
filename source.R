library(plotly) # ser? ?til para fazer nossos gr?ficos
library(dplyr) # manipula??o de dados
library(wordcloud2) # construir o gr?fico nuvem de palavras
library(readr) # biblioteca para ler os dados
library(reactable)
library(widyr)
library(igraph)
library(ggraph)
library(stringr)
library(stopwords)
library(tidytext)
library(tidyr)
library(geobr)
library(leaflet)
library(htmltools)
library(scales)
library(scico)
#configuração do layout das tabelas
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))# tema para as tabelas




menu_comparacao<-c("Índice de Solução"="indice_solucao",
                   "Não respondidas"="nao_respondidas",
                   "Qnt. Avaliadas"="avaliadas",
                   "Nota RA"="nota",
                   "Nota do Consumidor"="nota_consumidor",
                   "% que voltaria"="percent_voltariam",
                   "Qnt. Reclamações"="qnt_reclamacoes")

#     #     #     #     #     #

menu_empresas <- c("Total",
                   "FinanZero",
                   "Banco BV",
                   "Creditas",
                   "Inter",
                   "meutudo.",
                   "Banco Bmg",
                   "Safra Financeira (Veículos e Consignados)",
                   "Banco PAN" 
)
############################################################
estado2 <- c("Brasil",
             "AC",
             "AL",
             "AP",
             "AM",
             "BA",
             "CE",
             "DF",
             "ES",
             "GO",
             "MA",
             "MT",
             "MS",
             "MG",
             "PA",
             "PB",
             "PR",
             "PE",
             "PI",
             "RJ",
             "RN",
             "RS",
             "RO",
             "RR",
             "SC",
             "SP",
             "SE",
             "TO")




estado <- c("AC",
            "AL",
            "AP",
            "AM",
            "BA",
            "CE",
            "DF",
            "ES",
            "GO",
            "MA",
            "MT",
            "MS",
            "MG",
            "PA",
            "PB",
            "PR",
            "PE",
            "PI",
            "RJ",
            "RN",
            "RS",
            "RO",
            "RR",
            "SC",
            "SP",
            "SE",
            "TO")

pop <- c(906876,
         3365351,
         877613,
         4269995,
         14985284,
         9240580,
         3094325,
         4108508,
         7206589,
         7153262,
         3567234,
         2839188,
         21411923,
         8777124,
         4059905,
         11597484,
         9674793,
         3289290,
         17463349,
         3560903,
         11466630,
         1815278,
         652713,
         7338473,
         46649132,
         2338474,
         1607363)
states_pop <- tibble(estado,pop) 

#     #     #     #     #     #
info_basicas<- read_delim("info_basicas.txt",
                          "\t",
                          escape_double = FALSE,
                          trim_ws = TRUE)

info_basicas <- info_basicas %>% 
  mutate(
    percent_voltariam = str_trim(percent_voltariam, "both"),
    percent_voltariam = as.numeric(percent_voltariam)
  )

#     #     #     #     #     #
info_reclamacoes_avaliadas <- read_delim("info_reclamacoes_avaliadas.txt",
                                         "\t",
                                         escape_double = FALSE,
                                         trim_ws = TRUE)




info_reclamacoes_avaliadas  <- info_reclamacoes_avaliadas  %>%
  dplyr::mutate(reclamacao_id = row_number())

avaliadas_token<-info_reclamacoes_avaliadas  %>% 
  select(titulo_reclamacao,reclamacao_id) %>% 
  unnest_tokens(word,titulo_reclamacao) 

stop_words_pt<-data.frame(word=(stopwords(language = "pt")))

avaliadas_token <- avaliadas_token %>% 
  anti_join(stop_words_pt)

avaliadas_token<- avaliadas_token %>%  
  filter(sapply(avaliadas_token$word,nchar)>2)

#juntar o status da reclama??o no conjunto de dados com as palavras

avaliadas_token <-left_join(avaliadas_token,
                            info_reclamacoes_avaliadas[,c("reclamacao_id","status_reclamacao")])

analise_geral_palavra<-avaliadas_token %>%
  group_by(word) %>%
  dplyr::mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
                qnt_palavra = n()) %>%
  select(word,percentual_solucao,qnt_palavra) %>%
  unique()%>%
  ungroup()

#filtrar apenas os resultados com frequencia maior que 50
# analise_palavras <- analise_palavras %>%
#   dplyr::filter(qnt_palavra >5)

avaliadas_token_empresa <-left_join(avaliadas_token,
                                    info_reclamacoes_avaliadas[,c("reclamacao_id","empresa")])

analise_empresas <-avaliadas_token_empresa %>%
  group_by(empresa) %>%
  dplyr::mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n()) %>%
  select(percentual_solucao,empresa) %>%
  unique() %>%
  ungroup()

analise_empresa_palavras <-avaliadas_token_empresa %>%
  group_by(word,empresa) %>%
  dplyr::mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
                qnt_palavra = n()) %>%
  select(word,percentual_solucao,qnt_palavra,empresa) %>%
  unique()%>%
  ungroup()

#?
qnt_palavras_empresa <- analise_empresa_palavras %>%
  select(-percentual_solucao)

#?
qnt_palavras_geral <- analise_geral_palavra %>%
  select(-percentual_solucao ) %>%
  mutate(qnt_palavra = qnt_palavra/8)

##########################################
states.df <- read_state(year=2019)