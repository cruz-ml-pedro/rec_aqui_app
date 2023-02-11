library(tidyverse)
library(RSelenium)
library(stringr)
library(rvest)
library(xml2)
library(genderBR)

shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
Sys.sleep(5)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
Sys.sleep(15)
remDr$open()



local_links8 <- df_link$local_links[-c(1:722)] %>% 
  as.character()


df_reclamacao8 <- data.frame()


for (local_link in local_links8) {
  Sys.sleep(40)
  remDr$navigate(local_link)
  Sys.sleep(15)
  page_content <- remDr$getPageSource()[[1]]
  Sys.sleep(15)
  codigo <- read_html(page_content)
  Sys.sleep(15)
  # local <- codigo %>%
  #   html_elements("li.ng-binding:nth-child(1)")%>%
  #   html_text() %>% 
  #   str_trim(side = "both")
  
  titulo <- codigo %>% 
    html_elements("h1") %>% 
    html_text()
  ##################################################################
  info_rec <- codigo %>% 
    html_elements("#complain-detail .container .complain-head ul.local-date li") %>% 
    html_text()
  
  local_rec <- info_rec[1]
  
  data_rec <- info_rec[3]
  
  ######################################################################### 
  # data_resp <- codigo %>% 
  #   html_elements("#complain-detail .container .business-reply .header-date p.date") %>% 
  #   html_text()
  
  ###################################################
  resp_empresa <- codigo %>% 
    html_elements("#complain-detail .container .business-reply p") %>% 
    html_text()
  
  
   data_resp  <- resp_empresa[2]
  
   nome <- resp_empresa[3] %>%
     str_split(pattern = " ")
     
     
  nome_rec  <- nome[[1]][1:4] %>% 
      str_c(collapse = " ")
     
  ####################################################################
   
  df_final <- data.frame(titulo,local_rec, data_rec, data_resp, nome_rec) 
  
  df_reclamacao8 <- rbind(df_final,df_reclamacao8 )
}



 df_reclamacao %>%
      separate(local_rec, sep = " - ", c("cidade","estado")) %>% 
       separate(data_rec, sep = " às", c("data_rec", "hora_rec")) %>% 
       separate(data_resp, sep = " às", c("data_resp", "hora_resp")) %>% 
  mutate(
    nome_rec = str_remove(nome_rec, "!"),
    nome_rec = str_remove(nome_rec, "Esperamos"),
    gender = get_gender(nome_rec)
  )
  
  


  #mudar o modo de pegar o nome das pessoas



