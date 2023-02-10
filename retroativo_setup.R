library(tidyverse)#manipulação dos dados
library(openxlsx)#leitura de arquivos xlxs
library(stringi)#manipulação de strings
library(janitor)
library(lubridate)
library(rmarkdown)
source("FUNCTIONS/get_bank_info_function.R")
#----------------------------------------------------------------------------------


path_03 <- "planilhas_excell/Dados bancarios_SEBRAE.xlsx"

#-----------------------------------------------------------------------------------

df_quinzena_01 <- read.xlsx(xlsxFile = path_01,
                            startRow = 10, sheet = 1,
                            detectDates = TRUE,
                            fillMergedCells = TRUE)


df_quinzena_02 <- read.xlsx(xlsxFile = path_02,
                            startRow = 10, sheet = 1,
                            detectDates = TRUE,
                            fillMergedCells = TRUE)


df_empresas_cons <- read.xlsx(xlsxFile = path_03, colNames = TRUE, check.names = TRUE)
#---------------------------------------------------------------------------------------
date_execution <- path_01 %>% 
  str_extract(("(?<=-)(.+)(?=\\-)")) %>% 
  str_trim("both") %>% 
  str_replace(" ", "/" ) %>% 
  paste0("1/",.) %>% 
  as_date(format="%d/%B/%Y")

#-------------------------------------------------------------------------------
df_final <- rbind(df_quinzena_01,df_quinzena_02)

names(df_final) <- make_clean_names(names(df_final))

df_final <- df_final %>% 
  select(-c(total_km, valor_km, adicional, valor_do_reembolso)) %>% 
  drop_na(any_of("nome_do_consultor")) %>% 
  mutate(
    nome_do_consultor = tolower(nome_do_consultor),
    nome_do_consultor = str_trim(nome_do_consultor,side = "both"),
    nome_do_consultor = stri_trans_general(str = nome_do_consultor,id = "Latin-ASCII"),
    data_do_atendimento = format(as.Date(data_do_atendimento),format="%d/%m/%Y"),
    deslocamento_km_percorrido = as.numeric(deslocamento_km_percorrido)
  ) %>%
  group_by(id,nome_do_consultor, data_do_atendimento,codigo_do_produto) %>% 
  summarise(
    total_km = sum(deslocamento_km_percorrido),
    valor = if_else(total_km > 140, total_km*1.19, 167) + 50 
  ) %>% 
  as_tibble()


#----------------------------------------------------------------------------- 

report_complet <- df_final %>% 
  group_by(nome_do_consultor,codigo_do_produto) %>% 
  arrange(data_do_atendimento) %>% 
  nest() %>% 
  arrange(nome_do_consultor)

#------------------------------------------------------------------------------

# getting data from sheets - dados das empresas dos consultores

df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
  select(-Produtos) %>% 
  mutate(
    Nome = tolower(Nome),
    Nome = stri_trans_general(str = Nome, id = "Latin-ASCII"),
    Nome = str_trim(Nome, side = "both")
  ) %>% 
  as_tibble()


#-----------------------------------------------------------------------------

for (i in 1:nrow(report_complet)) {
  
  #
  nome <- str_to_title(report_complet[[1]][[i]])#pegando o nome do consultor
  #
  codigo <- report_complet[[2]][[i]]#pegando o produto
  #
  events <- report_complet[[3]][[i]] %>% 
    select(data_do_atendimento,total_km, valor)#selecionando as colunas de interesse
  #
  total <- sum(events$valor)
  #
  
  #pegando as informações do banco
  
  bank_info  <- tryCatch(get_bank_info(df_empresas_cons, tolower(nome), opt = 0),
                         error = function(e){
                           message(paste("ERRO CONSULTOR:", nome, codigo, sep = " "))
                           return(as.list(rep("ERRO",11)))
                         }) 
  bank_info <- as_vector(bank_info)#apenas para os resultados pararem de serem printados no console
  # e possibilitar a visualização dos consultores que deram erro
  # não mudei isso na funcção get_bank_info pq não sei se isso alteraria 
  # o comportamento dela em outras ocasioes
  
  #
  render("templates/TEMPLATE_RECIBO.Rmd", 
         output_file = paste0("Recibo","-",format(date_execution, "%B-%Y"), nome,"-", codigo,".doc" ),
         output_dir = paste0(format(date_execution, "%B-%Y"))
         
  )
  
  
}

