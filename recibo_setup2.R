library(tidyverse)#manipulação dos dados
library(openxlsx)#leitura de arquivos xlxs
library(stringi)#manipulação de strings
library(lubridate)
library(rmarkdown)
source("FUNCTIONS/get_bank_info_function.R")

#nova pasta para set de 2021 - como padronizar?
#-----------------------------------------------------------------------------------------------

path <- "planilhas_excell/Dados bancarios_SEBRAE.xlsx"

df_empresas_cons <- read.xlsx(xlsxFile = path, colNames = TRUE, check.names = TRUE)

# getting data from sheets - dados das empresas dos consultores

df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
  select(-Produtos) %>% 
  mutate(
    Nome = tolower(Nome),
    Nome = stri_trans_general(str = Nome, id = "Latin-ASCII"),
    Nome = str_trim(Nome, side = "both")
  ) %>% 
  as_tibble()





#-----------------------------------------------------------------------------------------------

# filenames <- list.files("C:/Users/marin/Downloads/arquivos_p_recibos-20220915T210326Z-001", 
#                         pattern = "*.xlsx",
#                         full.names = TRUE, 
#                         recursive = TRUE)

filenames <- unzip("C:/Users/marin/Downloads/arquivos_p_recibos-20220915T210326Z-001.zip")

# filenames <- lapply(filenames_zip, unzip)
# 
# filenames <- unlist(filenames)
# 



file_list <- lapply(filenames, 
                    openxlsx::read.xlsx,
                    startRow = 6,
                    colNames = TRUE,
                    check.names = FALSE,
                    detectDates = TRUE,
                    fillMergedCells = FALSE
                    ) 



names(file_list) <- filenames

names(file_list) <- names(file_list) %>% 
     str_extract("(?<=_\\s)(.+)(?=\\.)")

#-----------------------------------------------------------


file_list_final <- vector("list")

for (i in seq_along(file_list)) {
  
  file_list_final[[i]]<- file_list[[i]] %>% 
    select(c(Data,Km,Total)) %>% 
    drop_na() %>% 
    mutate(
      nome = names(file_list)[i] %>% 
        str_extract("(?<=-\\s)(.+)(?=\\s\\_)") %>% 
        str_trim("both") %>% 
        str_to_lower(),
      
      nome = stri_trans_general(str = nome, id = "Latin-ASCII"),
      
      produto = names(file_list)[i] %>% 
        str_extract("(^.)\\d*") %>% 
        str_trim("both"),
      
      mes_faturamento = names(file_list)[i] %>% 
        str_extract("(?<=_)(.+)(?=-)") %>% 
        str_trim("both") %>% 
        str_replace(" ", "-") %>% 
        str_c("1-",.) %>% 
        as_date(format = "%d-%B-%Y"),
      
      quinzena = names(file_list)[i] %>% 
        str_extract("(?<=\\d\\-)(.+)(?=_)"),
      
      Total = as.numeric(format(round(as.numeric(Total), digits =  2), nsmall = 2))
    )
  
}

#---------------------------------------------------------------------

# Todos os consultores em todas as planilhas
consultant_names <- vector("character")

for (i in seq_along(file_list_final)) {
  
  consultant_names[[i]] <- file_list_final[[i]] %>% 
    select(nome) %>% 
    distinct() %>% 
    drop_na()
  
}

consultant_names <- consultant_names %>% 
  unlist() %>% 
  str_remove("nome_do_consultor") %>% 
  as_tibble() %>% 
  distinct()
#---------------------------------------------------------------------


for (i in 1:nrow(consultant_names)) {
  
  
  list_consultant <- vector("list")
  df_consultor <- tibble()
  
  for (k in seq_along(file_list_final)) {
    
    list_consultant[[k]] <- file_list_final[[k]] %>% 
      filter(str_to_lower(nome) %in% consultant_names[i,])
    
    df_intermediate <- list_consultant[[k]] %>% 
      nest(data = -c(nome, produto, mes_faturamento, quinzena))
    
    df_consultor <- rbind(df_consultor, df_intermediate)
    
    df_consultor <- df_consultor %>% 
      drop_na() %>% 
      arrange(mes_faturamento)
    
    
    
  }  # fim do "for" que cri os df para cada consultor
  
  #---------------------------------------------------------------------
  report_complet <- df_consultor 
  #--------------------------------------------------------------------  
  #pegando as informações do banco
  
  nome <- str_to_title(report_complet[[1]][[1]])#pegando o nome do consultor
  
  bank_info  <- tryCatch(get_bank_info(df_empresas_cons, tolower(nome), opt = 0),
                         error = function(e){
                           message(paste("ERRO CONSULTOR:", nome, sep = " "))
                           return(as.list(rep("ERRO",11)))
                         }) 
  bank_info <- as_vector(bank_info)#apenas para os resultados pararem de serem printados no console
  # e possibilitar a visualização dos consultores que deram erro
  # não mudei isso na funcção get_bank_info pq não sei se isso alteraria 
  # o comportamento dela em outras ocasioes
  #---------------------------------------------------------------------------------------------------
  list_cons_prod  <-  split(report_complet,list(report_complet$produto, report_complet$nome), drop = TRUE)
  #-----------------------------------------------------------------------------
  for (j in seq_along(list_cons_prod)) {
    
    nome; bank_info
    
    
    codigo <- list_cons_prod[[j]][1,2]
    
    report_complet <- list_cons_prod[[j]]
    
tryCatch(
    render("templates/TEMPLATE_RECIBO.Rmd", 
           output_file = paste0("Recibo","-", nome,"-", codigo,".docx" ),
           output_dir = paste0("Recibos/",nome,"/",codigo),
           quiet = TRUE
      ),
    error = function(e){ 
      message(paste("Erro consultor:", nome, codigo),sep = " ")
    }
    )
 }
  
  
}

