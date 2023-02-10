#########################################################################
#                 Carregando os pacotes utilizados                      #
#                                                                       #
#########################################################################
library(tidyverse)#manipulação dos dados
library(openxlsx)#leitura de arquivos xlxs
library(stringi)#manipulação de strings
library(janitor)
library(lubridate)
source("FUNCTIONS/get_bank_info_function.R")
source("FUNCTIONS/excel_fill_infos_function.R")
source("FUNCTIONS/comp_diff_reembolso_function.R")
########################################################################
#              Carregando os dados utilizados                          #
#                                                                      #
########################################################################

#abrindo as diferentes planilhas contidas no arquivo de pagamentos
# specifying the path name - planilhas produtos
# getting data from sheets

df_reembolso_raw <- read.xlsx("planilhas_excell/2 Quinzena Deslocamento - SETEMBRO 2022 - FESPSP.xlsx",
                          startRow = 10, sheet = 1,
                          detectDates = TRUE,
                          fillMergedCells = TRUE)

#----------------------------------------------------------------------------------------------------

df_reembolso <- df_reembolso_raw %>% 
  janitor::clean_names() %>% 
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
  mutate(
    valor = as.numeric(format(round(valor, digits =  2), nsmall = 2))
  ) %>% 
 tibble::as_tibble()

#---------------------------------------------------------------------------------------------------



comp_list <- comparativo_reembolso(df_reembolso_raw = df_reembolso_raw, df_reembolso = df_reembolso)


if(length(comp_list[[1]])>0){
  
  stop(cat("Existem NAs em:",comp_list[[1]], sep = "\n"))
  
}else if(comp_list[[2]]!=comp_list[[3]] || comp_list[[4]]!=comp_list[[5]]){
  
  message(paste("O valor total ou de km das planilhas não bate.",
                "R$ original:", comp_list[[2]],
                "R$ reembolso:", comp_list[[3]],
                "-",
                "km original:", comp_list[[4]],
                "km reembolso:", comp_list[[5]],
                sep = " "))
  message("Essas são algumas das possíveis fontes de erro:")
  print(knitr::kable(comp_list[[6]]))
  
  var = readline(prompt = "Caso deseje continuar digite sim + Enter, ou aperte qualquer botão + enter para parar ")
  
  if(var != "sim"){
    
    stop("Tchau *-*")
    
  }else{
    #*----------------------------------------------------------------------*
    #     removendo os acentos dos nomes e                                  * 
    #     tranformando a coluna de deslocamento de string para numérica     *
    #-----------------------------------------------------------------------*
    
    path2 <- "planilhas_excell/Dados bancarios_SEBRAE.xlsx"
    
    # getting data from sheets - dados das empresas dos consultores
    df_empresas_cons <- read.xlsx(xlsxFile = path2, colNames = TRUE, check.names = TRUE)
    
    df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
      select(-Produtos) %>% 
      mutate(
        Nome = tolower(Nome),
        Nome = stri_trans_general(str = Nome, id = "Latin-ASCII"),
        Nome = str_trim(Nome, side = "both")
      ) %>% 
      as_tibble()
    
    #----------------------------------------------------------------------------------------
    
    report_complet <- df_reembolso %>% 
      group_by(nome_do_consultor,codigo_do_produto) %>% 
      arrange(data_do_atendimento) %>% 
      nest() %>% 
      arrange(nome_do_consultor)
    #----------------------------------------------------------------------------------------
    
    dia_semana <- format(as.Date(Sys.time(),format="%Y/%m/%d"), format = "%d")
    quinzena_path <- ifelse(dia_semana < 15,"2_quinzena","1_quinzena")
    data_rel <- ifelse(dia_semana < 15, format(as.Date(rollback(Sys.Date())),format = "%b-%Y"),
                       format(as.Date(Sys.time()), format = "%b %Y"))
    
    #----------------------------------------------------------------------------------------
    consultor_produto_totais <- report_complet %>% 
      unnest(cols = c(data)) %>% 
      group_by(nome_do_consultor, codigo_do_produto) %>% 
      summarise(total_km = sum(total_km),
                total_valor = sum(valor)) %>% 
      ungroup() %>% 
      as_tibble()
    
    #
    write_csv2(consultor_produto_totais,
               file = paste0("reembolso_consultores_produtos","-",
                             quinzena_path,"-",
                             data_rel, ".csv"))
    #-------------------------------------------------------------------------
    
    dir.create(paste("Reembolso_", quinzena_path, "_", data_rel, sep = ""))
    valor_total_planilha <- vector("numeric")
    km_total_planilha <- vector("numeric")
    
    for (i in 1:nrow(report_complet)) {
      
      #
      nome <-report_complet[[1]][[i]]#pegando o nome do cinsultor
      #
      codigo <- report_complet[[2]][[i]]#pegando o produto
      #
      events <- report_complet[[3]][[i]] %>% 
        select(data_do_atendimento,total_km, valor)#selecionando as colunas de interesse
      #
      if(nrow(events) > 14){
        print(paste("consultor",nome, codigo, "tem mais de 14 deslocamentos, arrumar relatório" ))
      }# apenas verificando se o consultor possui mais deslocamentos do que linhas no formulário
      
      #
      valor_total_planilha[[i]] <- report_complet[[3]][[i]] %>% 
        select(valor)#valor total planilha
      #
      km_total_planilha[[i]] <- report_complet[[3]][[i]] %>% 
        select(total_km)#Km total da planilha
      
      #
      names(events) <- c("Data", "Km", "Total")#colocando o nome das colunas
      #
      events <- events %>% add_column(Taxi = NA, #colocando as colunas em branco pra se adequar ao modelo em xlsx
                                      Combustivel = NA,
                                      `Refeição` = NA, .before = "Km") %>% 
        add_column(`Recarga de celular` = NA,
                   `Lavagem de Carro` = NA,
                   `Materiais para Escritório` = NA,
                   `Passagem / Hospedagem` = NA, .before = "Total")
      
      #pegando as informações do banco
      
      bank_info  <- tryCatch(get_bank_info(df_empresas_cons, tolower(nome), opt = 0),
                             error = function(e){
                               message(paste("ERRO CONSULTOR:", nome, codigo, sep = " "))
                               return(as.list(rep("ERRO",11)))
                             }) 
      
      #-----------------------------------------------------------------------------------------
      #colocando as informações do modelo excel em um workbook - wb
      
      wb <- loadWorkbook(xlsxFile = "templates/TEMPLATE_REFUND.xlsx")#carregando o modelo em excel "em branco"
      #
      #função criada para colocar as infos nos seus devidos lugares dentro
      #do modelo em excel
      wb <- excel_fill_infos(wb,nome,codigo, bank_info, events)
      #
      nome_do_arquivo <- paste("Solicitacao_Reembolso_",# criando um novo nome de arquivo a cada iteração
                               codigo,
                               "-",
                               nome,
                               paste("_",data_rel,"-", quinzena_path,".xlsx", sep = ""))
      path_folder <- paste("Reembolso_", quinzena_path, "_", data_rel,"/", sep = "")
      set_folder <- paste(path_folder,nome_do_arquivo, sep = "")
      
      saveWorkbook(wb, set_folder, overwrite = TRUE)#salvando as solicitações em xlsx
      
    }
    
       #para comparar com os totais da planilha excel
       print(sum(unlist(km_total_planilha)))
       print(format(sum(unlist(valor_total_planilha)), nsmall=2))
    
  }
  
} else{
  
  path2 <- "planilhas_excell/Dados bancarios_SEBRAE.xlsx"
  
  # getting data from sheets - dados das empresas dos consultores
  df_empresas_cons <- read.xlsx(xlsxFile = path2, colNames = TRUE, check.names = TRUE)
  
  df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
    select(-Produtos) %>% 
    mutate(
      Nome = tolower(Nome),
      Nome = stri_trans_general(str = Nome, id = "Latin-ASCII"),
      Nome = str_trim(Nome, side = "both")
    ) %>% 
    as_tibble()
  
  #----------------------------------------------------------------------------------------
  
  report_complet <- df_reembolso %>% 
    group_by(nome_do_consultor,codigo_do_produto) %>% 
    arrange(data_do_atendimento) %>% 
    nest() %>% 
    arrange(nome_do_consultor)
  #----------------------------------------------------------------------------------------
  
  dia_semana <- format(as.Date(Sys.time(),format="%Y/%m/%d"), format = "%d")
  quinzena_path <- ifelse(dia_semana < 15,"2_quinzena","1_quinzena")
  data_rel <- ifelse(dia_semana < 15, format(as.Date(rollback(Sys.Date())),format = "%b %Y"),
                     format(as.Date(Sys.time()), format = "%b %Y"))
  
  #----------------------------------------------------------------------------------------
  consultor_produto_totais <- report_complet %>% 
    unnest(cols = c(data)) %>% 
    group_by(nome_do_consultor, codigo_do_produto) %>% 
    summarise(total_km = sum(total_km),
              total_valor = sum(valor)) %>% 
    ungroup() %>% 
    as_tibble()
  
  #
  write_csv2(consultor_produto_totais,
             file = paste0("reembolso_consultores_produtos","-",
                           quinzena_path,"-",
                           data_rel, ".csv"))
  #-------------------------------------------------------------------------
  
  dir.create(paste("Reembolso_", quinzena_path, "_", data_rel, sep = ""))
  valor_total_planilha <- vector("numeric")
  km_total_planilha <- vector("numeric")
  
  for (i in 1:nrow(report_complet)) {
    
    #
    nome <-report_complet[[1]][[i]]#pegando o nome do cinsultor
    #
    codigo <- report_complet[[2]][[i]]#pegando o produto
    #
    events <- report_complet[[3]][[i]] %>% 
      select(data_do_atendimento,total_km, valor)#selecionando as colunas de interesse
    #
    if(nrow(events) > 14){
      print(paste("consultor",nome, codigo, "tem mais de 14 deslocamentos, arrumar relatório" ))
    }# apenas verificando se o consultor possui mais deslocamentos do que linhas no formulário
    
    #
    valor_total_planilha[[i]] <- report_complet[[3]][[i]] %>% 
      select(valor)#valor total planilha
    #
    km_total_planilha[[i]] <- report_complet[[3]][[i]] %>% 
      select(total_km)#Km total da planilha
    
    #
    names(events) <- c("Data", "Km", "Total")#colocando o nome das colunas
    #
    events <- events %>% add_column(Taxi = NA, #colocando as colunas em branco pra se adequar ao modelo em xlsx
                                    Combustivel = NA,
                                    `Refeição` = NA, .before = "Km") %>% 
      add_column(`Recarga de celular` = NA,
                 `Lavagem de Carro` = NA,
                 `Materiais para Escritório` = NA,
                 `Passagem / Hospedagem` = NA, .before = "Total")
    
    #pegando as informações do banco
    
    bank_info  <- tryCatch(get_bank_info(df_empresas_cons, tolower(nome), opt = 0),
                           error = function(e){
                             message(paste("ERRO CONSULTOR:", nome, codigo, sep = " "))
                             return(as.list(rep("ERRO",11)))
                           }) 
    
    #-----------------------------------------------------------------------------------------
    #colocando as informações do modelo excel em um workbook - wb
    
    wb <- loadWorkbook(xlsxFile = "templates/TEMPLATE_REFUND.xlsx")#carregando o modelo em excel "em branco"
    #
    #função criada para colocar as infos nos seus devidos lugares dentro
    #do modelo em excel
    wb <- excel_fill_infos(wb,nome,codigo, bank_info, events)
    #
    nome_do_arquivo <- paste("Solicitacao_Reembolso_",# criando um novo nome de arquivo a cada iteração
                             codigo,
                             "-",
                             nome,
                             paste("_",data_rel,"-", quinzena_path,".xlsx", sep = ""))
    path_folder <- paste("Reembolso_", quinzena_path, "_", data_rel,"/", sep = "")
    set_folder <- paste(path_folder,nome_do_arquivo, sep = "")
    
    saveWorkbook(wb, set_folder, overwrite = TRUE)#salvando as solicitações em xlsx
    
  }
  
  #para comparar com os totais da planilha excel
  print(sum(unlist(km_total_planilha)))
  print(format(sum(unlist(valor_total_planilha)), nsmall=2))
  
}
  
  
