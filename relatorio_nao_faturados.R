library(lubridate)
library(tidyverse)
library(openxlsx)
library(janitor)
library(rmarkdown)#criação de documentos através do pandoc/knit/LaTex
library(stringi) #manipulação de strings
source("FUNCTIONS/get_bank_info_function.R")
source("FUNCTIONS/get_parcelas2_function.R")
source("FUNCTIONS/get_consultores_mes.R")

path <- "planilhas_excell/Faturamento Consultores JUL22.xlsx"

# getting data from sheets
df_nao_faturados <- read.xlsx(path,
                          startRow = 5, sheet = 2,
                          detectDates = TRUE,
                          fillMergedCells = TRUE)

names(df_nao_faturados) <- make_clean_names(names(df_nao_faturados))
#------------------------------------------------------------------------------
lista_consultores <- df_nao_faturados %>%
   drop_na() %>% 
   mutate(
     mes = format(as.Date(data_de_execucao), "%m")
   ) %>%
   select(responsavel,mes, produto) %>% 
   distinct() %>% 
   arrange(responsavel, mes, produto)

write_csv2(lista_consultores,
           file = paste0("consultores_não_faturados_", format(rollback(Sys.Date()),format = "%b-%Y"),".csv"),
)
#------------------------------------------------------------------------------



#planilha com os dados bancarios dos consultores
path2 <- "planilhas_excell/Dados bancarios_SEBRAE.xlsx"

# getting data from sheets - dados das empresas dos consultores
df_empresas_cons <- read.xlsx(xlsxFile = path2,
                              colNames = TRUE,
                              check.names = TRUE)

df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
  select(-Produtos) %>% 
  mutate(
    Nome = tolower(Nome),
    Nome= str_trim(Nome, side = "both"),
    Nome = stri_trans_general(str = Nome, id = "Latin-ASCII")
  )

#------------------------------------------------------------------------------------------
#

#data_rel  <- format(as.Date(rollback(Sys.Date())),format = "%b-%Y")

consultores_zero <- vector(mode = "character")#vetor para salvar os consultores que aparecem 
#mas não tem valores atribuidos aos serviços do mês.
#---------------------------------------------------------------------------------------

  #salvando as diferentes planilhas a cada iteração
df_nao_faturados_nest <- df_nao_faturados %>% 
            select(-solucao) %>% 
            drop_na() %>% 
            mutate(
              produto = str_extract(produto, "PRODUTO?\\s*(\\d+)"),
              produto = str_trim(produto, side = "both"),
              responsavel = stri_trans_general(str = responsavel, id = "Latin-ASCII"),#removendo os acentos dos nosmes
              data_de_execucao = as.Date(data_de_execucao, "%d/%m/%Y"), #PASSANDO A DATA PARA O FORMATO DE DATA
              mo = month(data_de_execucao),
              ano = year(data_de_execucao)     
            ) %>% 
            group_by(produto,responsavel,mo,ano) %>% 
            nest() %>% 
            ungroup()
  
#-------------------------------------------------------------------------------------------
  
  for (k in 1:nrow(df_nao_faturados_nest)) {
    
    
    produto <- df_nao_faturados_nest[[1]][[k]]
    produto <- tolower(produto)
    produto <- str_remove(produto, "0")
      
    #
    nome <- df_nao_faturados_nest[[2]][[k]]# pegando o nome do consultor para o relatório 
    #
    mes_exe <-  df_nao_faturados_nest[[3]][[k]]#selecionando o mes de execução das atividades
    #
    ano_exe <- df_nao_faturados_nest[[4]][[k]]
    #
    df <-  df_nao_faturados_nest[[5]][[k]] %>% # pegando a tabela de atividades que vai no relatório
      mutate(
      tarefas = tolower(tarefas)# passando os nomes das colunas e a linhas das tarefas p/ minúscula
        )
    
       #
    total <- sum(df$valor)# calculando o valor total que o consultor deve receber 
    df$valor <- paste("R$",df$valor)#adiconando R$ nos valores que vão na tabela do relatório
    #
    data_rel  <- as.Date(df$data_de_execucao[[1]], format = "%b-%Y") 
    #----------------------------------------------------------------------------------
    if(total == 0){
      
      consultores_zero <- c(nome,produto)# caso o valor seja zero a parcela não sera atualizada e
      #o relatório não será criado
      
    }else{
      
      parcela <-tryCatch(get_parcelas2(nome=nome,df=df,total=total,produto=produto, mes=mes_exe, ano=ano_exe),
                         error = function(e){
                           message(paste("ERRO Parcela, CONSULTOR:", nome, produto, sep = " "))
                           return("Erro")
                         })
      
      #-----------------------------------------------------------------------------------
      #dados bancários
      #filtrando a planilha de dados bancários com os nomes que vem dos diferentes relatórios
      #de faturamento
      
      bank_info <- tryCatch(get_bank_info(df_empresas_cons, tolower(nome), opt = 1),
                            error = function(e){
                              message(paste("ERRO BANCO, CONSULTOR:", nome, produto, sep = " "))
                              return(as.list(c("Colocar o nome da empresa", rep("erro", 11))))
                            })
      #
      bank_info <- as_vector(bank_info)#apenas para os resultados pararem de serem printados no console
      # e possibilitar a visualização dos consultores que deram erro
      # não mudei isso na funcção get_bank_info pq não sei se isso alteraria 
      # o comportamento dela em outras ocasioes
      #-------------------------------------------------------------------------------  
      render("templates/TEMPLATE_NAO_FATURADOS.Rmd", #função que cria os relatórios e atualza as variáveis
             output_file = paste0("Faturamento-",data_rel,"_",nome,"_",produto,".docx"),
             output_dir = paste0("Faturamento-",format(data_rel, "%b-%Y"),"/",produto),
             quiet = TRUE
      )
      
    }
    
  }
  


write.table(consultores_zero, file = "consultores_faturamento_zero.txt", quote = TRUE)

