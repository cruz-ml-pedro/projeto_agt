#####################################################################################
##########################Carregando os pacotes usados###############################
library(lubridate)
library(tidyverse)
library(openxlsx)
library(janitor)
library(rmarkdown)#criação de documentos através do pandoc/knit/LaTex
library(stringi) #manipulação de strings
source("FUNCTIONS/get_bank_info_function.R")
source("FUNCTIONS/get_parcelas_function.R")
source("FUNCTIONS/get_consultores_mes.R")

#----------------------------------------------------------------------------------#
#####################carregando as planilhas dos produtos############################

#abrindo as diferentes planilhas contidas no arquivo de pagamentos
# specifying the path name - planilhas produtos
path <- "planilhas_excell/Faturamento Consultores JUL22.xlsx"
# getting data from sheets
sheets <- openxlsx::getSheetNames(path)
#
data_frame_list <- lapply(sheets,
                          openxlsx::read.xlsx,
                          startRow = 5,
                          xlsxFile=path)
# assigning names to data frame
names(data_frame_list) <- sheets
#
#--------------------------------------------------------------------------------
data_frame_list <- data_frame_list[-c(1,2,5,17)]#removendo as primeiras planilhas que não são dos produtos
new_p4 <- rbind(data_frame_list[[3]],data_frame_list[[4]])#criando um df único para o produto 4
data_frame_list <- data_frame_list[-c(3,4)]#removendo os df referentes ao produto 4
data_frame_list <- c(list(new_p4), data_frame_list)# adicionando o df novo para a lista de df dos produtos
names(data_frame_list)[1] <- "Relatório Produto 4"#renomeando o df novo do p04
#-----------------------------------------------------------------------------

#------------------------------------------------------------------------------
get_consultores_mes(data_frame_list)#Criando uma lista todos os consultores que atuaram no mês
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

data_rel  <- format(rollback(Sys.Date()),format = "%b-%Y")

consultores_zero <- vector(mode = "character")#vetor para salvar os consultores que aparecem 
                                              #mas não tem valores atribuidos aos serviços do mês.
#---------------------------------------------------------------------------------------
# criando as planilhas usadas para criar os relatórios
for (j in 1:length(data_frame_list) ) {
  
  #salvando as diferentes planilhas a cada iteração
  produto_df<- data_frame_list[[j]]# separando um df por vez
  #
  produto <- stri_trans_general(str = names(data_frame_list)[j], id = "Latin-ASCII")
  #
  produto <-str_extract(produto, "Produto?\\s*(\\d+)")
  #
  produto <- str_trim(produto, side = "both")
 
#-------------------------------------------------------------------------------------------
nest_produto_df <- produto_df %>% 
    select(-`SOLUÇÃO`) %>% 
    drop_na() %>% 
    mutate(
      responsavel = stri_trans_general(str = responsavel, id = "Latin-ASCII"),#removendo os acentos dos nosmes
      DATA.DE.EXECUÇÃO = as.Date(DATA.DE.EXECUÇÃO, "%d/%m/%Y"), #PASSANDO A DATA PARA O FORMATO DE DATA
      mo = month(DATA.DE.EXECUÇÃO)#CRIANDO VARIÁVEL PARA SEPARA RELATÓRIOS DE DIFERENTES MESES     
       ) %>% 
    group_by(responsavel, mo) %>% 
    nest()
#--------------------------------------------------------------------------------------------

 for (k in 1:nrow(nest_produto_df)) {
    produto# nome do produto usado no relatório, está aqui dentro para ser usado pelo render
    #
    nome <- nest_produto_df[[1]][[k]]# pegando o nome do consultor para o relatório 
    #
    mes_exe <- nest_produto_df[[2]][[k]]#selecionando o mes de execução das atividades
    #
    df <- nest_produto_df[[3]][[k]] %>% # pegando a tabela de atividades que vai no relatório
          select(-1) %>% 
          rename_at(1:6, .funs = tolower) %>% 
          mutate(
            tarefas = tolower(tarefas), # passando os nomes das colunas e a linhas das tarefas p/ minúscula
            `relatório.no.sistema` = tolower(`relatório.no.sistema`),
            valor = as.numeric(valor)
            
            )
    #
    #
total <- sum(df$valor)# calculando o valor total que o consultor deve receber 
df$valor <- paste("R$",df$valor)#adiconando R$ nos valores que vão na tabela do relatório
#----------------------------------------------------------------------------------
if(total == 0){
  
  consultores_zero <- c(nome,produto)# caso o valor seja zero a parcela não sera atualizada e
                                     #o relatório não será criado
  
}else{

  parcela <-tryCatch(get_parcelas(nome=nome,produto=produto),
                error = function(e){
                 message(paste("ERRO Parcela, CONSULTOR:", nome, produto, sep = " "))
                 return("Erro")
                })
       
  
  # 
  #parcela <-  get_parcelas2(nome =  nome, df= df, total = total, produto = produto)
  # 
   
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
render("templates/TEMPLATE_FATURAMENTO.Rmd", #função que cria os relatórios e atualza as variáveis
         output_file = paste0("Faturamento-",data_rel,"_",nome,"_",produto,".docx"),
         output_dir = paste0("Faturamento-",data_rel,"/",produto),
         quiet = TRUE
        )
  
 }

  }
  
}

write.table(consultores_zero, file = "consultores_faturamento_zero.txt", quote = TRUE)

