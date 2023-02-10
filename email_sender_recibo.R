library(blastula)
library(keyring)
library(tidyverse)
library(stringr)
library(openxlsx)
library(stringi)
library(lubridate)

#Verificar o porque de parte dos e-mails não estarem sendo enviados. 
#Warning messages:
#1: In header_unstructured(header_value, header_name) :
#The 'To' field contains impermissible characters, please use 7-bit ASCII only
#2: In header_unstructured(header_value, header_name) :
# The 'To' field contains impermissible characters, please use 7-bit ASCII only


# essa função salva na memória do computador suas credenciais
# blastula::create_smtp_creds_key(
#   id = "gmail",
#   user = "alima@fespsp.org.br",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE
# )
#Habilitar acesso de aplicativos menos seguros no Gmail, para que os e-mails sejam enviados. 

#delete_credential_key("gmail")# para deletar suas credenciais
#view_credential_keys()
#--------------------------------------------------------------------------------------------
#carregando a planilha onde tem os dados bancários e os emails

path <- "planilhas_excell/Lista Geral - Consultores por Produto.xlsx"

# getting data from sheets - dados das empresas dos consultores
df_empresas_cons <- read.xlsx(xlsxFile = path, colNames = TRUE, check.names = TRUE, startRow = 3)

df_empresas_cons <- df_empresas_cons %>% # colocando o nome dos consultores em minúscula
  mutate(
    NOME = tolower(NOME),
    NOME = stri_trans_general(str = NOME, id = "Latin-ASCII")
  ) 

#---------------------------------------------------------------------------------------------
#verificar como pegar o nome dos arquivos
path2 <- "C:/Users/marin/Downloads/Fesp_agata/Recibos"
list_data <-list.files(path = path2 , pattern = "*.doc", full.names = T, recursive = T)
# list_data<-rownames(list_data)


for (i in 1:length(list_data)) {
  
  string <- list_data[[i]]
  #
  produto <- str_extract(string, "(?<=-)(.*?)\\.") %>% 
    str_extract(("(?<=-)(.+)(?=\\.)"))
  #
  #produto <- str_c("Produto-",produto_n)
  #
  nome <-str_extract(string, ("(?<=-)(.+)(?=\\-)")) %>% 
         str_to_lower() %>% 
         stri_trans_general(id = "Latin-ASCII")
      
  #-------------------------------------------------------------------------------------------
  email_consultor  <- df_empresas_cons %>% 
    filter(NOME == nome & PRODUTO == produto) %>% 
    select(EMAIL) %>% 
    as.character()
  #-------------------------------------------------------------------------------------------  
  
  #criando o e-mail, através do modelo .RMD e da função render
  email <- render_email('templates/email_template_recibo.Rmd') %>% 
    add_attachment(file = list_data[[i]])
  
  tryCatch(email %>%
             smtp_send(
               from = "alima@fespsp.org.br",
               to = email_consultor,
               cc = c("aluisio@fespsp.org.br",
                      "ezunarelli@fespsp.org.br"),
               subject = paste("Recibo Retroativo","-",nome, produto),
               credentials = creds_key(id = "gmail")
             ),
           error = function(e){
             message(paste("ERRO, CONSULTOR:", nome, produto, sep = " "))
           })
 }

