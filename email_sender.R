library(blastula)
library(keyring)
library(tidyverse)
library(stringr)
library(openxlsx)
library(stringi)
library(lubridate)



# essa função salva na memória do computador suas credenciais
blastula::create_smtp_creds_key(
  id = "gmail",
  user = "alima@fespsp.org.br",
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE
)
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
    NOME = tolower(NOME)
  ) 

df_empresas_cons$NOME <- stri_trans_general(str =df_empresas_cons$NOME, id = "Latin-ASCII")
#---------------------------------------------------------------------------------------------
#verificar como pegar o nome dos arquivos

list_data <-file.info(list.files(path = paste("Faturamento","-",
                                        format(as.Date(rollback(Sys.Date())),
                                        format = "%b-%Y"), sep = ""),
                                        pattern = "*.docx", full.names = T, recursive = T))
list_data<-rownames(list_data)


for (i in 1:length(list_data)) {

  string <- list_data[[i]]
  produto_comp <- str_extract(string, "Produto?\\s*(\\d+)")
  init  <- str_extract(produto_comp,"^P")
  numero  <- str_extract(produto_comp,"(\\d+)")
  produto <- str_c(init,numero)
  nome <-  str_extract(string, ("(?<=_)(.+)(?=\\_)"))

#-------------------------------------------------------------------------------------------
email_consultor  <- df_empresas_cons %>% 
      filter(NOME == tolower(nome) & PRODUTO == produto) %>% 
      select(EMAIL) %>% 
     as.character()
#-------------------------------------------------------------------------------------------  
  
#criando o e-mail, através do modelo .RMD e da função render
  email <- render_email('templates/email_template.Rmd') %>% 
    add_attachment(file ="attachment_email/ANEXO 3_Dados FESPSP para faturamento.pdf") %>% 
    add_attachment(file = "attachment_email/ANEXO 4_DECLARAÇÃO PESSOAS JURÍDICAS OPTANTES PELO SIMPLES NACIONAL.docx") %>% 
    add_attachment(file = "attachment_email/ANEXO 5_V2 Fluxo e processo de pagamento - Consultores e Coordenadores.pptx") %>% 
    add_attachment(file = list_data[[i]])
  
  
  
tryCatch(email %>% 
    smtp_send(
      from = "alima@fespsp.org.br", #colocar o e mail da agata
      to = email_consultor,
      cc = c("aluisio@fespsp.org.br",
             "ezunarelli@fespsp.org.br",
             "bbotelho@fespsp.org.br",
             "gmartins@fespsp.org.br"),
      subject = paste("Faturamento","-",
                      format(rollback(Sys.Date()),format = "%m-%Y"),
                      "-",nome, produto), 
      credentials = creds_key(id = "gmail")
    ),
  error = function(e){
    message(paste("ERRO, CONSULTOR:", nome, produto, sep = " "))
  })
}

