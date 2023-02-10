library(tidyverse)
library(openxlsx)
library(janitor)
library(blastula)
library(rmarkdown)


##########################################################################
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())

}
####################################################################################

path <- "C://Users//marin//Documents//R_projects//Fesp_agata//planilhas_pagamentos//Pagamentos_Consultores FEV22.xlsx"
# getting data from sheets
resumo_produto <- read.xlsx(xlsxFile = path,
                            sheet = "CONSOLIDADO", startRow = 2, fillMergedCells = TRUE)

names(resumo_produto) <- c("PRODUTO", "HORAS EXECUTADAS_APROVADAS",  "UNITÁRIO_(HH)","APROVADOS_(R$)")


#COLOCAR O MES NO EMAIL
df_plot <- resumo_produto

resumo_produto <- resumo_produto %>% 
  slice(1:14)


  #
resumo_produto$PRODUTO <- str_remove(resumo_produto$PRODUTO,".*- ")
  
  
  #

  
  valores  <-  resumo_produto %>% 
  ggplot(aes(x = fct_reorder(PRODUTO, `APROVADOS_(R$)`), y = `APROVADOS_(R$)`))+
  geom_col()+
    coord_flip()+
    xlab("Produtos")+ylab("Valores")+
    my_theme()
  
  
horas <- resumo_produto %>% 
    ggplot(aes(x = fct_reorder(PRODUTO, `HORAS EXECUTADAS_APROVADAS`), y = `HORAS EXECUTADAS_APROVADAS`))+
    geom_col()+
    coord_flip()+
    xlab("Produtos")+ylab("HORAS EXECUTADAS")+
    my_theme()
  
#############################################################
# blastula::create_smtp_creds_key(
#   id = "gmail",
#   user = "cruz.ml.pedro@gmail.com",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE
# )
#delete_credential_key("gmail")
  
#criando o e-mail, através do modelo .RMD e da função render
email <- render_email("C://Users//marin//Documents//R_projects//Fesp_agata//email_template_gerente_produto.Rmd") 


email %>% 
  smtp_send(
    from = "cruz.ml.pedro@gmail.com", 
    to = "cruz.ml.pedro@gmail.com", # lista com os emails dos consultores
    subject = paste("Relatório mensal"), 
    credentials = creds_key(id = "gmail")
  )

  
  
  

#colocar o agendador de tarefas
