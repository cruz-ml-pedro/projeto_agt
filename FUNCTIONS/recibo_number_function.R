
numero_parcelas <- function(nome, codigo){
  
  require("readr")
  require("tibble")

parcela_inicial <- read_delim("C:/Users/marin/Downloads/Fesp_agata/numero_recibo.txt",
                            col_names = TRUE,
                            show_col_types = FALSE)

if(parcela_inicial$ano != format(Sys.Date(),"%Y")){
  
  parcela_inicial$ano <- format(Sys.Date(),"%Y")
  
  parcela_inicial$numero_parcela <- 0
  
  parcela_inicial$numero_parcela <- parcela_inicial$numero_parcela + 1
  
  parcela_text <- paste(parcela_inicial$numero_parcela, parcela_inicial$ano, sep="-")
  
  write_delim(parcela_inicial,"C:/Users/marin/Downloads/Fesp_agata/numero_recibo.txt")
  
  df_controle <- read_delim("C:/Users/marin/Downloads/Fesp_agata/controle_recibo_consultor_mes.txt",
                          col_names = TRUE,
                          show_col_types = FALSE)
 
  df_recibo <- tibble(numero_da_parcela= parcela_text, nome_consultor= nome, produto =  codigo)
  
  df_controle <- rbind(df_controle,df_recibo)
  
  write_delim(df_controle, "C:/Users/marin/Downloads/Fesp_agata/controle_recibo_consultor_mes.txt", append = FALSE)
  
  return(parcela_text)
  
} else{
  
  parcela_inicial$numero_parcela <- parcela_inicial$numero_parcela + 1
  
  parcela_text <- paste(parcela_inicial$numero_parcela, parcela_inicial$ano, sep="-")
  
  write_delim(parcela_inicial,"C:/Users/marin/Downloads/Fesp_agata/numero_recibo.txt")
  
  df_controle <- read_delim("C:/Users/marin/Downloads/Fesp_agata/controle_recibo_consultor_mes.txt",
                          col_names = TRUE,
                          show_col_types = FALSE)
                          
  df_recibo <- tibble::tibble(numero_da_parcela= parcela_text, nome_consultor= nome, produto =  codigo)
  
  df_final <- rbind(df_controle,df_recibo)
  
  write_delim(df_final,"C:/Users/marin/Downloads/Fesp_agata/controle_recibo_consultor_mes.txt", append = FALSE)
  
  return(parcela_text)
  
}

}
