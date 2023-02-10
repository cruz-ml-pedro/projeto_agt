get_parcelas2 <- function(nome, df,total, produto, mes, ano){

df_controle_raw <- read.xlsx("C:\\Users\\agata\\Downloads\\NOVO - Controle de Pagamento - Consultores (V2).xlsx",
                              startRow = 8, sheet = 1,
                              detectDates = TRUE,
                              fillMergedCells = TRUE)

df_controle_raw <- df_controle_raw %>% 
  drop_na(any_of("CONSULTOR")) %>% 
  select(-X17) %>% 
  mutate(
    CONSULTOR = tolower(CONSULTOR),
    CONSULTOR= str_trim(CONSULTOR, side = "both"),
    CONSULTOR = stri_trans_general(str = CONSULTOR, id = "Latin-ASCII")
    
  )


numero_produto <- str_extract(produto,"\\d+" )

codigo <- paste0("P",numero_produto)



df_parcela  <- df_controle_raw %>% 
    filter(CONSULTOR == str_to_lower(nome) & PRODUTO == codigo) %>% 
    select(CONSULTOR, ANO, `MÊS`,PRODUTO, NUMERO.PARCELA) %>% 
    mutate(
      ANO = as.numeric(ANO),
      `MÊS` = as.numeric(`MÊS`)
    )

#----------------------------------------------------------

df_parcela_comparativo <- df_parcela %>%
  select(-NUMERO.PARCELA) 
  


#-----------------------------------------------------------
df_comparativo<- tibble("CONSULTOR" = str_to_lower(nome) ,
       "ANO" =year(Sys.Date()),
       "MÊS" = month(Sys.Date()),
       "PRODUTO" = paste0("P", numero_produto)
)


v <- semi_join(df_parcela_comparativo, df_comparativo)
#usar o semi_join para saber se tem algum caso complementar em relação as informações que estão
#entrando na função, como só entra uma por vez se retornar zero linhas ñ tem parcela complementar
#-----------------------------------------------------------

if(nrow(df_parcela)==0){
  
  numero_parcela <- 1
  
} else if(nrow(v)>0){
  
parcela_p_comp <- df_parcela %>% 
        filter(ANO== ano & `MÊS`==mes & PRODUTO ==paste0("P", numero_produto)) %>% 
        select(NUMERO.PARCELA)

#separar a parcela em duas strijngs e fazer os testes para saber se é .1, .2...
      
} else{
  
  numero_parcela<- df_parcela %>% 
    arrange(NUMERO.PARCELA) %>% 
    select(NUMERO.PARCELA) %>% 
    slice(1) %>% 
    as.numeric()
  
  numero_parcela <- numero_parcela + 1
  
  
  
}
  
  


infos_to_fill <- tibble("CONSULTOR" = nome ,
                        "DATA.DA.EMISSÃO" = NA,
                        "ANO" = ano,
                        "MÊS" = mes,
                        "PRODUTO" = paste0("P", numero_produto),
                        "Mês.de.referência" = NA,
                        "NF" = NA,
                        "HORAS.FATURADAS" = sum(df$horas.executadas) ,
                        "NUMERO.PARCELA" = numero_parcela,
                        "VALOR.TOTAL.$" = total,
                        "STATUS.RESPOSTA.CONSULTOR" = "Pendente",
                        "STATUS.ENVIO.RECIBO" = "Pendente" ,
                        "STATUS.ENVIO.FIN" = "Pendente" ,
                        "TEVE.PROBLEMAS?" = NA ,
                        "DETALHE.DO.PROBLEMA" =  NA,
                        "SOLUÇÃO.-.STATUS" = NA 
                        )



write_csv2(infos_to_fill,"controle_R_parcelas.csv" ,append = TRUE, col_names = FALSE)

return(numero_parcela)

}
