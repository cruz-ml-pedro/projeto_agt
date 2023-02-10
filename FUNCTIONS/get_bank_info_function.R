#function to extract information from the "excel db"
#this information came from dados_bancarios_SEBRAE.xlsx

#--------------------------------------------------------------------------#
#--------------ESSE BLOCO É PARA OS RELATÓRIOS DE PAGAMENTO----------------#
#--------------------------------------------------------------------------#

get_bank_info <- function(empresas, name, opt){
  
  df <- empresas %>%
    filter(Nome == name) %>% 
    mutate(
      Conta= as.character(Conta),
      Conta = str_remove(Conta, "\\E.*")
    )
  
  dados_banco <- tibble()
  error <- c()
  
if(opt == 1){
    
  if (nrow(df) == 1){ 
    
      if(df$MEI == "N" & df$Tipo.de.Conta == "PF"){
        
        error <- df %>% 
          select(Nome.da.empresa, E.mail) %>% 
          as_tibble()
          message(paste(name,"Consultor não é MEI e possui apenas conta PF"))
          
      }else{
        
        dados_banco <- df %>% 
          select(-c("Nome")) %>% 
          as_tibble()
      }
    
  }else{
    
    pj_pf  <- df %>% 
      group_by(Tipo.de.Conta) %>% 
      nest()
    
    dados_banco <- pj_pf %>% 
        filter(Tipo.de.Conta == "PJ") %>% 
        unnest(cols = data) %>% 
        relocate(Tipo.de.Conta, .after = E.mail) %>% 
        as_tibble() %>% 
        select(-c( "Nome"))
  }
  
  if(length(dados_banco) > 0){
    
    output <- dados_banco
    
  } else {
    
    output <- c(error[[1]], error[[2]],"VOCÊ PRECISA INFORMAR UMA CONTA PJ", rep(NA, 8))
    
    } 
  
  return(output)
#--------------------------------------------------------------------------#
#-----------------BLOCO PARA A AS PLANILHAS DE REEMBOILSO------------------#
#--------------------------------------------------------------------------#
  } else {
  
   if (nrow(df) == 1){ 
    
    if(df$Tipo.de.Conta == "PJ"){
      
      error <-c(rep(NA,10),"Para reembolso você deve informar uma PF")
      message(name, "Não possui uma conta PF cadastrada")
    }else{
      
      dados_banco <- df %>% 
        select(-c("Nome")) %>% 
        as_tibble()
    }
    
  }else{
    
    pj_pf  <- df %>% 
      group_by(Tipo.de.Conta) %>% 
      nest()
    
    dados_banco <- pj_pf %>% 
      filter(Tipo.de.Conta == "PF") %>% 
      unnest(cols = data) %>% 
      relocate(Tipo.de.Conta, .after = E.mail) %>% 
      as_tibble() %>% 
      select(-c( "Nome"))
  
  }
  
  if(length(dados_banco) > 0){
    
    output <- dados_banco
    
  } else {
    
    output <- error
    
   } 
  
  return(output)
  
  
  }

}

#---------------------------------------------------------------

