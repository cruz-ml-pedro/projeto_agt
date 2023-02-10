get_parcelas <- function(nome, produto){
  
  require(openxlsx)
  require(tidyverse)
  require(stringi)
  #----------------------------------------------------------------------------------  
  path <- "planilhas_excell/Parcelas - Consultores SEBRAE.xlsx"
  
  # getting data from sheets - dados das empresas dos consultores
  parcelas_df <- read.xlsx(xlsxFile = path, colNames = TRUE, check.names = TRUE)
  
  parcelas_df <- parcelas_df %>% 
    mutate(ID = row_number(),
           Nome = tolower(Nome),
           across(c(Produto, Nome), str_trim))
  

  parcelas_df$Nome <- stri_trans_general(str =parcelas_df$Nome, id = "Latin-ASCII")
#-----------------------------------------------------------------------------------
  
  
  parcela <- parcelas_df %>% 
    filter(Nome == nome & Produto == produto) %>% 
    select(-c(Empresa,Nome,Produto)) 
  
  parcela_nova <- parcela$N..Parcela + 1
  
  
  #------------------------------------------------------------------------------
  #atualizando a planilha original
  wb <- loadWorkbook(xlsxFile = path)
  
  writeData(wb, sheet = "CONTRATOS",
            x = parcela_nova,
            startCol = "D",
            startRow = parcela$ID + 1,
            colNames = FALSE)
  
  saveWorkbook(wb, path , overwrite = TRUE)
  #----------------------------------------------------------------------
 
  if(length(parcela_nova) == 0){
    
    errorCondition()# apenas para criar um erro quando o nome não é encontrado
  
    
  }else{
    
    return(parcela_nova)
    
  }
  
}


