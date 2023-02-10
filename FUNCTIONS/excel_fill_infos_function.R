#função para posicionar as informações necessarias dentro do tamplete em excel
excel_fill_infos <- function(wb,nome,codigo, bank_info, events){
  
  require(openxlsx)
  require(lubridate)
  
  cells_positions <- list(c("B",3), c("F",4), c("I", 25),c("I", 26),c("I", 27))
  
  infos_to_fill <- list(nome, paste("EVENTOS DO PRODUTO",
                                     codigo, "-",
                                     format(as.Date(rollback(Sys.Date())),format = "%b-%Y"),
                                     "-", quinzena_path),
                       bank_info[12],
                       paste(bank_info[5],"/",bank_info[6], "-", bank_info[7]),
                       paste(bank_info[9], "-",bank_info[10]))
  

  writeData(wb, sheet = "Reembolsos", x = events, startCol = "A", startRow = 7, colNames = FALSE)
  
  for (i in 1:length(cells_positions)) {
    
    writeData(wb,
              sheet = "Reembolsos",
              x =infos_to_fill[[i]],
              xy =cells_positions[[i]],
              colNames = FALSE)
  }
  
 
  return(wb)
  
  
}

