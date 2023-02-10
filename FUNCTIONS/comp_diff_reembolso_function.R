comparativo_reembolso <- function(df_reembolso_raw, df_reembolso){
  
  
  
  df_original<- df_reembolso_raw %>% 
    janitor::clean_names() %>% 
    select(id,nome_do_consultor, data_do_atendimento,total_km,codigo_do_produto, valor_do_reembolso) %>% 
    mutate(
      nome_do_consultor = stri_trans_general(str = nome_do_consultor,id = "Latin-ASCII"),
      nome_do_consultor = tolower(nome_do_consultor),
      valor_do_reembolso = as.numeric(format(round(as.numeric(valor_do_reembolso), digits =  2), nsmall = 2)),
      data_do_atendimento = format(as.Date(data_do_atendimento),format="%d/%m/%Y")
    ) %>% 
    drop_na(any_of("nome_do_consultor")) %>% 
    distinct()
  
#-----------------------------------------------------------------------------------------------------

  
  df_reembolso_comp <- df_reembolso %>% 
    rename("valor_do_reembolso" = valor) %>% 
    mutate(
      data_do_atendimento = as_date(data_do_atendimento, format = "%d/%m/%Y"),
      valor_do_reembolso = as.numeric(format(round(as.numeric(valor_do_reembolso), digits =  2), nsmall = 2)),
      total_km = as.numeric(format(round(as.numeric(total_km), digits =  2), nsmall = 2)),
      data_do_atendimento = format(as.Date(data_do_atendimento),format="%d/%m/%Y")
    ) %>% 
    relocate(total_km,
             .before = codigo_do_produto
    )
  
  #--------------------------------------------------------------------------------------------------
  
  
  na_possibly <- names(which(colSums(is.na(df_reembolso_comp))>0))
  
  valor_original  <-  sum(df_original$valor_do_reembolso)
  valor_comp  <- sum(df_reembolso_comp$valor_do_reembolso)
  
  km_original <- sum(df_original$total_km)
  km_comp <- sum(df_reembolso_comp$total_km)
  
  
  diff_tab  <- anti_join(df_reembolso_comp, df_original)
  
  diff_tab_final <- tibble()
  
  if(nrow(diff_tab)==0){
    
    
    return(list(na_possibly, valor_original, valor_comp, km_original, km_original, diff_tab_final))
    
  } else{
    for (i in 1:nrow(diff_tab)) {
      
      original <-  df_original %>% filter(id==diff_tab$id[[i]]) %>% 
        mutate(
          font = "original"
        ) %>% 
        as_tibble()
      
      comp  <- df_reembolso_comp %>% filter(id==diff_tab$id[[i]]) %>%
        mutate(
          font = "df_reembolso"
        ) %>% 
        as_tibble()
      
      diff_tab_final <- rbind(diff_tab_final,original,comp)
    }
    
    return(list(na_possibly, valor_original, valor_comp, km_original, km_original, diff_tab_final))
    
  }
  
}