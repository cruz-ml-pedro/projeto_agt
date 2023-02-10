
get_consultores_mes <- function(data_frame_list){

consultores <- list()
final_list_consultant <- tibble()
#data_frame_list <- data_frame_list[-15]

for (i in 1:length(data_frame_list)) {
  
  consultores[[i]]  <- data_frame_list[[i]]%>% 
    select(responsavel, PRODUTO, `DATA.DE.EXECUÇÃO`) %>% 
    distinct(responsavel,PRODUTO) %>% 
    drop_na()

}

consultores_mes<-consultores %>% 
    bind_rows() %>% 
    as_tibble()
  

  
write_csv2(consultores_mes,
          file = paste0("consultores_", format(rollback(Sys.Date()),format = "%b-%Y"),".csv"),
          )
}
