#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het uitlezen van input XML bestanden van de KLW.

#INPUTS
#path_xml_files --> string met path naar mappen met input XML files vanuit KLW.

XMLtoDataFrame <- function(path_xml_files){
  
  #Libraries
  library(xml2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  #Welke input XML bestanden hebben we?
  files = list.files(path = path_xml_files, full.names = T)
  
  #Inlezen van XML bestanden
  data_totaal_running = NULL
  
  counter = 0
  for(file in files){
    
    counter = counter + 1
    
    print(round( counter / length(files) * 100, 2))
    
    xml_file = as_list(read_xml(file))
    
    #Verkrijgen algemene informatie van bestand: kvk_nummer en jaartal
    xml_df_algemeen = as.data.frame(tibble::as_tibble(xml_file) %>% tidyr::unnest_longer(KW007_Input) %>% dplyr::filter(KW007_Input_id == "ALGEMEEN") %>% tidyr::unnest_wider(KW007_Input) %>% tidyr::unnest( cols = names(.))) %>% tidyr::unnest(cols = names(.))
    jaartal = pull(xml_df_algemeen %>% dplyr::select(jaartal))
    kvk_nummer = pull(xml_df_algemeen %>% dplyr::select(kvk_nummer))
    
    #Verkrijgen van Aanleg sectie uit XML.
    xml_df_vrd = tibble::as_tibble(xml_file) %>% tidyr::unnest_longer(KW007_Input) %>% dplyr::filter(KW007_Input_id == "VRDAANLEG") %>% tidyr::unnest_wider(KW007_Input)
    
    krachtvoer_running = NULL
    krachtvoer_found = FALSE
    
    for(name in names(xml_df_vrd)){
      
      if(str_detect(name, "Krachtvoer")){
        
        krachtvoer_found = TRUE
        
        df_kv = xml_df_vrd %>% dplyr::select(name) %>% tidyr::unnest_longer(name) %>% tidyr::unnest(cols=names(.)) %>% tidyr::unnest(cols=names(.))
        
        df_kv = as.data.frame(df_kv)
        names_df_kv = df_kv[,2]
        
        df_kv_transpose =t(df_kv[,1])
        
        colnames(df_kv_transpose) = names_df_kv
        
        if(is.null(krachtvoer_running)){
          krachtvoer_running = df_kv_transpose
        } else {
          krachtvoer_running = merge(krachtvoer_running, df_kv_transpose, all = T)
        }
      }
      
    }
    
    #Als er nieuwe data is:
    if(krachtvoer_found){
      
      #Toevoegen van KVK nummer en jaartal aan de data
      krachtvoer_running$kvk_nummer = kvk_nummer
      krachtvoer_running$jaartal = jaartal
      
      
      if(is.null(data_totaal_running)){
        data_totaal_running = krachtvoer_running
      } else {
        data_totaal_running = merge(data_totaal_running, krachtvoer_running, all = T)
        
      }
    }
    
  }
  
  return(data_totaal_running)
  
}




