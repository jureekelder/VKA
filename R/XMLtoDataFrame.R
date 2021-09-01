#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het uitlezen van input XML bestanden van de KLW.

#INPUTS
#path_dataset --> string met path naar mappen met input XML files vanuit KLW.


path_xml_files = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/KLW 2020"

XMLtoDataFrame <- function(path_xml_files){
  
  setwd(path_xml_files)
  
  #Libraries
  library(xml2)
  
  #Welke input XML bestanden hebben we?
  files = list.files(path = path_xml_files, full.names = T)
  
  #Inlezen van XML bestanden
  for(file in files){
    
    xml_file = as_list(read_xml(file))
    
    xml_df = XMLtoDataFrame(xml_file)
    
    #Verkrijgen algemene informatie van bestand: kvk_nummer en jaartal
    xml_df_algemeen = as.data.frame(tibble::as_tibble(test) %>% unnest_longer(KW007_Input) %>% filter(KW007_Input_id == "ALGEMEEN") %>% unnest_wider(KW007_Input) %>% unnest( cols = names(.))) %>% unnest(cols = names(.))
    jaartal = pull(xml_df_algemeen %>% select(jaartal))
    kvk_nummer = pull(xml_df_algemeen %>% select(kvk_nummer))
    
    #Verkrijgen van Aanleg sectie uit XML.
    xml_df_vrd = tibble::as_tibble(test) %>% unnest_longer(KW007_Input) %>% filter(KW007_Input_id == "VRDAANLEG") %>% unnest_wider(KW007_Input)
    
    krachtvoer_running = data.frame()
    
    for(name in names(xml_df_vrd)){
      
      if(str_detect(name, "Krachtvoer")){
        
        df_kv = xml_df_vrd %>% select(name) %>% unnest_longer(name) %>% unnest(cols=names(.)) %>% unnest(cols=names(.))
        
        df_kv = as.data.frame(df_kv)
        names_df_kv = df_kv[,2]
        
        df_kv_transpose =t(df_kv[,1])
        
        colnames(df_kv_transpose) = names_df_kv
        
        krachtvoer_running = merge(krachtvoer_running, df_kv_transpose, all = T)
        
      }
      
    }
    
    krachtvoer_running['kvk_nummer'] = kvk_nummer
    krachtvoer_running['jaartal'] = jaartal
    
  }
}

XMLtoDataFrame(path_xml_files)


