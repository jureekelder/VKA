#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het uitlezen van input XML bestanden van de KLW ten behoeve van VoerwinstMonitor om krachtvoeders en bijproducten uit input XML te halen!

#INPUTS
#path_xml_files --> string met path naar mappen met input XML files vanuit KLW.

#voor testen
#path_xml_files = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst/InputXMLtest"

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
  feed_type_counter = 0
  for(file in files){
    
    counter = counter + 1
    
    print(round( counter / length(files) * 100, 2))
    
    xml_file = as_list(read_xml(file))
    
    #Verkrijgen algemene informatie van bestand: kvk_nummer en jaartal
    xml_df_algemeen = as.data.frame(tibble::as_tibble(xml_file) %>% tidyr::unnest_longer(KW008_Input) %>% dplyr::filter(KW008_Input_id == "ALGEMEEN") %>% tidyr::unnest_wider(KW008_Input) %>% tidyr::unnest( cols = names(.))) %>% tidyr::unnest(cols = names(.))
    jaartal = pull(xml_df_algemeen %>% dplyr::select(jaartal))
    kvk_nummer = pull(xml_df_algemeen %>% dplyr::select(kvk_nummer))
    
    print(file)
    print(kvk_nummer)
    
    #Verkrijgen van Aanleg sectie uit XML. # VRDAANLEG + VRDBEGIN - VRDEIND = VERBRUIK
    nested_xmls = c("VRDAANLEG", "VRDBEGIN", "VRDEIND")
    for(nest in nested_xmls){
      
      xml_df_vrd = tibble::as_tibble(xml_file) %>% tidyr::unnest_longer(KW008_Input) %>% dplyr::filter(KW008_Input_id == nest) %>% tidyr::unnest_wider(KW008_Input)
      
      krachtvoer_running = NULL
      krachtvoer_found = FALSE
      
      #Overigrvbp --> splitsen op basis van VEM 850. <
      
      for(name in names(xml_df_vrd)){
        
        if(any(str_detect(name, c("Krachtvoer", "Overigrvbp")))){
          
          krachtvoer_found = TRUE
          
          feed_type_counter = feed_type_counter + 1
          
          df_kv = xml_df_vrd %>% dplyr::select(name) %>% tidyr::unnest_longer(name) %>% tidyr::unnest(cols=names(.)) %>% tidyr::unnest(cols=names(.))
          
          df_kv = as.data.frame(df_kv)
          names_df_kv = df_kv[,2]
          
          df_kv_transpose = as.data.frame(t(df_kv[,1]))
          
          colnames(df_kv_transpose) = names_df_kv
          
          df_kv_transpose$jaartal = jaartal
          df_kv_transpose$kvk_nummer = kvk_nummer
          
          df_kv_transpose$feedtype = unlist(str_split(name, "\\."))[1]
          df_kv_transpose$objecttype = nest
          df_kv_transpose$ID = feed_type_counter
          
          if(is.null(krachtvoer_running)){
            krachtvoer_running = df_kv_transpose
          } else {
            krachtvoer_running = merge(krachtvoer_running, df_kv_transpose, all = T)
          }
        }
        
      }
      
      #Als er nieuwe data is:
      if(krachtvoer_found){
        

        if(is.null(data_totaal_running)){
          data_totaal_running = krachtvoer_running
        } else {
          data_totaal_running = merge(data_totaal_running, krachtvoer_running, all = T)
          
        }
      }
    }
    
    
  }
  
  
  #Zorg dat numerieke kolommen als numeriek worden opgeslagen!
  is_all_numeric <- function(x) {
    !any(is.na(suppressWarnings(as.numeric(na.omit(
      x
    ))))) & is.character(x)
  }
  
  
  dataset = data_totaal_running
  
  dataset = dataset %>% dplyr::mutate_if(is_all_numeric, as.numeric)
  
  #Splitsen op basis van VEM 850 a 900
  dataset = dataset %>% dplyr::filter(hoev > 0)
  dataset = dataset %>% dplyr::filter(!is.na(vem))
  
  dataset = dataset %>% dplyr::mutate(vem_categorie = ifelse(vem > 750, "hoog", "laag"))
  
  dataset_xml_samengevat = dataset %>% dplyr::group_by(kvk_nummer, jaartal, feedtype, objecttype, vem_categorie) %>% dplyr::summarise(vem_gewogen = weighted.mean(vem, hoev), re_gewogen = weighted.mean(re, hoev), sum_product = sum(hoev, na.rm = T))
  
  data_wider = pivot_wider(dataset_xml_samengevat, names_from = objecttype, values_from = c("vem_gewogen", "re_gewogen", "sum_product"), values_fill = 0)
  
  #Wat was het verbruik van het product --> BEGIN + AANLEG - EIND. ALs < 0 dan is eindvoorraad groter dan BEGIN + AANLEG, dat is niet reeel!
  data_wider = data_wider %>% dplyr::mutate(verbruik_test = sum_product_VRDAANLEG + sum_product_VRDBEGIN - sum_product_VRDEIND) %>% dplyr::mutate(verbruik = ifelse(verbruik_test < 0, (sum_product_VRDAANLEG + sum_product_VRDBEGIN), verbruik_test))
  data_wider = data_wider %>% dplyr::mutate(verbruik_aanleg = verbruik - sum_product_VRDBEGIN)
  
  #LET OP, gewogen gemiddelde wordt nu per rij berekend!
  data_wider = data_wider %>% dplyr::rowwise() %>% dplyr::mutate(verbruik_re = sum(verbruik_aanleg * re_gewogen_VRDAANLEG, sum_product_VRDBEGIN * re_gewogen_VRDBEGIN) / verbruik)
  data_wider = data_wider %>% dplyr::rowwise() %>% dplyr::mutate(verbruik_vem = sum(verbruik_aanleg * vem_gewogen_VRDAANLEG, sum_product_VRDBEGIN * vem_gewogen_VRDBEGIN) / verbruik)
  
  data_output = data_wider %>% select(kvk_nummer, jaartal, feedtype, vem_categorie, verbruik, verbruik_re, verbruik_vem)
  
  data_output = pivot_wider(data_output, names_from = c("feedtype", "vem_categorie"), values_from = c("verbruik", "verbruik_re", "verbruik_vem"))
  
  return(data_output)
  
}


if(FALSE){
  path_xml_files = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKO_2020/Voerwinst/InputXMLtest"
  data_xml_test = XMLtoDataFrame(path_xml_files) 
}





