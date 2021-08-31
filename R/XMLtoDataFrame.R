

#      data = xmlParse(file = fileList[j])
#         tempData = xmlToDataFrame(nodes = getNodeSet(data, paste("//DUMPFILES/",sheet)))
#library(XML)

path_xml = "C:/Users/JurEekelder/OneDrive - Cooperatie De Marke U.A/DatabaseVKA/VKA/KLW 2020/Aalderink Eefde 2020.xml"

XMLtoDataFrame <- function(path_xml){
  

  
  xml_file = as_list(read_xml(path_xml))
  
}

test = XMLtoDataFrame(path_xml)

xml_df = tibble::as_tibble(test) %>% unnest_longer(KW007_Input) %>% filter(KW007_Input_id == "VRDAANLEG") %>% unnest_wider(KW007_Input)

xml_df_meta = tibble::as_tibble(test) %>% unnest_longer(KW007_Input) %>% filter(KW007_Input_id == "ALGEMEEN") %>% unnest_wider(KW007_Input) %>% unnest( cols = names(.))


names(xml_df)

krachtvoer_running = data.frame()

for(name in names(xml_df)){
    
  
    
  if(str_detect(name, "Krachtvoer")){
      
    heya = xml_df %>% select(name) %>% unnest_longer(name) %>% unnest(cols=names(.)) %>% unnest(cols=names(.))
    
    dataframe = as.data.frame(heya)
    names_dataframe = dataframe[,2]

    dataframe_transpose =t(dataframe[,1])
    
    colnames(dataframe_transpose) = names_dataframe

    krachtvoer_running = merge(krachtvoer_running, dataframe_transpose, all = T)
    
  }
  
}

