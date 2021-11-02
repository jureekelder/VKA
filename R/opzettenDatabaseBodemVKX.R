#Functie voor samenvoegen KLW data en grondmonsters 

#Jur Eekelder; 23-08-2021
#jur.eekelder@demarke.eu
#Script voor het toeoegen van extra kengetaleln aan KLW databases.

#INPUTS
#path_to_database_KLW --> dataframe met KLW kengetallen
#path_to_database_VKX --> locatie naar map voor VKX ledenlijst (hierin staan klantnummers Dumea / Eurofins / anders)
#path_to_database_Bodem --> locatie naar map met bodemanalyses (samengevoegd al vanuit Eurofins en Dumea)
#output_path --> locatie naar map waar resultaat mag staan

#VOOR TESTEN
#path_to_database_KLW  = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Bodemanalyses/Database_KLW"
#path_to_database_Bodem = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Bodemanalyses/Database_Bodem"
#output_path = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Bodemanalyses/Figuren"


#plaatje C/N vs Nitraat voor BES boeren.
#

opzettenDatabaseBodemVKX <- function(path_to_database_KLW, path_to_database_Bodem, output_path){

  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Het laden van benodigde functies die ook op GIT staan.
  library(devtools)
  scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R")
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  
  #Inlezen van datasets voor alle inputs
  input_settings = matrix(ncol = 2, byrow = T, data =  c("KLW", path_to_database_KLW, "Bodem", path_to_database_Bodem))
  
  for(i in 1:nrow(input_settings)){
    
    path_to_dataset = input_settings[i,2]
    dataset_name = input_settings[i,1]
    
    if(file.exists(path_to_dataset)){
      data_temp = getDataInFolder(path_to_dataset)
      
      #Zitten er dubbele data in de dataset?
      data_temp_duplicates = duplicated(data_temp)
      
      if (any(data_temp_duplicates)) {
        outputToLog("Dubbele  data in bestand: ", NULL)
        
        data_temp = distinct(data_temp, .keep_all = T)
      }
      
      assign(paste("data_",dataset_name,sep = ""),data_temp)
      
    } else {
      stop(paste("Path naar dataset", dataset_name, "bestaat niet!", sep = " "))
    }
    
  }
  
  
  #Zitten er dubbele KringloopWijzers in de dataset?
  data_KLW_input_duplicates = duplicated(data_KLW %>% dplyr::select(jaartal, PK_KLW))
  
  if (any(data_KLW_input_duplicates)) {
    outputToLog("Dubbele KLW data in bestand: ", data_KLW[data_KLW_input_duplicates, c("naaminv", "jaartal")])
    
    data_KLW = distinct(data_KLW, jaartal, PK_KLW, .keep_all = T)
  }
  
  
  #Bestaat de output folder
  if(file.exists(output_path)){
    setwd(output_path)
  } else {
    warning("Path naar voor output bestaat niet!")
  }
  
  #Verwerken input Eurofins
  kolommen_eurofins = c(
    "Klantnummer",
    "Postcode",
    "Huisnummer",
    "Omschrijving",
    "Monsternamedatum",
    "DiepteTot",
    "Gewas1",
    "Ntotaal",
    "Cnratio",
    "NLV",
    "PPAE",
    "PAL",
    "PW",
    "pH",
    "Corg",
    "OS",
    "COSratio",
    "CEC"
  )
  
  if(all(kolommen_eurofins %in% colnames(data_Bodem))){
    data_Bodem = data_Bodem[, kolommen_eurofins]
  } else {
    stop("Eurofinsbestand heeft niet de juiste headers")
  }
  
  #Toevoegen PK bodemdata
  
  #Functie om getallen uit string te krijgen.
  getNumbersFromString <- function(x) {
    elements = unlist(strsplit(x, ""))
    numberString = NULL
    
    for (e in elements) {
      suppressWarnings({
        number = as.numeric(e)
      })
      if (is.na(number)) {
        
      } else {
        numberString = paste(numberString, number, sep = "")
      }
      
    }
    
    if (is.null(numberString)) {
      return(NA)
    } else {
      return(numberString)
    }
    
  }
  
  #Functie om letters uit string te krijgen.
  getCharactersFromString <- function(x) {
    elements = unlist(strsplit(x, ""))
    characterString = NULL
    
    for (e in elements) {
      suppressWarnings({
        number = as.numeric(e)
      })
      if (is.na(number)) {
        characterString = paste(characterString, e, sep = "")
      } else {
      }
    }
    
    if (is.null(characterString)) {
      return(NA)
    } else {
      return(characterString)
    }
  }

  
  #Functie om PK te maken van postcode en huisnummer
  maak_PK <- function(postcode, huisnummer) {

    postcode_temp = gsub(" ", "", postcode, fixed = T) 
    postcode_temp = toupper(postcode_temp)
    
    huisnummer_temp = as.numeric(huisnummer)#getNumbersFromString(huisnummer)
    
    PK = paste(postcode_temp, huisnummer_temp, sep = "")

    return(PK)
  }
  
  #Toevoegen van datum in bodemdata
  library(lubridate)
  library(dplyr)
  
  data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(datum_object = as.Date(as.character(Monsternamedatum), format = "%Y%m%d"))
  data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(bodem_maand = month(datum_object), bodem_jaar = year(datum_object))
  
  #Bodemanalyses in het najaar worden bij het volgende teeltjaar gezet. Dus "eerst" komt bodemanalyse en dan "KLW / teeltjaar".
  data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(jaartal = ifelse(bodem_maand < 7, bodem_jaar, bodem_jaar + 1))
  
  if(FALSE){
    data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(jaartal = jaartal - 1)
  }
  
  bepaalGewasEurofins <- function(x){
    
    gewas = "Overig"
    if(grepl("Gras", x, ignore.case = T)){
      gewas = "Gras"
    } else if (grepl("Snij", x, ignore.case = T)){
      gewas = "Mais"
    }
    
    return(gewas)
  }
  data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(gewas_kort = bepaalGewasEurofins(Gewas1))
  
  #Observaties bodemdata
  jaartal_min = min(data_Bodem_samengevat$jaartal, na.rm = T)
  jaartal_max = max(data_Bodem_samengevat$jaartal, na.rm = T)
  
  data_Bodem_meta = data_Bodem %>% group_by(jaartal) %>% dplyr::summarise(count = n())
  
  data_Bodem_samengevat = data_Bodem %>% 
    dplyr::group_by(jaartal, gewas_kort) %>% 
    dplyr::summarise(mean_cn = mean(Cnratio, na.rm = T), sd_cn = sd(Cnratio, na.rm = T),
                     mean_nlv = mean(NLV, na.rm = T), sd_nlv = sd(NLV, na.rm = T),
                     mean_os = mean(OS, na.rm = T), sd_os = sd(OS, na.rm = T))
  
  plot = ggplot(data = data_Bodem_samengevat, aes(x = jaartal, y = mean_cn )) +
    theme_bw() +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.9, aes(fill = gewas_kort)) +
    geom_errorbar(aes(ymin = mean_cn - sd_cn, ymax = mean_cn + sd_cn, group = gewas_kort), position = position_dodge(0.9), width = 0.6, size = 0.5) +
    theme(legend.text  = element_text("")) +
    scale_x_continuous(breaks = seq(jaartal_min, jaartal_max, 1)) +
    coord_cartesian(ylim = c(8,18), expand = F) +
    xlab("Jaar") +
    geom_smooth(se = FALSE, linetype = "dashed", aes(color = gewas_kort)) +
    ylab("C/N ratio [-]") +
    guides(fill = guide_legend(title = "Landgebruik")) + 
    guides(color = "none", linetype = "none") 
  print(plot)
  ggsave( "cn_ratio_verloop.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = data_Bodem_samengevat, aes(x = jaartal, y = mean_os )) +
    theme_bw() +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.9, aes(fill = gewas_kort)) +
    geom_errorbar(aes(ymin = mean_os - sd_os, ymax = mean_os + sd_os, group = gewas_kort), position = position_dodge(0.9), width = 0.6, size = 0.5) +
    theme(legend.text  = element_text("")) +
    scale_x_continuous(breaks = seq(jaartal_min, jaartal_max, 1)) +
    coord_cartesian(ylim = c(2,10), expand = F) +
    xlab("Jaar") +
    geom_smooth(se = FALSE, linetype = "dashed", aes(color = gewas_kort)) +
    ylab("OS [%]") +
    guides(fill = guide_legend(title = "Landgebruik")) + 
    guides(color = "none", linetype = "none") 
  print(plot)
  ggsave( "os_verloop.png", width = 20, height = 12, units = "cm")
  
  
  plot = ggplot(data = data_Bodem_samengevat, aes(x = jaartal, y = mean_nlv )) +
    theme_bw() +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.9, aes(fill = gewas_kort)) +
    geom_errorbar(aes(ymin = mean_nlv - sd_nlv, ymax = mean_nlv + sd_nlv, group = gewas_kort), position = position_dodge(0.9), width = 0.6, size = 0.5) +
    theme(legend.text  = element_text("")) +
    scale_x_continuous(breaks = seq(jaartal_min, jaartal_max, 1)) +
    coord_cartesian(ylim = c(50,200), expand = F) +
    xlab("Jaar") +
    geom_smooth(se = FALSE, linetype = "dashed", aes(color = gewas_kort)) +
    ylab("NLV [kg N / ha]") +
    guides(fill = guide_legend(title = "Landgebruik")) + 
    guides(color = "none", linetype = "none") 
  print(plot)
  ggsave( "nlv_verloop.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = data_Bodem, aes(x = NLV, y = Cnratio, color = gewas_kort)) +
    geom_point()
  print(plot)
  
  #### KOPPELEN AAN KLW DATA
  
  #Toevoegen van PK aan bodemdata
  data_Bodem = data_Bodem %>% rowwise() %>% dplyr::mutate(PK_Bodem = maak_PK(Postcode, Huisnummer))
  data_Bodem$PK = data_Bodem$PK_Bodem
  data_KLW$PK = data_KLW$PK_VKX
  
  #Samenvatten van bodemgegevens per boer per jaar
  data_Bodem_Boer_Jaar = data_Bodem %>% dplyr::group_by(PK, jaartal, gewas_kort) %>% dplyr::summarise_all(mean)
  data_Bodem_Boer_Jaar = data_Bodem_Boer_Jaar %>% dplyr::mutate(jaar_categorie = ifelse(jaartal > 2016, "2017-2021", "2013-2016"))
  
  #Groep 1 = t/m dec 2015
  #Groep 2 = 1 jan 2016 tot heden
  #alles omrekenen naar 25 cm.
  
  #KLW 16-20, bodemtoestand op opbrengstniveau.
  #Klasses Lutum
  
  
  
  #Maak brede kolommen voor gras, mais, overig
  data_Bodem_Boer_Jaar_Gras = data_Bodem_Boer_Jaar %>% filter(gewas_kort == "Gras")
  data_Bodem_Boer_Jaar_Mais = data_Bodem_Boer_Jaar %>% filter(gewas_kort == "Mais")
  
  data_Bodem_Boer_Jaar_Alles = data_Bodem %>% dplyr::group_by(PK, jaartal) %>% dplyr::summarise_all(mean)
  
  data_Bodem_Boer_Jaar_Gras_Mais = full_join(data_Bodem_Boer_Jaar_Gras, data_Bodem_Boer_Jaar_Mais, by = c("jaartal", "PK"), suffix = c(".Gras", ".Mais"))
  data_Bodem_Boer_Jaar_Gras_Mais_Alles = full_join(data_Bodem_Boer_Jaar_Gras_Mais, data_Bodem_Boer_Jaar_Alles, by =c("jaartal", "PK"), suffix = c("", ".Alles") )
  
  write.xlsx(data_Bodem_Boer_Jaar_Gras_Mais_Alles, "data_VKA_Bodem.xlsx", asTable = F, overwrite = T)
  
  #JOIN bodemdata en KLW data op basis van postcode huisnummer
  data_Bodem_KLW = full_join(data_Bodem_Boer_Jaar_Gras_Mais_Alles, data_KLW, by = c("PK", "jaartal"))
  
  write.xlsx(data_Bodem_KLW, "data_VKA_Bodem_KLW.xlsx", asTable = F, overwrite = T)
  
  
  outputToLog("Aantal unieke klantnummers in Volledige Bodemanalyse data", length(unique(data_Bodem$Klantnummer)))
  outputToLog("Aantal unieke leden in Volledige KLW data", length(unique(data_KLW$PK_VKX)))
  
  data_Bodem_KLW = data_Bodem_KLW %>% filter(!is.na(ID_KLW))
  
  outputToLog("Aantal matches klantnummers in KLW-bodem data", length(unique(data_Bodem_KLW$PK_VKX)))

  plot = ggplot(data = data_Bodem_KLW, aes(x = over_bod_gras1, y = Cnratio.Gras, color = as.factor(jaartal))) +
    geom_point() +
    geom_smooth(method = lm, se = F) 
    
  print(plot)
  
  
  plot = ggplot(data = data_Bodem_KLW, aes(x = over_bod_mais1, y = Cnratio.Mais)) +
    geom_point()
  print(plot)
  
  
  plot = ggplot(data = data_Bodem_KLW, aes(x = verl_bodbal1_ha, y = NLV, color = as.factor(jaartal))) +
    geom_point()  +
    geom_smooth(method = lm, se = F) 
  print(plot)
  
  library(GGally)
  
  data_Bodem_KLW = ungroup(data_Bodem_KLW)
  plot = ggpairs(data = data_Bodem_KLW %>% select(grondsoort, over_bod_gras1, Cnratio.Gras, NLV.Gras, OS.Gras, Ntotaal.Gras, graspr_dmst_m3, graspr_totaal_kgn, opb_graspr_ds_per_N_bemest  ), aes(colour = as.factor(grondsoort)))
  print(plot)  
  ggsave( "correlatie_gras.png", width = 40, height = 24, units = "cm")
  
  
  plot = ggpairs(data = data_Bodem_KLW %>% select(grondsoort,over_bod_mais1, Cnratio.Mais, NLV.Mais, OS.Mais, Ntotaal.Mais, mais_dmst_m3, mais_totaal_kgn, opb_mais_ds_per_N_bemest  ), aes(colour = as.factor(grondsoort)))
  print(plot)  
  ggsave( "correlatie_mais.png", width = 40, height = 24, units = "cm")
  
  plot = ggpairs(data = data_Bodem_KLW %>% select(grondsoort,verl_bedbal1_ha, Cnratio, NLV, OS, Ntotaal ), aes(colour = as.factor(grondsoort)))
  print(plot)  
  ggsave( "correlatie_bedrijf.png", width = 40, height = 24, units = "cm")
  
}
