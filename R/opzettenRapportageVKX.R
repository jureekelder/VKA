#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het maken van KLW databases op basis van KLW .dmpx files ÉN ledenlijsten VKA / VKO 

#INPUTS
#path_to_dataset --> string die path naar VKX dataset weergeeft.
#output_path --> string die path naar output locatie geeft

#getDataInFolder
#toevoegenKengetallenKLW

#Voor testen:
#set.seed(1001)
#rm(list = ls())
#path_to_dataset = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Database/2017_2020"
#output_path  = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Figuren"

opzettenRapportageVKX <- function(path_to_dataset, output_path){
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Het laden van benodigde functies die ook op GIT staan.
  library(devtools)
  scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R",
                        "https://raw.githubusercontent.com/jureekelder/VKA/main/R/berekenVoerwinst.R",
                        "https://raw.githubusercontent.com/jureekelder/VKA/main/R/XMLtoDataFrame.R")
  
  
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  #Kleuren VKA
  kleur_vka_rood = rgb(167, 25, 48, maxColorValue = 255)
  kleur_vka_groen = rgb(0, 102, 67, maxColorValue = 255)
  
  kleur_vka_oranje = rgb(233, 131, 0, maxColorValue = 255)
  
  kleur_vka_geel = rgb(204, 204, 0, maxColorValue = 255)
  
  #Functie voor wegschrijven van tabellen
  standard_template_path = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Figuren/template.xlsx"
  write_to_excel <- function(df){
    
    file_name = paste("Tabel_",deparse(substitute(df)),".xlsx", sep = "")
    
    wb = loadWorkbook(standard_template_path)
    
    boldHeader <- createStyle(textDecoration = 'bold', fontName = 'corbel', fontSize = 10, fgFill  = kleur_vka_rood, fontColour = "white" ) # Makes first row bold
    boldYears <- createStyle(textDecoration = 'bold', fontName = 'calibri', fontSize = 10, fgFill = kleur_vka_groen, fontColour = "white" ) # Makes first row bold
    
    style_text = createStyle(fontSize = 10)
    
    addStyle(wb, sheet = 1, style = style_text, rows = 2:100, cols = 2:100, gridExpand = T)
    addStyle(wb, sheet = 1, style = boldYears, rows = 2:100, cols = 1, gridExpand = F)
    
    writeData(wb, sheet = 1, x = df,  headerStyle = boldHeader)
    saveWorkbook(wb, file_name, overwrite = T)
    
  }
  
  
  #Functie om kolommen af te ronden op aantal decimalen
  df_round_manual <- function(df, names_digits_matrix){
    
    #Itereer over alle kolommen in de dataset
    for(i in 1:nrow(names_digits_matrix)){
      
      #Krijg kolomnaam en aantal digits.
      col_name = names_digits_matrix[i,1]
      col_digit = as.numeric(names_digits_matrix[i,2])
      col_name_new = names_digits_matrix[i,3]
      
      #Als aantal digits niet NA is dan
      if(!is.na(col_digit)){
        df = df %>% dplyr::mutate( across(col_name, round, col_digit))
      }
      
    }
    
    col_names_old = names_digits_matrix[,1]
    col_names_new = names_digits_matrix[,3]
    
    df = df %>% dplyr::rename_at(vars(col_names_old), ~col_names_new)
    
    return(df)
    
  }
  
  #Functie om het gemiddelde te bereken voor sommige kolommen
  bereken_gemiddelde_over_jaren <- function(df, names_digits_matrix){
    
    #df[sapply(df, is.infinite)] <- NA
    
    output = df %>% 
      dplyr::select(c("jaartal",names_digits_matrix[,1])) %>%
      dplyr::group_by(jaartal) %>%
      dplyr::summarise_all(mean, na.rm = T)
    
    output_rounded = df_round_manual(output, names_digits_matrix)
    
    names_df = colnames(output_rounded)
    if(any(str_detect(names_df, "Aandeel"))){
      
      for(name_df in names_df){
        
        if(str_detect(name_df, "Aandeel")){
          output_rounded = output_rounded %>% dplyr::mutate(across(name_df, ~.x * 100))
        }
        
      }
      
    }
    
    return(output_rounded)
    
  }
  
  krijg_KLW_kengetal_naam <- function(kengetal, tabel){
    
    rownames(tabel) = tabel[,1]
    kolomnaam = tabel[kengetal, 3]
    return(as.character(kolomnaam))
    
  }
  
  
  #Laden libraries
  library(dplyr)  
  library(openxlsx)
  library(ggplot2)
  library(dplyr)
  options(scipen = 999) #Do not use 5e09 etc for large numbers.
  library(reshape2)
  library(reshape)
  library(ggforce)
  library(tidyr)
  library(scales) #voor pretty_breaks()
  
  #Inlezen van dataset
  if(file.exists(path_to_dataset)){
    dataset_VKX = getDataInFolder(path_to_dataset)
  } else {
    stop("Path naar dataset bestaat niet!")
  }
  
  if(file.exists(output_path)){
    setwd(output_path)
  } else {
    warning("Path naar voor output bestaat niet!")
  }
  
  #Wat is de working directory:
  outputToLog("Working Directory is", getwd())
  
  jaartallen = unique(dataset_VKX$jaartal)
  
  outputToLog("Functie gestart --> opzetten Rapportage", NULL)
  outputToLog("Voor jaartallen", jaartallen)
  
  outputToLog("Aantal observaties", nrow(dataset_VKX))
  
  aantal_bedrijven = round(nrow(dataset_VKX) / length(jaartallen), 0)
  outputToLog("Aantal bedrijven", aantal_bedrijven)
  
  #### HANDMATIG TOEVOEGEN MISSENDE KENGETALLEN ####
  
  if(TRUE){
    dataset_VKX$jaartal_factor = as.factor(dataset_VKX$jaartal)
    
    #Handmatig toevoegen ontbrekende kengetallen
    dataset_VKX = dataset_VKX %>% mutate(bodem_type_zand = ifelse(grondsoort == "zand", 1, 0))
    dataset_VKX = dataset_VKX %>% mutate(bodem_type_klei = ifelse(grondsoort == "klei", 1, 0))
    dataset_VKX = dataset_VKX %>% mutate(bodem_type_veen = ifelse(grondsoort == "veen", 1, 0))
    
    dataset_VKX = dataset_VKX %>% dplyr::mutate(gr_verbruik_voerhek = ifelse(gr_verbruik > 0, gr_verbruik / 1.0, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(gk_verbruik_voerhek = ifelse(gk_verbruik > 0, gk_verbruik / 0.95, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(sm_verbruik_voerhek = ifelse(sm_verbruik > 0, sm_verbruik / 0.95, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(ov_verbruik_voerhek = ifelse(ov_verbruik > 0, ov_verbruik / 0.97, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(kv_verbruik_voerhek = ifelse(kv_verbruik > 0, kv_verbruik / 0.98, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(mp_verbruik_voerhek = ifelse(mp_verbruik > 0, mp_verbruik / 0.98, 0))
    
    #Verbruik producten zoals het in de kuil gaat --> vóór conserveringsverliezen
    dataset_VKX = dataset_VKX %>% dplyr::mutate(gr_verbruik_kuil = ifelse(gr_verbruik_voerhek > 0, gr_verbruik_voerhek / 1.0, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(gk_verbruik_kuil = ifelse(gk_verbruik_voerhek > 0, gk_verbruik_voerhek / 0.90, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(sm_verbruik_kuil = ifelse(sm_verbruik_voerhek > 0, sm_verbruik_voerhek / 0.96, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(ov_verbruik_kuil = ifelse(ov_verbruik_voerhek > 0, ov_verbruik_voerhek / 0.96, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(kv_verbruik_kuil = ifelse(kv_verbruik_voerhek > 0, kv_verbruik_voerhek / 1.0, 0))
    dataset_VKX = dataset_VKX %>% dplyr::mutate(mp_verbruik_kuil = ifelse(mp_verbruik_voerhek > 0, mp_verbruik_voerhek / 1.0, 0))
    
    
    #Rantsoenaandeel; overig en melkpoeder optellen
    dataset_VKX = dataset_VKX %>% mutate(ov_mp_aandeel = ov_aandeel + mp_aandeel)
    dataset_VKX = dataset_VKX %>% mutate(vg_aandeel_boolean = ifelse(gr_aandeel > 0, 1, 0))
    
    
    #OPPERVLAKTES
    dataset_VKX$opp_maisNA = ifelse(dataset_VKX$opp_mais > 0, dataset_VKX$opp_mais, NA)
    dataset_VKX$opp_natuurNA = ifelse(dataset_VKX$opp_natuur > 0, dataset_VKX$opp_natuur, NA)
    dataset_VKX$opp_overigNA = ifelse(dataset_VKX$opp_overig > 0, dataset_VKX$opp_overig, NA)
    
    dataset_VKX$opp_maisboolean = ifelse(is.na(dataset_VKX$opp_maisNA),0,1)
    dataset_VKX$opp_natuurboolean = ifelse(is.na(dataset_VKX$opp_natuurNA),0,1)
    dataset_VKX$opp_overigboolean = ifelse(is.na(dataset_VKX$opp_overigNA),0,1)
    
    #RANTSOEN
    dataset_VKX$ov_mp_aandeel = dataset_VKX$ov_aandeel + dataset_VKX$mp_aandeel
    
    dataset_VKX$gr_aandeelNA = ifelse(dataset_VKX$gr_aandeel > 0, dataset_VKX$gr_aandeel, NA)
    dataset_VKX$gr_aandeelboolean = ifelse(is.na(dataset_VKX$gr_aandeelNA),0,1)
    
    dataset_VKX$sm_aandeelNA = ifelse(dataset_VKX$sm_aandeel > 0, dataset_VKX$sm_aandeel, NA)
    dataset_VKX$sm_aandeelboolean = ifelse(is.na(dataset_VKX$sm_aandeelNA),0,1)
    
    dataset_VKX$ov_mp_aandeelNA = ifelse(dataset_VKX$ov_mp_aandeel > 0, dataset_VKX$ov_mp_aandeel, NA)
    dataset_VKX$ov_mp_aandeelboolean = ifelse(is.na(dataset_VKX$ov_mp_aandeelNA),0,1)
    
    #AANKOOP AANLEG
    dataset_VKX$aankoop_aanleg_gk_hoevNA = ifelse(dataset_VKX$aankoop_aanleg_gk_hoev > 0 , dataset_VKX$aankoop_aanleg_gk_hoev, NA)
    dataset_VKX$aankoop_aanleg_gk_hoevboolean = ifelse(is.na(dataset_VKX$aankoop_aanleg_gk_hoevNA), 0,1)
    
    dataset_VKX$aankoop_aanleg_sm_hoevNA = ifelse(dataset_VKX$aankoop_aanleg_sm_hoev > 0 , dataset_VKX$aankoop_aanleg_sm_hoev, NA)
    dataset_VKX$aankoop_aanleg_sm_hoevboolean = ifelse(is.na(dataset_VKX$aankoop_aanleg_sm_hoevNA), 0,1)
    
    dataset_VKX$aankoop_aanleg_ov_hoevNA = ifelse(dataset_VKX$aankoop_aanleg_ov_hoev > 0 , dataset_VKX$aankoop_aanleg_ov_hoev, NA)
    dataset_VKX$aankoop_aanleg_ov_hoevboolean = ifelse(is.na(dataset_VKX$aankoop_aanleg_ov_hoevNA), 0,1)
    
    dataset_VKX$aankoop_aanleg_kv_hoevNA = ifelse(dataset_VKX$aankoop_aanleg_kv_hoev > 0 , dataset_VKX$aankoop_aanleg_kv_hoev, NA)
    dataset_VKX$aankoop_aanleg_kv_hoevboolean = ifelse(is.na(dataset_VKX$aankoop_aanleg_kv_hoevNA), 0,1)
    
    dataset_VKX$aankoop_aanleg_gk_hoev_rantsoen = ifelse(is.na(dataset_VKX$aankoop_aanleg_gk_hoevNA), NA, dataset_VKX$aankoop_aanleg_gk_hoev / dataset_VKX$gk_verbruik_kuil * 100)
    dataset_VKX$aankoop_aanleg_sm_hoev_rantsoen = ifelse(is.na(dataset_VKX$aankoop_aanleg_sm_hoevNA), NA, dataset_VKX$aankoop_aanleg_sm_hoev / dataset_VKX$sm_verbruik_kuil * 100)
    dataset_VKX$aankoop_aanleg_ov_hoev_rantsoen = ifelse(is.na(dataset_VKX$aankoop_aanleg_ov_hoevNA), NA, dataset_VKX$aankoop_aanleg_ov_hoev / dataset_VKX$ov_verbruik_kuil * 100)
    dataset_VKX$aankoop_aanleg_kv_hoev_rantsoen = ifelse(is.na(dataset_VKX$aankoop_aanleg_kv_hoevNA), NA, dataset_VKX$aankoop_aanleg_kv_hoev / dataset_VKX$kv_verbruik_kuil * 100)
    
    
    #AMMONIAK
    dataset_VKX$em_nh3_tonmelk_stalopslag = dataset_VKX$em_nh3_stal / dataset_VKX$melkprod * 1000
    dataset_VKX$em_nh3_tonmelk_drijfmest =  (dataset_VKX$em_nh3_ombouw	+ dataset_VKX$em_nh3_omgras) / dataset_VKX$melkprod * 1000
    dataset_VKX$em_nh3_tonmelk_kunstmest =  (dataset_VKX$em_nh3_kmbouw	+ dataset_VKX$em_nh3_kmgras) / dataset_VKX$melkprod * 1000
    dataset_VKX$em_nh3_tonmelk_beweid = dataset_VKX$em_nh3_beweid / dataset_VKX$melkprod * 1000
    
    #FOSFAAT
    dataset_VKX$kring2_bodafv = dataset_VKX$kring2_bodafv_gk + dataset_VKX$kring2_bodafv_vg + dataset_VKX$kring2_bodafv_sm + dataset_VKX$kring2_bodafv_ov
    
  }
  
  
  #### DEEL 1 : BEDRIJFSONTWIKKELING ####
  
  parameters_bedrijf = matrix(ncol = 3, byrow = T, data = (c(
    "opp_totaal", 1, "Oppervlakte [ha]",
    "nkoe", 0, "Aantal melkkoeien [-]",
    "njongvee", 0, "Aantal jongvee [-]",
    "jvper10mk", 1, "Jongvee / 10 melkkoeien [-]",
    "melkprod", 0 , "Melkproductie bedrijf [kg]",
    "melkperha", 0, "Intensiteit [kg melk / ha]",
    "fpcmperha", 0, "Intensiteit [kg FPCM / ha]",
    "gveperha", 1, "Veebezetting [GVE / ha]",
    "bodem_type_zand", 2, "Aandeel bedrijven op zandgrond [%]",
    "bodem_type_veen", 2, "Aandeel bedrijven op veengrond [%]",
    "bodem_type_klei", 2, "Aandeel bedrijven op kleigrond [%]")))
  
  dataset_VKX_gemiddeld_bedrijf = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_bedrijf)
  write_to_excel(dataset_VKX_gemiddeld_bedrijf)
  
  parameters_jongvee = matrix(ncol = 3, byrow = T, data = c(
    "jvper10mk", 1, "Jongvee / 10 melkkoeien [-]",
    "njvtienmkdrieboolean", 2, "Aandeel bedrijven eigen opfok [%]",
    "njvtienmkdrie", 1, "Jongvee / 10 melkkoeien eigen opfok [-]"))
  
  dataset_VKX_gemiddeld_jongvee = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_jongvee)
  write_to_excel(dataset_VKX_gemiddeld_jongvee)
  
  #Plot melkproductie en intensiteit
  variables_in_plot = c("melkprod", "melkperha")
  data_plot_melkprod_intensiteit = dataset_VKX %>% select(jaartal_factor, variables_in_plot) %>% pivot_longer( cols = c("melkprod", "melkperha"), names_to = "Grootheid", values_to = "Waarde")
  data_plot_melkprod_intensiteit_samengevat = data_plot_melkprod_intensiteit %>% group_by(jaartal_factor, Grootheid) %>% dplyr::summarise(Gemiddelde = mean(Waarde, na.rm = T), SD = sd(Waarde, na.rm = T))
  data_plot_melkprod_intensiteit_samengevat$Grootheid <- factor(data_plot_melkprod_intensiteit_samengevat$Grootheid, levels = c("melkprod", "melkperha"),
                    labels = c(krijg_KLW_kengetal_naam("melkprod", parameters_bedrijf), krijg_KLW_kengetal_naam("melkperha", parameters_bedrijf) )
  )
  
  plot = ggplot(data = data_plot_melkprod_intensiteit_samengevat, aes(x = jaartal_factor, y = Gemiddelde, fill = Grootheid, color = Grootheid )) +
    theme_bw() +
    #geom_bar(position = position_dodge(), stat = "identity") +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = Gemiddelde - SD, ymax = Gemiddelde + SD), position = position_dodge(0.6), width = 0.4, size = 0.6) +
    ylab("") +
    xlab("") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    facet_wrap(~Grootheid, scales = "free_y") +
    theme(legend.position = "none") +
    scale_color_manual(values = c(kleur_vka_rood, kleur_vka_groen)) +
    xlab("Jaartal") +
    theme( strip.background = element_rect( color="black", fill = "white", size=1.5, linetype="solid" ) ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  print(plot)
  ggsave( "Melkproductie_Intensiteit.png", width = 17, height = 10, units = "cm")

  plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = jvper10mk, fill = jaartal_factor)) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Jongvee per 10 melkkoeien [-]") +
    geom_violin(aes(color = jaartal_factor)) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = pretty_breaks(n = 6),labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(0, 12), expand = F) +
    geom_hline(yintercept = 3, size = 1, color ="red") +
    #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
    stat_summary( fun = mean,
                  geom = "point",
                  size = 4,
                  color = "black",
                  na.rm = T
    ) +
    stat_summary( aes(y = jvper10mk, group =1),
                  fun = mean,
                  geom = "line",
                  size = 1,
                  color = "black",
                  na.rm = T
    ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1) +
    scale_fill_manual(values = alpha(c(kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood), 0.6)) +
    scale_color_manual(values = alpha(c(kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood,kleur_vka_groen, kleur_vka_rood), 0.6))
  print(plot)
  ggsave( "jongveebezetting.png", width = 17, height = 10, units = "cm")
  
  #BEWEIDING
  dataset_VKX$dagenweidenmelkkoeien = dataset_VKX$dgnweidb + dataset_VKX$dgnweido + dataset_VKX$dgncombib + dataset_VKX$dgncombio
  dataset_VKX$dagenweidenmelkkoeienNA = ifelse(dataset_VKX$dagenweidenmelkkoeien < 1, NA, dataset_VKX$dagenweidenmelkkoeien)
  
  dataset_VKX$urenweidenmelkkoeien = dataset_VKX$dgnweidb * dataset_VKX$uurweidb + dataset_VKX$dgnweido * dataset_VKX$uurweido + dataset_VKX$dgncombib * dataset_VKX$uurcombib + dataset_VKX$dgncombio * dataset_VKX$uurcombio
  dataset_VKX$urenweidenmelkkoeienNA = ifelse(dataset_VKX$urenweidenmelkkoeien < 0.1, NA, dataset_VKX$urenweidenmelkkoeien)
  
  dataset_VKX$dgnweidpiNA = ifelse(dataset_VKX$dgnweidpi < 0.1, NA, dataset_VKX$dgnweidpi)
  
  dataset_VKX$urenweidenmelkkoeienperdag = dataset_VKX$urenweidenmelkkoeien / dataset_VKX$dagenweidenmelkkoeien
  dataset_VKX$urenweidenmelkkoeienperdag = ifelse(is.nan(dataset_VKX$urenweidenmelkkoeienperdag),0,dataset_VKX$urenweidenmelkkoeienperdag)
  dataset_VKX$urenweidenmelkkoeienperdagNA = ifelse(dataset_VKX$urenweidenmelkkoeienperdag < 0.1, 0, dataset_VKX$urenweidenmelkkoeienperdag)
  
  dataset_VKX$beweidenmelkkoeienboolean = ifelse(is.na(dataset_VKX$dagenweidenmelkkoeienNA), 0 ,1)
  
  parameters_beweiding = matrix(ncol = 3, byrow = T, data = c( 
    "urenweidenmelkkoeienNA", 0, "Beweiding weidebedrijven [uren]",
    "beweidenmelkkoeienboolean", 2, "Aandeel bedrijven beweiding [%]",
    "zstvdagenNA", 0, "Zomerstalvoedering [dagen]",
    "zstvdagenboolean", 2, "Aandeel bedrijven zomerstalvoedering [%]"))
  
  dataset_VKX_gemiddeld_beweiding = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_beweiding)
  write_to_excel(dataset_VKX_gemiddeld_beweiding)
  

  
  
  
  plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = melkprod, fill = jaartal_factor)) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Melkproductie bedrijf [kg]") +
    geom_violin(aes(color = jaartal_factor)) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(0, 3000000)) +
    #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 4,
      color = "black"
    ) +
    stat_summary( aes(y = melkprod, group =1),
                  fun = mean,
                  geom = "line",
                  size = 1,
                  color = "black"
    ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 4, color = "black", vjust = -1)
  
  print(plot)
  ggsave( "melkproductie.png", width = 20, height = 12, units = "cm")
  
  
  parameters_areaal = matrix(ncol = 3, byrow = T, data = c("opp_totaal", 1, "Oppervlakte [ha]",
                                                           "oppgras", 1, "Oppervlakte grasland [ha]",
                                                           "opp_prgras", 1, "Oppervlakte productiegrasland [ha]",
                                                           "opp_natuurboolean", 2, "Aandeel bedrijven natuur [%]",
                                                           "opp_natuurNA", 1, "Oppervlakte natuur [ha]",
                                                           "opp_maisboolean", 2, "Aandeel bedrijven met eigen maisland [%]",
                                                           "opp_maisNA", 1, "Oppervlakte maisland [ha]",
                                                           "opp_overigboolean", 2, "Aandeel bedrijven met overig bouwland [%]",
                                                           "opp_overigNA", 1, "Oppervlakte overig [ha]"))
  

  dataset_VKX_gemiddeld_areaal = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_areaal)
  write_to_excel(dataset_VKX_gemiddeld_areaal)
  
  parameters_productie = matrix(ncol = 3, byrow = T, data = c(
    
    "melkpkoe", 0, "Melkproductie [kg melk / koe / jaar]",
    "fpcmkoejaar", 0, "Melkproductie [kg FPCM / koe / jaar]",
    "vet", 2, "Vetgehalte [%]",
    "eiwit", 2, "Eiwitgehalte [%]",
    "ureum", 1, "Ureumgehalte [mg/100 gr]",
    "fosfor", 0, "Fosforgehalte [mg/100 gr]"
    
  ))
  
  dataset_VKX_gemiddeld_productie = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_productie)
  write_to_excel(dataset_VKX_gemiddeld_productie)
  
  parameters_vee = matrix(ncol = 3, byrow = T , data = c(
    "efficientie_N", 1, "Stikstofefficiëntie veestapel [%]",
    "efficientie_P", 1, "Fosfaatefficiëntie veestapel [%]",
    "voereff_melk", 2, "Voerefficiëntie [kg melk / kg ds]",
    "voereff_fpcm", 2, "Voerefficiëntie [kg fpcm / kg ds]",
    
    "pcvoordeelspec1", 1, "BEX N-voordeel [%]",
    "pcvoordeelspec2", 1, "BEX P-voordeel [%]",
    "excr_spec1", 0, "Stikstofexcretie [kg/bedrijf]",
    "excr_spec2", 0, "Fosfaatexcretie [kg/bedrijf]",
    "excretie1_melk", 1, "Stikstofexcretie [kg N / ton melk]",
    "excretie2_melk", 1, "Fosfaatexcretie [kg P2O5 / ton melk]",
    "melk_excretie1", 0, "Melkproductie per N-excretie [kg/kg]",
    "melk_excretie2", 0, "Melkproductie per P2O5-excretie [kg/kg]"
    
  ))
  
  dataset_VKX_gemiddeld_vee = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_vee)
  write_to_excel(dataset_VKX_gemiddeld_vee)
  
  parameters_benutting_bedrijf = matrix(ncol =3, byrow = T, data = c("kring1_benut_tot", 0, "Stikstofbenutting bedrijf [%]",
                                                                     "kring1_bedbal_ovrtot", 0, "Stikstofoverschot bedrijf [kg N / ha]",
                                                                     "kring1_bedbal_ovrbod", 0, "Bodemoverschot",
                                                                     "kring1_bedbal_vrlnh3", 0, "Ammoniakemissie",
                                                                     "kring1_bedbal_vrln2o", 0, "Lachgasemissie",
                                                                     "kring1_bedbal_vrlnov", 0, "Overige stikstofverliezen",

                          "kring2_benut_tot", 0 ,"Fosfaatbenutting bedrijf [%]",
                          "kring2_bedbal_ovrtot", 0, "Fosfaatoverschot bedrijf [kg P2O5 / ha]",
                          "kring2_bedbal_ovrbod", 0, "Fosfaatoverschot bodem [kg P2O5 / ha]"
                          
                           
  ))
  dataset_VKX_gemiddeld_benutting_bedrijf = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_benutting_bedrijf)
  write_to_excel(dataset_VKX_gemiddeld_benutting_bedrijf)

  
  
  parameters_mest = c("N_generiek", #lage N_generiek -->  lage gebrnorm1. is dit reeel?
                      "P_generiek",
                      "dmgraasafv_ton",
                      "dmgraasafv_n",
                      "dmgraasafv_p2o5")
  
  
  parameters_bekalken = c("limemais",
                          "limeovbouw",
                          "limegras",
                          "limenatuur",
                          "dolomais",
                          "doloovbouw",
                          "dologras",
                          "dolonatuur")
  
  #### AMMONIAK ####
  
  parameters_ammoniak = matrix(ncol = 3, byrow = T, data = c(
    
    "em_nh3_bedrijf", 0, "Ammoniakemissie bedrijf [kg]",
    "em_nh3_hagrond", 1, "Ammoniakemissie [kg / ha]",
    "em_nh3_tonmelk", 2, "Ammoniakemissie [kg / ton melk]",
    "em_nh3_gve", 1, "Ammoniakemissie [kg / GVE]",
    
    "em_nh3_tonmelk_stalopslag", 2, "Stal en opslag",
    "em_nh3_tonmelk_drijfmest", 2, "Drijfmest toediening",
    "em_nh3_tonmelk_kunstmest", 2, "Kunstmest toediening",
    "em_nh3_tonmelk_beweid", 2, "Beweiden",
    
    "efficientie_N", 1, "Stikstofefficientie veestapel [%]",
    "n_excretie_mlk", 0, "Stikstofexcretie veestapel [kg N]",
    "pctan_excr_mlk", 0, "Aandel TAN [%]"
    
  ))
  
  dataset_VKX_gemiddeld_ammoniak = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_ammoniak)
  write_to_excel(dataset_VKX_gemiddeld_ammoniak)
  
  plot = ggplot(data = dataset_VKX, aes(x = efficientie_N, y = em_nh3_tonmelk)) +
    theme_bw() +
    xlab("Stikstofefficientie veestapel [%]") +
    ylab("Ammoniakemissie [kg NH3 / ton melk]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(1,5)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1)   
  print(plot)
  ggsave( "Ammoniak_versus_Efficientie.png", width = 18, height = 10, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = melkperha, y = em_nh3_hagrond)) +
    theme_bw() +
    xlab("Intensiteit [kg melk /ha]") +
    ylab("Ammoniakemissie [kg NH3 / ha]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(40,100),xlim = c(8000,35000)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1) 
  print(plot)
  ggsave( "Ammoniak_versus_Intensiteit.png", width = 18, height = 10, units = "cm")
  
  
  parameters_aankoop_voer = matrix(ncol = 3, byrow = T, data =  c(
    "aankoop_aanleg_gk_hoevboolean", 2, "Aandeel bedrijven aankoop graskuil [%]",
    "aankoop_aanleg_gk_hoev_rantsoen", 0, "Aankoop graskuil van rantsoen [%]",
    "aankoop_aanleg_sm_hoevboolean", 2, "Aandeel bedrijven aankoop snijmais [%]",
    "aankoop_aanleg_sm_hoev_rantsoen", 0, "Aankoop maiskuil van rantsoen [%]",
    "aankoop_aanleg_kv_hoevboolean", 2, "Aandeel bedrijven aankoop krachtvoer [%]",
    "aankoop_aanleg_kv_hoev_rantsoen", 0, "Aankoop krachtvoer van rantsoen [%]",
    "aankoop_aanleg_ov_hoevboolean", 2, "Aandeel bedrijven aankoop overige producten [%]",
    "aankoop_aanleg_ov_hoev_rantsoen", 0, "Aankoop overige producten van rantsoen [%]",
    "akvoer_n", 1, "Voeraankoop stikstof [kg N / ton melk]",
    "akvoer_p", 1 , "Voeraankoop fosfor [kg P / ton melk]"
  ))
  
  dataset_VKX_gemiddeld_aankoop = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_aankoop_voer)
  write_to_excel(dataset_VKX_gemiddeld_aankoop)
  
  dataset_VKX = dataset_VKX %>% mutate(jurtest = (gr_verbruik + gk_verbruik)/(opb_graspr_ds*opp_prgras))
  
  dataset_VKX = dataset_VKX %>% mutate(jurtest2 = (kring1_bedbal_aankv) * opp_totaal / melk_bedr * 100)
  dataset_VKX = dataset_VKX %>% mutate(jurtest1 = (kring1_bedbal_afvvoe) * opp_totaal / melk_bedr * 100)
  dataset_VKX = dataset_VKX %>% mutate(kring1_bedbal_afvvoeNA = ifelse(kring1_bedbal_afvvoe < 1, NA, kring1_bedbal_afvvoe))
  dataset_VKX = dataset_VKX %>% mutate(jurtest3 = jurtest2/(kring1_bedbal_afvvoeNA  * opp_totaal / melk_bedr * 100)) 

  
  data_plot = (dataset_VKX %>% filter(jaartal>2017) %>% group_by(Lidmaatschapsnummer) %>%
                 dplyr::summarise_all(mean) %>%
                 dplyr::mutate(intensiteit = cut(melkperha, c(0, 12500,15000,17500,20000,22500,25000,90000), 
                                                 labels = c("<12500","12500-15000","15000-17500","17500-20000","20000-22500","22500-25000",">25000"))))
  
  data_plot = data_plot %>% mutate(Verkoop = ifelse(is.na(jurtest3), "Incidenteel", "Structureel"))
  
  plot = ggplot(data = data_plot, aes(x = jurtest1, y = jurtest2, color = Verkoop)) +
    theme_bw() +
    xlab("Afvoer stikstof uit ruwvoer [kg N / 100 kg melk]") +
    ylab("Aanvoer stikstof uit krachtover [kg N / 100 kg melk]") +
    geom_point(size = 2) +

    #guides(color = "none") +
    scale_color_manual(values = c(kleur_vka_groen, kleur_vka_rood)) +
    theme(legend.position = "top") +
    ylim(0.5,1.25)

  print(plot)
  ggsave( "zelfvoorzienendheid.png", width = 20, height = 12, units = "cm")
  
  
  plot = ggplot(data = data_plot, aes(x = melkperha, y = kring1_bedbal_aanrv)) +
    theme_bw() +
    xlab("Intensiteit [kg melk / ha]") +
    ylab("Aanvoer stikstof uit ruwvoer en bijproducten [kg N / ha]") +
    geom_point(size = 2, color = kleur_vka_groen) +
    geom_smooth(se = FALSE, method = "lm", color = kleur_vka_rood) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) 

  print(plot)
  ggsave( "aankoop_intensiteit.png", width = 20, height = 12, units = "cm")
  
  dataset_VKX_gras_aankoop = dataset_VKX %>% filter(aankoop_aanleg_gk_hoevboolean>0) %>% group_by(PK_VKX)  %>% dplyr::summarise(count = n()) #%>% group_by(count) %>% dplyr::summarise(aantal = n())
  dataset_VKX_gras_aankoop$Product = "Gras"
  dataset_VKX_mais_aankoop = dataset_VKX %>% filter(aankoop_aanleg_sm_hoevboolean>0) %>% group_by(PK_VKX)  %>% dplyr::summarise(count = n()) #%>% group_by(count) %>% dplyr::summarise(aantal = n())
  dataset_VKX_mais_aankoop$Product = "Mais"
  
  data_histogram = rbind(dataset_VKX_gras_aankoop, dataset_VKX_mais_aankoop)

  plot = ggplot(data = data_histogram, aes( x = count, fill = Product)) +
    theme_bw() +
    geom_histogram(binwidth = 0.5, position = position_dodge(0.6),alpha = 1) +
    xlab("Aantal jaren voeraankoop") +
    ylab("Aantal bedrijven") + 
    scale_x_continuous(breaks = 1:8) +
    scale_y_continuous(breaks = pretty_breaks(n = 6)) + 
    coord_cartesian(ylim = c(0,200), expand = F) +
    scale_fill_manual(values = c(kleur_vka_groen, kleur_vka_rood)) +
    theme(legend.position = "top")
  print(plot)
  ggsave( "aankoop.png", width = 20, height = 12, units = "cm")
  

  
  parameters_afvoer_gras = matrix(ncol = 3, byrow = T, data = c(
    afv_gkp1_hoev, 0 , "Verkoop uit beginvoorraad [kg ds]",
    afv_gkp2_hoev, 1,  "Verkoop uit aanleg [kg ds]",
    afv_sm1_hoev, 0, "Verkoop uit beginvoorraad [kg ds]",
    afv_sm2_hoev, 1, "Verkoop uit aanleg [kg ds]",
    
  ))
  
  #Een suggestie om bedrijven met een ruwvoeroverschot te ontdekken:
  #Je zou het percentage eigen geteeld voer tov voerverbruik voor N, P en VEM kunnen nemen (pagina voeding) en vervolgens vergelijken met het aandeel N van eigen teelt in het rantsoen (of P of VEM). 
  #Als het percentage eigen geteeld (veel) hoger is dan het aandeel eigen teelt in het rantsoen, dan heb je waarschijnlijk een bedrijf dat veel ruwvoer (over) heeft en toch veel krachtvoer voert.
  
  

  
  #op basis van bedrijfskringloop.
  parameters_kringloop_stikstof = matrix(ncol = 3, byrow = T, data = c(
  "kring1_bedbal_aankv", 0, "Aanvoer N uit krachtvoer [kg / ha]",
  "kring1_bedbal_aanrv", 0, "Aanvoer N uit ruwvoer en bijproducten [kg / ha]",

  "kring1_minaan_vee", 0, "Aanvoer stikstof naar vee [kg / ha]",
  "kring1_min_gewvee", 0, "Doorloop N van gewas naar vee [kg / ha]"
  ))
  
  dataset_VKX_gemiddeld_kringloop_stikstof = bereken_gemiddelde_over_jaren(dataset_VKX , parameters_kringloop_stikstof)
  write_to_excel(dataset_VKX_gemiddeld_kringloop_stikstof)
  
  #gk_mut_hoev
  #sm_mut_hoev
  #ov_mut_hoev
  #kv_mut_hoev
  
  
  
  #pceigen_n
  #pceigen_p
  #pceigen_vem
  #akvoer_n
  #akvoer_P
  #eiwiteig_tlt_gk
  #eiwiteig_tlt_sm
  #eiwiteig_tlt_ov
  #eiwiteig_vbr_vg
  #eiwiteig_vbr_gk
  #eiwiteig_vbr_sm
  #eiwiteig_vbr_ov
  #eiwiteig_vbr_kv
  #eiwiteig_vbr_mp
  
  #rantsoenverbruiken en aandelen. n_opname_mlk

  
  #Maken van de overzichten en exporteren
  dataset_VKX_gemiddeld_aankoop = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_aankoop_voer)
  write_to_excel(dataset_VKX_gemiddeld_aankoop)
  
  parameters_kunstmest = c("kmaan_kg",
                           "kmaan_namm",
                           "kmaan_nnit",
                           "kmaan_nure"
  )
  
  #Eiwit eigen land
  
  parameters_eiwit_eigen_teelt = matrix(ncol = 3, byrow = T, data = c("pceigen_n", 0, "Eiwit eigen land [%]",
                                                                      "pceigen_n_buurt", 0, "Eiwit eigen land + buurt [%]",
                                                                      "pceigen_n_buurt_65_boolean", 1, "Aandeel bedrijven >65% met buurt [%]",
                                                                      "aandeel_N_behoefte", 0, "Percentage N-behoevende gewassen [%]",
                                                                      "opb_graspr_ds", 0 , "Opbrengst productiegrasland [kg ds]",
                                                                      "aanleg_gk_re", 0, "RE-gehalte productiegrasland [g/kg ds]",
                                                                      
                                                                      "urenweidenmelkkoeienNA", 0, "Beweiding weidebedrijven [uren]",
                                                                      "beweidenmelkkoeienboolean", 2, "Aandeel bedrijven beweiding [%]",
                                                                      "vg_aandeel_boolean", 2, "Aandeel bedrijven vers gras [%]"

                                                                      
  ))
  
  plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = pceigen_n_buurt, fill = jaartal_factor, color = jaartal_factor)) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Eiwit eigen land met buurtaankoop [%]") +
    geom_violin(aes(fill = jaartal_factor)) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 65, color = "red", size = 1, linetype = "dashed") +
    scale_fill_hue()  +
    scale_color_hue()  +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks()) +
    #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
    stat_summary( fun = mean, 
                  geom = "point",
                  size = 4,
                  color = "black",
                  na.rm = T
    )   +
    stat_summary( aes(y = pceigen_n_buurt, group = 1),
                  fun = mean,
                  geom = "line",
                  size = 1,
                  color = "black",
                  
                  na.rm = T , position = position_dodge(.9) 
    ) +
    stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
  
  print(plot)
  ggsave( "eigen_n_met_buurt.png", width = 20, height = 12, units = "cm")
  
  #Maken van de overzichten en exporteren
  dataset_VKX_gemiddeld_eiwit_eigen_teelt = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_eiwit_eigen_teelt)
  write_to_excel(dataset_VKX_gemiddeld_eiwit_eigen_teelt)
  
  dataset_VKX = dataset_VKX %>% mutate(eiwiteigenlandcategoriebuurt = cut(pceigen_n_buurt, c(0, 50, 55, 60, 65, 70, 75, 200), labels = c("<50","50-55","55-60","60-65","65-70","70-75", ">75")))
  dataset_VKX = dataset_VKX %>% mutate(eiwiteigenlandcategorie = cut(pceigen_n, c(0, 50, 55, 60, 65, 70, 75, 200), labels = c("<50","50-55","55-60","60-65","65-70","70-75", ">75")))
  
  
  
  data_eiwit_eigen_land_alles_buurt = dataset_VKX %>% 
    dplyr::select(jaartal, eiwiteigenlandcategoriebuurt) %>%
    dplyr::group_by(jaartal, eiwiteigenlandcategoriebuurt) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::mutate(freq = round(100 * n/ sum(n),0)) %>%
    dplyr::mutate(jaartal = as.factor(jaartal))
  
  data_eiwit_eigen_land_alles = dataset_VKX %>% 
    dplyr::select(jaartal, eiwiteigenlandcategorie) %>%
    dplyr::group_by(jaartal, eiwiteigenlandcategorie) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::mutate(freq = round(100 * n/ sum(n),0)) %>%
    dplyr::mutate(jaartal = as.factor(jaartal))
  
  data_eiwit_eigen_land_2020 = dataset_VKX %>% 
    dplyr::filter(jaartal>2019) %>%
    dplyr::select(jaartal, eiwiteigenlandcategoriebuurt) %>%
    dplyr::group_by(jaartal, eiwiteigenlandcategoriebuurt) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::mutate(freq = round(100 * n/ sum(n),0)) %>%
    dplyr::mutate(jaartal = as.factor(jaartal))
  
  data_eiwit_eigen_land_avg = dataset_VKX %>% 
    dplyr::filter(jaartal > 2017) %>%
    dplyr::select(jaartal, pceigen_n_buurt, Lidmaatschapsnummer) %>%
    dplyr::group_by(Lidmaatschapsnummer) %>%
    dplyr::summarise(pceigen_n_buurt_mean = mean(pceigen_n_buurt, na.rm = T)) %>%
    dplyr::mutate(jaartal = "2018-2020") %>%
    dplyr::mutate(eiwiteigenlandcategoriebuurt = cut(pceigen_n_buurt_mean, c(0, 45, 50, 55, 60, 65, 70, 75, 80, 200), labels = c("<45","45-50","50-55","55-60","60-65","65-70","70-75", "75-80", ">80"))) %>%
    dplyr::group_by(jaartal, eiwiteigenlandcategoriebuurt) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::mutate(freq = round(100 * n/ sum(n),0)) %>%
    dplyr::mutate(jaartal = as.factor(jaartal))
  
  
  data_eiwit_eigen_land = rbind(data_eiwit_eigen_land_2020, data_eiwit_eigen_land_avg)
  
  plot = ggplot(data = data_eiwit_eigen_land_alles_buurt, aes(x = eiwiteigenlandcategoriebuurt, y = freq, fill = as.factor(jaartal))) +
    geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75) +
    scale_y_continuous(limits=c(0,40), expand = c(0,0)) +
    xlab("Eiwit eigen land + buurtaankoop [%]") +
    ylab("Aandeel bedrijven [%]") +
    theme(text = element_text(size=8)) +
    theme_light() +
    theme(panel.grid.major.x = element_blank()) +
    theme(legend.key.size = unit(1,"line")) +
    guides(fill=guide_legend(title = "Periode"))  +
    scale_fill_manual(values=c(kleur_vka_geel,kleur_vka_groen, kleur_vka_rood, kleur_vka_oranje)) +
    theme(legend.position = "top")
  print(plot)
  ggsave("eiwit_van_eigen_land_buurt.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = data_eiwit_eigen_land_alles, aes(x = eiwiteigenlandcategorie, y = freq, fill = as.factor(jaartal))) +
    geom_bar(stat="identity", position = position_dodge(0.8), width = 0.75) +
    scale_y_continuous(limits=c(0,60), expand = c(0,0)) +
    xlab("Eiwit eigen land [%]") +
    ylab("Aandeel bedrijven [%]") +
    theme(text = element_text(size=8)) +
    theme_light() +
    theme(panel.grid.major.x = element_blank()) +
    theme(legend.key.size = unit(1,"line")) +
    guides(fill=guide_legend(title = "Periode"))  +
    scale_fill_manual(values=c(kleur_vka_geel,kleur_vka_groen, kleur_vka_rood, kleur_vka_oranje)) +
    theme(legend.position = "top")
  print(plot)
  ggsave("eiwit_van_eigen_land.png", width = 20, height = 12, units = "cm")
  
  #### RANTSOEN ####
  parameters_rantsoen_aandeel = matrix(ncol = 3, byrow = T,  data = c(
                          "gr_aandeel", 0, "Vers gras [%]",
                          "gk_aandeel", 0 , "Graskuil [%]",
                          "sm_aandeel", 0 , "Snijma?s [%]",
                          "kv_aandeel", 0 , "Krachtvoer [%]",
                          "ov_mp_aandeel", 0 , "Overige bijproducten [%]"

  ))

  factorNH3toN = (14/17)
  #verl_nh3stal_ha (in kg N)
  #verl_nh3weid_ha
  #verl_nh3bem_ha
  
  plot = ggplot(data = dataset_VKX, aes(x = (verl_nh3stal_ha / factorNH3toN), y = em_nh3_stal_ha)) +
    geom_point()
  print(plot)
  
  dataset_VKX_gemiddeld_rantsoen_aandeel = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_rantsoen_aandeel)
  write_to_excel(dataset_VKX_gemiddeld_rantsoen_aandeel)
  
  parameters_rantsoen_re_gehaltes = matrix(ncol = 3, byrow = T, data = c(
    "rants_geh_re", 0, "RE-gehalte rantsoen [g/kg ds]",
    "gr_geh_re", 0, "RE-gehalte vers gras [g/kg ds]",
    "gk_geh_re", 0, "RE-gehalte graskuil [g/kg ds]",
    "sm_geh_re", 0, "RE-gehalte maiskuil [g/kg ds]",
    "kv_geh_re", 0, "RE-gehalte krachtvoer [g/kg ds]",
    "rants_re_kvem", 0 , "RE/kVEM verhouding rantsoen [-]"
    ))
  
  dataset_VKX_gemiddeld_rantsoen_re_gehaltes = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_rantsoen_re_gehaltes)
  write_to_excel(dataset_VKX_gemiddeld_rantsoen_re_gehaltes)
  
  parameters_rantsoen_p_gehaltes = matrix(ncol = 3, byrow = T, data = c(
    "rants_geh_p", 1, "P-gehalte rantsoen [g/kg ds]",
    "gr_geh_p", 1, "P-gehalte vers gras [g/kg ds]",
    "gk_geh_p", 1, "P-gehalte graskuil [g/kg ds]",
    "sm_geh_p", 1, "P-gehalte maiskuil [g/kg ds]",
    "kv_geh_p", 1, "P-gehalte krachtvoer [g/kg ds]",
    "rants_p_kvem", 1 , "P/kVEM verhouding rantsoen [-]"
  ))
  
  dataset_VKX_gemiddeld_rantsoen_p_gehaltes = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_rantsoen_p_gehaltes)
  write_to_excel(dataset_VKX_gemiddeld_rantsoen_p_gehaltes)
  
  parameters_eiwit_eigen_teelt_bemesting = matrix(ncol = 3, byrow = T, data = c(
    "graspr_tmst_kgn", 0, "Stikstofbemesting productiegras [kg N / ha]",
    "opb_graspr_n", 0, "Stikstofopbrengst productiegras [kg N / ha]",
    "opb_graspr_re_g_kg", 0 , "RE-gehalte oogst productiegras [g/kg ds]" ,
    "mais_tmst_kgn", 0, "Stikstofbemesting mais [kg N / ha]",
    "opb_mais_n", 0, "Stikstofopbrengst mais [kg N / ha]",
    "opb_mais_re_g_kg", 0 , "RE-gehalte oogst mais [g/kg ds]" 
    
  ))
  
  dataset_VKX_gemiddeld_eiwit_eigen_teelt_bemesting = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_eiwit_eigen_teelt_bemesting)
  write_to_excel(dataset_VKX_gemiddeld_eiwit_eigen_teelt_bemesting)

  parameters_bemesting_gras = matrix(ncol =3, byrow = T, data = c(
    "graspr_dmst_m3", 0, "Organische mest [m3 / ha]",
    
    "graspr_totaal_kgn", 0, "Stikstofbemesting totaal productiegras [kg N / ha] ",
    "graspr_dmst_kgn", 0, "Stikstof uit organische mest [kg N / ha]",
    "graspr_kmst_kgn", 0, "Stikstof uit kunstmest [kg N / ha]",
    "graspr_wmst_kgn", 0, "Stikstof uit weidemest [kg N / ha]",
    
    "graspr_totaal_kgp2o5", 0, "Fosfaatbemesting productiegras totaal [kg P2O5 / ha]",
    
    "graspr_dmst_kgp2o5", 0, "Fosfaat uit drijfmest [kg P2O5 / ha]",
    "graspr_kmst_kgp2o5", 0, "Fosfaat uit kunstmest [kg P2O5 / ha]",
    "graspr_wmst_kgp2o5", 0, "Fosfaat uit weidemest [kg P2O5 / ha]"))
  
  dataset_VKX_gemiddeld_bemesting_gras = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand"), parameters_bemesting_gras)
  write_to_excel(dataset_VKX_gemiddeld_bemesting_gras)
  
  parameters_bemesting_mais = matrix(ncol =3, byrow = T, data = c(
    "mais_dmst_m3", 0, "Organische mest [m3 / ha]",
    
    "mais_totaal_kgn", 0, "Stikstofbemesting totaal mais [kg N / ha] ",
    "mais_dmst_kgn", 0, "Stikstof uit organische mest [kg N / ha]",
    "mais_kmst_kgn", 0, "Stikstof uit kunstmest [kg N / ha]",
    
    "mais_totaal_kgp2o5", 0, "Fosfaatbemesting mais totaal [kg P2O5 / ha]",
    
    "mais_dmst_kgp2o5", 0, "Fosfaat uit drijfmest [kg P2O5 / ha]",
    "mais_kmst_kgp2o5", 0, "Fosfaat uit kunstmest [kg P2O5 / ha]"))
  
  dataset_VKX_gemiddeld_bemesting_mais = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand"), parameters_bemesting_mais)
  write_to_excel(dataset_VKX_gemiddeld_bemesting_mais)
  
  
  parameters_opbrengst_gras = matrix(ncol = 3, byrow = T, data = c(
    "opb_graspr_ds", 0 , "Opbrengst productiegras [kg ds / ha]",
    "opb_graspr_vem_g_kg",  0 , "VEM productiegras [g / kg ds]",
    "opb_graspr_re_g_kg",  0 , "RE-gehalte productiegras  [g / kg ds]",
    "opb_graspr_p_g_kg",  1 , "P-gehalte productiegras  [g / kg ds]",
    "aanleg_gk_hoev", 0, "Aanleg graskuil [kg ds]",
    "aanleg_gk_vem", 0, "Aanleg graskuil VEM [g/kg]",
    "aanleg_gk_re", 0, "Aanleg graskuil RE-gehalte [g/kg]",
    "aanleg_gk_p", 1, "Aanleg graskuil P-gehalte [g/kg]"
  ))
  
  dataset_VKX_gemiddeld_opbrengst_gras = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_opbrengst_gras)
  write_to_excel(dataset_VKX_gemiddeld_opbrengst_gras)
  
  
  parameters_opbrengst_mais = matrix(ncol = 3, byrow = T, data = c(
    
    "opb_mais_ds", 0 , "Opbrengst mais [kg ds / ha]",
    "opb_mais_vem_g_kg", 0 , "VEM mais [g / kg ds]",
    "opb_mais_re_g_kg", 0 , "RE-gehalte mais [g / kg ds]",
    "opb_mais_p_g_kg", 1 , "P-gehalte mais [g / kg ds]",
    "aanleg_sm_hoev", 0, "Aanleg maiskuil [kg ds]",
    "aanleg_sm_vem", 0, "Aanleg maiskuil VEM [g/kg]",
    "aanleg_sm_re", 0, "Aanleg maiskuil RE-gehalte [g/kg]",
    "aanleg_sm_p", 1, "Aanleg maiskuil P-gehalte [g/kg]"
  ))
  
  
  plot = ggplot(data = dataset_VKX, aes(x = benut_n_bod, y = kring1_benut_bod)) +
    geom_point() 
  print(plot)
  
  parameters_stikstof_bodemoverschot = matrix(ncol = 3, byrow = T, data = c(
    "verl_bodbal1_ha", 0, "Stikstofbodemoverschot [kg N / ha]",
    "max_N_bodemoverschot", 0, "Norm stikstofbodemoverschot [kg N / ha]",
    "bodemoverschot_N_minus_norm", 0, "Stikstofbodemoverschot minus norm [kg N / ha]",
    "bodemoverschot_N_minus_norm_boolean", 2, "Aandeel bedrijven onder norm [%]",
    "benut_n_bod", 0, "Stikstofbenutting bodem [%]"
  ))

  lm.model = lm((bodemoverschot_N_minus_norm_boolean) ~  graspr_totaal_kgn + opb_graspr_ds + opb_graspr_n + graspr_totaal_kgp2o5 + mais_totaal_kgp2o5 + 
                  mais_totaal_kgn + opb_mais_ds + opb_mais_n + aandeel_zand +melkperha , data = dataset_VKX %>% filter(grondsoort == "zand"))
  summary(lm.model)
  
  df_xgboost = (dataset_VKX %>% filter(grondsoort == "zand") %>% select(bodemoverschot_N_minus_norm_boolean, graspr_totaal_kgn, graspr_totaal_kgp2o5, mais_totaal_kgp2o5, opb_graspr_n, opb_graspr_ds, 
                                         mais_totaal_kgn, opb_mais_ds, opb_mais_n,melkperha, aandeel_zand))
  
  parameters_xgboost = matrix(ncol  = 3, byrow = T, data = c(
    "aandeel_zand", 0, "Aandeel zand bedrijf [%]",
    "opb_graspr_n", 0, "Stikstofopbrengst productiegrasland [kg N / ha]",
    "opb_mais_n", 0, "Stikstofopbrengst maisland [kg N / ha]",
    "aandeel_N_behoefte", 0, "Aandeel N-behoevende gewassen [%]"
  ))
  
  tabel_totaal = rbind(parameters_opbrengst_gras, parameters_opbrengst_mais, parameters_bemesting_gras, parameters_bemesting_mais, parameters_bedrijf,parameters_xgboost, parameters_stikstof_bodemoverschot)
  
  names(df_xgboost) = krijg_KLW_kengetal_naam(names(df_xgboost), tabel_totaal)
  
  data.matrix = as.matrix(df_xgboost)
  krijg_KLW_kengetal_naam
  
  train = data.matrix[,-1]
  train = as(train, "dgCMatrix")
  test = data.matrix[,-1]
  test = as(test, "dgCMatrix")
  
  labels.stikstof = as.factor(data.matrix[,1])
  library(Ckmeans.1d.dp)
  bst = xgboost(data = train, label = labels.stikstof, nrounds = 100)
  plot = xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst)) +
    theme_bw() +
    ggtitle("") +
    xlab("") +
    guides(fill="none") +
    scale_fill_manual(values = c(kleur_vka_groen,kleur_vka_groen,kleur_vka_groen,kleur_vka_groen,kleur_vka_groen,kleur_vka_groen))
    
  print(plot)
  ggsave( "XGboost_stikstoverschot.png", width = 20, height = 12, units = "cm")
  
  #Doen voor klei en zand!
  dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand"), parameters_stikstof_bodemoverschot)
  write_to_excel(dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand)
  dataset_VKX_gemiddeld_stikstofbodemoverschotten_klei = bereken_gemiddelde_over_jaren(dataset_VKX%>% filter(grondsoort == "klei"), parameters_stikstof_bodemoverschot)
  write_to_excel(dataset_VKX_gemiddeld_stikstofbodemoverschotten_klei)
  
  dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand_norm_haal = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand", bodemoverschot_N_minus_norm_boolean > 0), parameters_stikstof_bodemoverschot)
  write_to_excel(dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand_norm_haal)
  dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand_norm_faal = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand", bodemoverschot_N_minus_norm_boolean < 1), parameters_stikstof_bodemoverschot)
  write_to_excel(dataset_VKX_gemiddeld_stikstofbodemoverschotten_zand_norm_faal)
  
  parameters_bodemoverschotten = c("verl_bodbal1_ha",
                                   "verl_bodbal2_ha",
                                   "over_bod_gras1",
                                   "over_bod_gras2",
                                   "over_bod_mais1",
                                   "over_bod_mais2",
                                   "nbodem_over",
                                   "kring1_bodover",
                                   "max_N_bodemoverschot",
                                   "bodemoverschot_N_minus_norm",
                                   "bodemoverschot_N_minus_norm_boolean",
                                   "max_N_bodemoverschot_gras",
                                   "max_N_bodemoverschot_mais"
                                   
                                   
                                   
                                   
  )
  

  dataset_VKX_gemiddeld_opbrengst_mais = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_opbrengst_mais)
  write_to_excel(dataset_VKX_gemiddeld_opbrengst_mais)
  
  plot = ggplot(data = dataset_VKX, aes(x = opb_graspr_re_g_kg, y = rants_geh_re)) +
    geom_point()
  print(plot)
  
  plot = ggplot(data = dataset_VKX, aes(x = opb_graspr_re_g_kg, y = rants_geh_re)) +
    theme_bw() +
    xlab("Productiegras ruw eiwitgehatle [g/kg ds]") +
    ylab("Ruw eiwitgehalte rantsoen [g/kg ds]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(140,180), xlim = c(140,220)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1)   
  print(plot)
  ggsave( "Eiwitproductiegras_versus_eiwitrantsoen.png", width = 10, height = 10, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = graspr_totaal_kgp2o5, y = opb_graspr_p_g_kg)) +
    theme_bw() +
    xlab("Fosfaatbemesting productiegrasland [kg P2O5 / ha]") +
    ylab("Fosforopbrengst productiegrasland [g / kg ds]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(2.5,4.5), xlim = c(40,120)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1) 
  print(plot)
  ggsave( "Fosfaatbemsting_grasland_versus_fosfor_opbrengst.png", width = 10, height = 10, units = "cm")
  
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
  
  dataset_VKX = dataset_VKX %>% rowwise() %>% dplyr::mutate(pceigen_vem_numeric = as.numeric(getNumbersFromString(pceigen_vem)))
  dataset_VKX = dataset_VKX %>% rowwise() %>% dplyr::mutate(pceigen_p_numeric = as.numeric(getNumbersFromString(pceigen_p)))
  
  
  parameters_eigen_voer = matrix(ncol = 3, byrow = T, data =c(
    "pceigen_n", 1, "Eiwit eigen land [%]",
    "pceigen_n_buurt", 1, "Eiwit eigen land met buurtaankoop [%]",
    "pceigen_p_numeric", 1, "Fosfor eigen land [%]",
    "pceigen_vem_numeric", 1, "VEM eigen land [%]"
  ))
  
  dataset_VKX_gemiddeld_eigen_voer = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_eigen_voer)
  write_to_excel(dataset_VKX_gemiddeld_eigen_voer)
  
  parameters_kuil_aanleg = c("aanleg_gk_vem",
                             "aanleg_gk_re",
                             "aanleg_gk_p",
                             "aanleg_sm_vem",
                             "aanleg_sm_re",
                             "aanleg_sm_p")
  
  
  parameters_planet_proof = c("PP_beweiding",
                              "PP_eiwit_van_eigen_land",
                              "PP_stikstof_bodemoverschot",
                              "PP_ammoniak",
                              "PP_blijvend_grasland",
                              "PP_broeikasgas")
  
  
  #### Fosfaat en BEP ####

  parameters_fosfaat_bodem = matrix(ncol = 3, byrow = T, data = c(
    "verl_bodbal2_ha", 0, "Fosfaatbodemoverschot [kg P2O5 / ha]",
    "bodemoverschot_P_norm_boolean", 2, "Aandeel bedrijven met negatief overschot [%]",
    "kring2_bodaan", 0, "Fosfaataanvoer bodem [kg P2O5 / ha]",
    "kring2_bodafv", 0, "Fosfaatafvoer bodem [kg P2O5 / ha]",
    "kring2_benut_bod", 0, "Fosfaatbenutting bodem [%]"
  ))
  
  dataset_VKX_gemiddeld_fosfaat_bodem_zand = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand"), parameters_fosfaat_bodem)
  write_to_excel(dataset_VKX_gemiddeld_fosfaat_bodem_zand)
  dataset_VKX_gemiddeld_fosfaat_bodem_klei = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "klei"), parameters_fosfaat_bodem)
  write_to_excel(dataset_VKX_gemiddeld_fosfaat_bodem_klei)
  
  parameters_BEP = matrix(ncol = 3, byrow = T, data = c(
    "fosfaatnorm_hageneriek", 0, "Fosfaatgebruiksnorm [kg P2O5 / ha]",
    "fosfaatnorm_haspecifiek3", 0, "BEP gemiddelde 3 jaar [kg P2O5 / ha]",
    "BEP_voordeel_boolean", 2, "Aandeel bedrijven met BEP voordeel [%]"
  ))
  
  dataset_VKX_gemiddeld_BEP_zand = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "zand"), parameters_BEP)
  write_to_excel(dataset_VKX_gemiddeld_BEP_zand)
  dataset_VKX_gemiddeld_BEP_klei = bereken_gemiddelde_over_jaren(dataset_VKX %>% filter(grondsoort == "klei"), parameters_BEP)
  write_to_excel(dataset_VKX_gemiddeld_BEP_klei)
  
  data = dataAggregated %>% 
    select(jaartal, BEP_voordeel_boolean, bodem_type) %>%
    group_by(jaartal, bodem_type) %>%
    summarise_all(mean) 
  
  
  #PLOT VOOR ZAND
  data_BEP_plot_zand = dataset_VKX %>% select(c("jaartal_factor", "jaartal", "grondsoort", "fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan", "kring2_bodafv")) %>% 
    filter(grondsoort == "zand") %>%
    filter(jaartal > 2014) %>%
    select(!jaartal) %>%
    group_by(jaartal_factor) %>%
    dplyr::summarise_all(mean, na.rm = T)
  
  data_BEP_plot_zand_long = pivot_longer(data_BEP_plot_zand, cols = c("fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan", "kring2_bodafv") )
  

  plot = ggplot(data = data_BEP_plot_zand_long, aes(x= jaartal_factor, y = value, fill = name)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6)+
    coord_cartesian(ylim = c(0, 100), expand = F) +
    xlab("Jaartal") +
    ylab("Fosfaat [kg / ha]") +
    theme_bw() +
    theme(legend.position="top") +
    scale_fill_manual(values = c(kleur_vka_geel, kleur_vka_groen, kleur_vka_rood, kleur_vka_oranje), labels = c("BEP", "Gebruiksnorm", "Bemesting", "Onttrekking")) +
    theme(legend.title=element_blank())
  
  print(plot)
  ggsave("Fosfaatnorm_fosfaatbemesting_zand.png", width = 20, height = 12, units = "cm")
  
  #PLOT VOOR KLEI
  data_BEP_plot_zand = dataset_VKX %>% select(c("jaartal_factor", "jaartal", "grondsoort", "fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan", "kring2_bodafv")) %>% 
    filter(grondsoort == "klei") %>%
    filter(jaartal > 2014) %>%
    select(!jaartal) %>%
    group_by(jaartal_factor) %>%
    dplyr::summarise_all(mean, na.rm = T)
  
  data_BEP_plot_klei_long = pivot_longer(data_BEP_plot_zand, cols = c("fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan", "kring2_bodafv") )
  
  
  plot = ggplot(data = data_BEP_plot_klei_long, aes(x= jaartal_factor, y = value, fill = name)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6)+
    coord_cartesian(ylim = c(0, 100), expand = F) +
    xlab("Jaartal") +
    ylab("Fosfaat [kg / ha]") +
    theme_bw() +
    theme(legend.position="top") +
    scale_fill_manual(values = c(kleur_vka_geel, kleur_vka_groen, kleur_vka_rood, kleur_vka_oranje), labels = c("BEP", "Gebruiksnorm", "Bemesting", "Onttrekking")) +
    theme(legend.title=element_blank())
  
  print(plot)
  ggsave("Fosfaatnorm_fosfaatbemesting_klei.png", width = 20, height = 12, units = "cm")
  
  
  #### BROEIKASGASSEN ####
  

  
  parameters_broeikasgassen = matrix(ncol = 3, byrow = T, data = c("dzh_co2_melkprod", 0, "Bedrijf [g CO2-eq/kg FPCM]",
                                                                   "dzh_co2_pensferm", 0, "Pens [g CO2-eq/kg FPCM]", 
                                                                   "dzh_co2_mestopsl", 0, "Mest [g CO2-eq/kg FPCM]",
                                                                   "dzh_co2_voerprod", 0, "Voerproductie [g CO2-eq/kg FPCM]",
                                                                   "dzh_co2_energie", 0, "Energie [g CO2-eq/kg FPCM]",
                                                                   "dzh_co2_aanvoer", 0, "Aanvoer [g CO2-eq/kg FPCM]"))
  
  #Plot melkproductie en intensiteit
  variables_in_plot = parameters_broeikasgassen[-1,1]
  data_plot_melkprod_intensiteit = dataset_VKX %>% select(jaartal_factor, variables_in_plot) %>% pivot_longer( cols = variables_in_plot, names_to = "Grootheid", values_to = "Waarde")
  data_plot_melkprod_intensiteit_samengevat = data_plot_melkprod_intensiteit %>% group_by(jaartal_factor, Grootheid) %>% dplyr::summarise(Gemiddelde = mean(Waarde, na.rm = T), SD = sd(Waarde, na.rm = T))
  data_plot_melkprod_intensiteit_samengevat$Grootheid <- factor(data_plot_melkprod_intensiteit_samengevat$Grootheid, levels = variables_in_plot,
                                                                labels = c( 
                                                                           krijg_KLW_kengetal_naam(parameters_broeikasgassen[2,1], parameters_broeikasgassen),
                                                                           krijg_KLW_kengetal_naam(parameters_broeikasgassen[3,1], parameters_broeikasgassen),
                                                                           krijg_KLW_kengetal_naam(parameters_broeikasgassen[4,1], parameters_broeikasgassen),
                                                                           krijg_KLW_kengetal_naam(parameters_broeikasgassen[5,1], parameters_broeikasgassen),
                                                                           krijg_KLW_kengetal_naam(parameters_broeikasgassen[6,1], parameters_broeikasgassen))
  )
  
  plot = ggplot(data = data_plot_melkprod_intensiteit_samengevat, aes(x = jaartal_factor, y = Gemiddelde, fill = Grootheid)) +
    theme_bw() +
    ylab("BKG emissie [kg CO2-eq / kg FPCM") +
    xlab("Jaartal") +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
    coord_cartesian(ylim = c(0,500), expand = F) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 10)) +
    theme(legend.position = "top") +
    scale_fill_discrete(name = "") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  print(plot)
  ggsave( "bkg_verdeling.png", width = 17, height = 10, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = voereff_fpcm, y = dzh_co2_melkprod)) +
    theme_bw() +
    xlab("Voerefficientie veestapel [kg FPCM / kg ds]") +
    ylab("BKG emissie [kg CO2-eq / FPCM]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(900,1400)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1)   
  print(plot)
  ggsave( "BKG_versus_EfficientieVoer.png", width = 10, height = 10, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = (gk_aandeel + gr_aandeel), y = dzh_co2_pensferm)) +
    theme_bw() +
    xlab("Aandeel (vers)gras in rantsoen [%]") +
    ylab("BKG emissie [kg CO2-eq / FPCM]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(300,600), xlim = c(25,65)) +
    geom_point(color = kleur_vka_groen, size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE, color = kleur_vka_rood, size = 1)   
  print(plot)
  ggsave( "BKG_versus_Gras.png", width = 10, height = 10, units = "cm")
  
  dataset_VKX$jaartal_2020_factor = ifelse(dataset_VKX$jaartal> 2019, "2020", "2013-2019")
  plot = ggplot(data = dataset_VKX, aes(x = kv_aandeel, y = co2_aanv_kvmp, color = jaartal_2020_factor, group = jaartal_2020_factor)) +
    theme_bw() +
    xlab("Aandeel krachtvoer in rantsoen [%]") +
    ylab("BKG emissie [kg CO2-eq / FPCM]") +
    scale_y_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(100,400), xlim = c(15,35)) +
    geom_point( size = 0.75) +
    scale_x_continuous(breaks = pretty_breaks(n = 6), labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    geom_smooth(method = "lm", se = FALSE,  size = 1)   +
    theme(legend.position = "top") +
    scale_color_manual(name = "Periode", values = c(kleur_vka_groen, kleur_vka_rood)) 
  print(plot)
  ggsave( "BKG_uit_kv.png", width = 17, height = 10, units = "cm")
  
  
  parameters_broeikasgassen_extra = matrix(ncol = 3, byrow = T, data = c(
  "dzh_co2_pensferm", 0, "Pens [g CO2-eq/kg FPCM]",
  "co2_pens_vg", 0 , "Vers gras",
  "co2_pens_gk", 0 , "Graskuil",
  "co2_pens_sm", 0 , "Snijmais",
  "co2_pens_nt", 0 , "Bijproducten",
  "co2_pens_kv", 0 , "Krachtvoer",
  "co2_pens_mp", 0 , "Melkproducten",
  "dzh_co2_mestopsl", 0, "Mest [g CO2-eq/kg FPCM]",
  "co2_stal_ch4_m", 0 , "Methaan mest",
  "co2_stal_n2o_d", 0 , "Lachgas",
  "co2_stal_n2o_i", 0 , "Overig mest",
  "dzh_co2_voerprod", 0, "Voerproductie [g CO2-eq/kg FPCM]",
  "co2_voer_n2o_wm", 0 , "Weidemest",
  "co2_voer_n2o_km", 0 , "Kunstmest",
  "co2_voer_n20_om", 0 , "Overig organisch",
  "co2_voer_n2o_ve", 0 , "Lachgas veen",
  "co2_voer_overig", 0 , "Overig",
  "dzh_co2_energie", 0, "Energie [g CO2-eq/kg FPCM]",
  "co2_ene_elek", 0 , "Elektriciteit",
  "co2_ene_dies", 0 , "Diesel",
  "co2_ene_ngas", 0 , "Gas",
  "co2_ene_prop", 0 , "Propaan",
  "co2_ene_olie", 0 , "Olie",
  "co2_ene_prod", 0 , "Productie",
  "dzh_co2_aanvoer", 0, "Aanvoer [g CO2-eq/kg FPCM]",
  "co2_aanv_mech", 0 , "Productie werktuigen",
  "co2_aanv_drog", 0 , "Voer drogen",
  "co2_aanv_gras", 0 , "Gras",
  "co2_aanv_mais", 0 , "Mais",
  "co2_aanv_ovbp", 0 , "Overig voer",
  "co2_aanv_kvmp", 0 , "Krachtvoer en melkpoeder",
  "co2_aanv_mest", 0 , "Organische, en kunstmest",
  "co2_aanv_vee", 0 , "Vee",
  "co2_aanv_water", 0 , "Water",
  "co2_aanv_stroo", 0 , "Strooisel",
  "co2_aanv_gwbm", 0 , "Gewasbescherming",
  "co2_aanv_afdek", 0 , "Plastic"))
  

  
  dataset_VKX_gemiddeld_bkg = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_broeikasgassen)
  write_to_excel(dataset_VKX_gemiddeld_bkg)
  
  dataset_VKX_gemiddeld_bkg_extra = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_broeikasgassen_extra)
  write_to_excel(dataset_VKX_gemiddeld_bkg_extra)
  
  #### Melk & Klimaat ####
  
  plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = dzh_co2_melkprod, fill = jaartal_factor)) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("BKG emissie [g CO2-eq / eenheid melk]") +
    theme(legend.position = "none") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(1000, 1200)) +
    stat_summary(fun = "mean", geom = "bar", 
                 alpha = .7, position = position_dodge(0.95)) +
    stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = 3 )
  print(plot)
  ggsave( "bkg_8HRK.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = dzh_co2_melkprod, fill = jaartal_factor, color = jaartal_factor)) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("BKG emissie [g CO2-eq / FPCM]") +
    geom_violin(aes(fill = jaartal_factor)) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(900, 1400)) +
    #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
    stat_summary( fun = mean, 
                  geom = "point",
                  size = 4,
                  color = "black",
                  na.rm = T
    )   +
    stat_summary( aes(y = dzh_co2_melkprod, group = 1),
                  fun = mean,
                  geom = "line",
                  size = 1,
                  color = "black",
                  
                  na.rm = T , position = position_dodge(.9) 
    ) +
    stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
  print(plot)
  ggsave( "bkg.png", width = 20, height = 12, units = "cm")
  
  
  plot = ggplot(data = dataset_VKX, aes(x = voereff_fpcm, y = dzh_co2_pensferm, color = jaartal_factor)) +
    theme_bw() +
    xlab("Aandeel krachtvoer rantsoen [%]") +
    ylab("BKG emissie aanvoer [g CO2-eq / FPCM]") +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    #theme(legend.position = "none") +
    guides(color=guide_legend(title = "Jaartal"))+
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) 
  print(plot)
  ggsave( "kv_co2.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = voereff_fpcm, y = dzh_co2_melkprod, color = jaartal_factor)) +
    theme_bw() +
    xlab("Voerfefficientie veestapel [%]") +
    ylab("BKG emissie bedrijf [g CO2-eq / FPCM]") +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    #theme(legend.position = "none") +
    guides(color=guide_legend(title = "Jaartal"))+
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE))  
    #coord_cartesian(ylim = c(900, 1300), xlim=c(22,32)) 
  
  print(plot)
  ggsave( "effN_co2.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_VKX, aes(x = melkperha, y = dzh_co2_melkprod, color = jaartal_factor)) +
    theme_bw() +
    xlab("Intensiteit [kg melk / ha ]") +
    ylab("BKG emissie bedrijf [g CO2-eq / FPCM]") +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    #theme(legend.position = "none") +
    guides(color=guide_legend(title = "Jaartal"))+
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
    coord_cartesian(ylim = c(900, 1300), xlim = c(10000,30000)) 
  print(plot)
  ggsave( "co2_int.png", width = 20, height = 12, units = "cm")
  
  #Laagste bedrijven:
  bkg_low = dataset_VKX %>% group_by(Lidmaatschapsnummer) %>% 
    dplyr::summarise(bkg_mean = mean(dzh_co2_melkprod)) %>% 
    dplyr::arrange((bkg_mean)) %>%
    dplyr::top_n(n = -20)
  
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  colorPalette = RColorBrewer::brewer.pal(9, "Set1") #gg_color_hue(4)
  colorsGGplot = gg_color_hue(3)
  options(scipen = 999)
  
  #Grafieken
  data_KLW_VKX_Compleet = dataset_VKX
  
  parameters = c(
    "melkperha",
    "melkprod",
    "rants_geh_re",
    "gk_geh_re",
    "kvper100kgmelk",
    "sm_aandeel",
    "kv_geh_re",
    "urenweidenmelkkoeienNA",
    "dzh_nh3_bedrha",
    "dzh_co2_melkprod",
    "opb_gras_ds",
    "opb_mais_ds"
  )
  library(rlang)
  for (p in parameters) {
    data = data_KLW_VKX_Compleet %>% group_by(jaartal) %>% dplyr::summarise(mean_y = mean(get(p), na.rm = T),
                                                                            sd_y = sd(get(p), na.rm = T))
    assign(paste("data_", p, sep = ""), data)
    
  }
  
  #RE-gehalte rantsoenen
  data_KLW_VKX_Compleet$rants_geh_re_categorie = cut(
    data_KLW_VKX_Compleet$rants_geh_re,
    c(100, seq(150, 175, 5), 200),
    labels = c(
      "<151",
      "151-155",
      "156-160",
      "161-165",
      "166-170",
      "171-175",
      ">175"
    )
  )
  View(data_KLW_VKX_Compleet %>% select(rants_geh_re, rants_geh_re_categorie))
  
  data_2020 = data_KLW_VKX_Compleet %>%
    filter(jaartal == 2020) %>%
    select(jaartal, rants_geh_re_categorie) %>%
    group_by(jaartal, rants_geh_re_categorie) %>%
    dplyr::summarise(n = n()) %>%
    mutate(freq = round(100 * n / sum(n), 0))
  data_2020$jaartal = as.character(data_2020$jaartal)
  
  data_gem = data_KLW_VKX_Compleet %>%
    select(Lidmaatschapsnummer, rants_geh_re) %>%
    group_by(Lidmaatschapsnummer) %>%
    dplyr::summarise_all(mean, na.rm = T)
  data_gem$rants_geh_re_categorie = cut(
    data_gem$rants_geh_re,
    c(100, seq(150, 175, 5), 200),
    labels = c(
      "<151",
      "151-155",
      "156-160",
      "161-165",
      "166-170",
      "171-175",
      ">175"
    )
  )
  data_gem$jaartal = "Gemiddeld 2018-2020"
  
  data_gem_cat = data_gem %>%
    select(jaartal, rants_geh_re_categorie) %>%
    group_by(jaartal, rants_geh_re_categorie) %>%
    dplyr::summarise(n = n()) %>%
    mutate(freq = round(100 * n / sum(n), 0))
  
  data = rbind(data_gem_cat, data_2020)
  
  #write.xlsx(data, "re_gehalte_rantsoenen_staafdiagram.xlsx",T)
  
  
  ymin = 0
  ymax = 45
  plot = ggplot(data = data, aes(
    x = rants_geh_re_categorie,
    y = freq,
    fill = as.factor(jaartal)
  )) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      width = 0.75,
      color = "black"
    ) +
    xlab("RE-gehalte rantsoen [g/kg ds]") +
    ylab("Aandeel bedrijven [%]") +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_y_continuous(
      breaks = seq(ymin, ymax, 2),
      expand = c(0, 0),
      labels = function(x)
        format(x, big.mark = ".", scientific = FALSE)
    ) +
    theme(text = element_text(size = 8)) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank()) +
    theme(legend.key.size = unit(1, "line")) +
    guides(fill = guide_legend(title = "Jaar"))  +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c(
      rgb(167, 25, 48, maxColorValue = 255),
      rgb(0, 102, 67, maxColorValue = 255)
    ))
  
  
  print(plot)
  ggsave(
    "re_gehalte_rantsoenen.png",
    width = 18,
    height = 12,
    units = "cm"
  )
  
  data_gem_re = data_KLW_VKX_Compleet %>%
    select(jaartal, rants_geh_re) %>%
    group_by(jaartal) %>%
    dplyr::summarise_all(mean, na.rm = T)
  head(data_gem_re)
  write.xlsx(data_gem_re, "re_gehalte_rantsoenen_gemiddeld.xlsx", asTable = T, overwrite = T)
  
  ymin = 8000
  ymax = 32000
  #### INTENSITEIT VIOLIN####
  plot = ggplot(data = data_KLW_VKX_Compleet, aes(
    x = as.factor(jaartal),
    y = melkperha,
    fill = as.factor(jaartal)
  )) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Intensiteit [kg melk / ha]") +
    geom_violin(aes(color = as.factor(jaartal))) +
    geom_sina(color = "black", size = 0.5) +
    theme(legend.position = "none") +
    geom_boxplot(width = 0.2,
                 size = 1,
                 outlier.shape = NA) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 2,
      color = "red"
    ) +
    coord_cartesian(ylim = c(ymin, ymax), expand = c(0, 0)) +
    scale_y_continuous(
      breaks = seq(ymin, ymax, 2000),
      expand = c(0, 0),
      labels = function(x)
        format(x, big.mark = ".", scientific = FALSE)
    )
  print(plot)
  ggsave(
    "intensiteit.png",
    width = 24,
    height = 13.5,
    units = "cm",
    dpi = 300
  )
  
  #### MAISSOPBRENGSTEN ####
  plot = ggplot(data = data_opb_mais_ds, aes(
    x = jaartal,
    y = mean_y,
    label = round(mean_y, 0)
  )) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Maisopbrengst [kg ds / ha]") +
    scale_x_continuous(breaks = 2013:2020, minor_breaks =  NULL) +
    geom_bar(
      stat = "identity",
      position = position_dodge(),
      fill = colorPalette[1],
      width = 0.75
    ) +
    geom_errorbar(
      aes(ymin = mean_y - sd_y, ymax = mean_y + sd_y),
      position = position_dodge(0.9),
      width = 0.4,
      size = 1
    ) +
    coord_cartesian(ylim = c(10000, 23000)) +
    scale_y_continuous(
      breaks = seq(10000, 23000, 1000),
      labels = function(x)
        format(x, big.mark = ".", scientific = FALSE)
    ) +
    geom_text(size = 5,
              position = position_stack(vjust = 0.9),
              colour = "white")
  
  print(plot)
  ggsave(
    "maisopbrengst_ds.png",
    width = 24,
    height = 13.5,
    units = "cm"
  )
  
  data_re = data_KLW_VKX_Compleet %>%
    select(jaartal,
           gr_geh_re,
           gk_geh_re,
           sm_geh_re,
           ov_geh_re,
           kv_geh_re,
           rants_geh_re) %>%
    group_by(jaartal) %>%
    dplyr::summarise_all(mean)
  
  colnames(data_re)[which(colnames(data_re) == "gr_geh_re")] = "Weidegras"
  colnames(data_re)[which(colnames(data_re) == "gk_geh_re")] = "Kuilgras"
  colnames(data_re)[which(colnames(data_re) == "sm_geh_re")] = "Snijmais"
  colnames(data_re)[which(colnames(data_re) == "ov_geh_re")] = "Overig"
  colnames(data_re)[which(colnames(data_re) == "kv_geh_re")] = "Krachtvoer"
  colnames(data_re)[which(colnames(data_re) == "rants_geh_re")] = "Rantsoen"
  
  data_re = pivot_longer(data_re,
                         cols = c(Weidegras, Kuilgras, Snijmais, Overig, Krachtvoer, Rantsoen))
  
  plot = ggplot(data = data_re, aes(
    x = jaartal,
    y = value,
    fill = name,
    label = round(value, 0)
  )) +
    theme_bw() +
    xlab("Jaartal") +
    ylab("Ruw eiwitgehalte [g / kg ds]") +
    scale_x_continuous(breaks = 2013:2020, minor_breaks =  NULL) +
    geom_bar(stat = "identity",
             position = position_dodge(width = 0.8),
             width = 0.5) +
    #geom_errorbar(aes(ymin=mean_y - sd_y, ymax = mean_y + sd_y), position = position_dodge(0.9), width = 0.4, size = 1) +
    coord_cartesian(ylim = c(50, 275)) +
    scale_y_continuous(breaks = seq(50, 275, 10)) +
    theme(legend.title = element_blank())
  
  print(plot)
  ggsave(
    "rantsoen_re.png",
    width = 24,
    height = 13.5,
    units = "cm"
  )
  
  #Violin  plots
  
  {                    
    dataset_VKX$jaartal_factor = as.factor(dataset_VKX$jaartal)
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = melkprod, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Melkproductie bedrijf [kg]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(0, 3000000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary(
        fun = mean,
        geom = "point",
        size = 4,
        color = "black"
      ) +
      stat_summary( aes(y = melkprod, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black"
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 4, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "melkproductie.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = melkperha, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Intensiteit [kg melk / ha]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(10000, 30000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary(
        fun = mean,
        geom = "point",
        size = 4,
        color = "black"
      ) +
      stat_summary( aes(y = melkperha, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black"
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 4, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "intensiteit.png", width = 20, height = 12, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = urenweidenmelkkoeienNA, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Beweiding melkkoeien [uren]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(0, 2000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary(
        fun = mean,
        geom = "point",
        size = 4,
        color = "black",
        na.rm = T
      ) +
      stat_summary( aes(y = urenweidenmelkkoeienNA, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 4, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "beweiding.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = beweidenmelkkoeienboolean*100, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Aandeel bedrijven weidegang [%]") +
      #geom_violin(aes(color = jaartal_factor)) +
      #geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(50, 100)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( aes(color = jaartal_factor),
                    fun = mean,
                    geom = "point",
                    size = 4,
                    #color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = beweidenmelkkoeienboolean*100, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "beweidingaandeel.png", width = 24, height = 8, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = rants_geh_re, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("RE-gehalte rantsoen [g/kg ds]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(140, 180)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = rants_geh_re, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "re_rantsoen.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = melkpkoe, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Melkprodutie [kg / koe / jaar]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(6000, 13000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = melkpkoe, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "melkperkoe.png", width = 20, height = 12, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = kvper100kgmelk, fill = jaartal_factor)) +
      theme_bw() +
      xlab("Jaartal") +
      ylab("Krachtvoerverbruik [kg / 100 kg melk]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(15, 35)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = kvper100kgmelk, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "kv_100kg_melk.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = sm_aandeel, fill = jaartal_factor)) +
      theme_bw() +
      xlab("Jaartal") +
      ylab("Aandeel snijmais rantsoen [%]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(0, 50)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = sm_aandeel, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "aandeel_snijmais.png", width = 20, height = 12, units = "cm")
    
    dataset_VKX$kv_ov_aandeel = dataset_VKX$kv_aandeel + dataset_VKX$ov_aandeel
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = kv_ov_aandeel, fill = jaartal_factor)) +
      theme_bw() +
      xlab("Jaartal") +
      ylab("Aandeel krachtvoer en overige bijproducten rantsoen [%]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(10, 60)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = kv_ov_aandeel, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "aandeel_kv_overig.png", width = 20, height = 12, units = "cm")
    
    dataset_VKX$gk_gr_aandeel = dataset_VKX$gr_aandeel + dataset_VKX$gk_aandeel
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = gk_gr_aandeel, fill = jaartal_factor)) +
      theme_bw() +
      xlab("Jaartal") +
      ylab("Aandeel weidegras en graskuil rantsoen [%]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(20, 60)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = gk_gr_aandeel, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 6, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "aandeel_vg_gk.png", width = 20, height = 12, units = "cm")
    
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = opb_graspr_ds, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Opbrengst productiegrasland [kg ds / ha]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(4000, 16000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = opb_graspr_ds, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "opb_gras_pr.png", width = 20, height = 12, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = opb_mais_ds, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Opbrengst mais [kg ds / ha]") +
      geom_violin(aes(color = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(6000, 26000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      ) +
      stat_summary( aes(y = opb_mais_ds, group =1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    na.rm = T
      ) + stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1)
    
    print(plot)
    ggsave( "opb_mais.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX %>% filter(grondsoort %in% c("veen", "klei", "zand")), aes(x = jaartal_factor, y = verl_bodbal1_ha, fill = grondsoort, color = grondsoort)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Stikstofbodemoverschot [kg N/ha]") +
      geom_violin(aes(fill = grondsoort)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      #theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n=6)) +
      coord_cartesian(ylim = c(-50, 300)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, aes(group = grondsoort), position = position_dodge(.9) ,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T, show.legend = F
                    
      )   +
      stat_summary( aes(y = verl_bodbal1_ha, group = grondsoort, linetype  = grondsoort),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) ,
                    show.legend = F
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1, position = position_dodge(.9) )+
      scale_fill_manual(name = "Grondsoort", values = alpha(c(kleur_vka_groen, kleur_vka_rood), 0.6)) +
      scale_color_manual(name = "Grondsoort",values = alpha(c(kleur_vka_groen, kleur_vka_rood), 0.6)) +
      theme(legend.position = "top") 
      
      
    print(plot)
    ggsave( "n_overschot.png", width = 24, height = 16, units = "cm")
    
    plot = ggplot(data = dataset_VKX %>% filter(grondsoort %in% c("veen", "klei", "zand")) , aes(x = jaartal_factor, y = verl_bodbal2_ha, fill = grondsoort, color = grondsoort)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Fosfaatbodemoverschot [kg P2O5/ha]") +
      geom_violin(aes(fill = grondsoort)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      #theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n=6)) +
      coord_cartesian(ylim = c(-65, 65)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, aes(group = grondsoort), position = position_dodge(.9) ,
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T,
                    show.legend = F
      )   +
      stat_summary( aes(y = verl_bodbal2_ha, group = grondsoort, linetype  = grondsoort),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) ,
                    show.legend = F
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1, position = position_dodge(.9) )+
    scale_fill_manual(name = "Grondsoort", values = alpha(c(kleur_vka_groen, kleur_vka_rood), 0.6)) +
      scale_color_manual(name = "Grondsoort",values = alpha(c(kleur_vka_groen, kleur_vka_rood), 0.6))  +
      theme(legend.position = "top") 
    print(plot)
    ggsave( "p_overschot.png", width = 24, height = 16, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = dzh_nh3_bedrha, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Ammoniakemissie [kg NH3 / ha]") +
      geom_violin(aes(fill = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      #coord_cartesian(ylim = c(30, 90)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, 
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      )   +
      stat_summary( aes(y = dzh_nh3_bedrha, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
    
    print(plot)
    ggsave( "ammoniak.png", width = 20, height = 12, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = dzh_nh3_bedrha * opp_totaal, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Ammoniakemissie [kg NH3]") +
      geom_violin(aes(fill = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      #coord_cartesian(ylim = c(1000, 6000)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, 
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      )   +
      stat_summary( aes(y = dzh_nh3_bedrha * opp_totaal, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
    
    print(plot)
    ggsave( "ammoniak_totaal.png", width = 20, height = 12, units = "cm")
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = dzh_co2_melkprod, fill = jaartal_factor, color = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("BKG emissie [g CO2-eq / FPCM]") +
      geom_violin(aes(fill = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(900, 1400)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, 
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      )   +
      stat_summary( aes(y = dzh_co2_melkprod, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
    
    print(plot)
    
    ggsave( "bkg.png", width = 20, height = 12, units = "cm")
    
    
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = pceigen_n, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Eiwit eigen land [%]") +
      geom_violin(aes(fill = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(25, 100)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, 
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      )   +
      stat_summary( aes(y = pceigen_n, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
    
    print(plot)
    ggsave( "eigen_n.png", width = 20, height = 12, units = "cm")
    
    plot = ggplot(data = dataset_VKX, aes(x = jaartal_factor, y = pceigen_n_buurt, fill = jaartal_factor)) +
      
      theme_bw() +
      xlab("Jaartal") +
      ylab("Eiwit eigen land + buurtaankoop [%]") +
      geom_violin(aes(fill = jaartal_factor)) +
      geom_sina(color = "lightgrey", size = 0.5) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
      coord_cartesian(ylim = c(25, 100)) +
      #geom_boxplot(width = 0.2, size = 1, outlier.shape = NA) +
      stat_summary( fun = mean, 
                    geom = "point",
                    size = 4,
                    color = "black",
                    na.rm = T
      )   +
      stat_summary( fun = mean,  aes(y=pceigen_n),
                    geom = "point",
                    size = 4,
                    color = "red",
                    na.rm = T
      )   +
      stat_summary( aes(y = pceigen_n_buurt, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "black",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) + 
      stat_summary( aes(y = pceigen_n, group = 1),
                    fun = mean,
                    geom = "line",
                    size = 1,
                    color = "red",
                    
                    na.rm = T , position = position_dodge(.9) 
      ) +
      stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )+
      stat_summary (fun = mean, geom = "text", aes(y = pceigen_n,label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "red", vjust = 2 )
    
    print(plot)
    ggsave( "eigen_n_buurt.png", width = 20, height = 12, units = "cm")
    
    
    
    data_re = dataset_VKX %>%
      select(jaartal_factor,
             gr_geh_re,
             gk_geh_re,
             sm_geh_re,
             ov_geh_re,
             kv_geh_re,
             rants_geh_re) %>% dplyr::group_by(jaartal_factor) %>% dplyr::summarise_all(mean, na.rm=T)
    
    colnames(data_re)[which(colnames(data_re) == "gr_geh_re")] = "Weidegras"
    colnames(data_re)[which(colnames(data_re) == "gk_geh_re")] = "Kuilgras"
    colnames(data_re)[which(colnames(data_re) == "sm_geh_re")] = "Snijmais"
    colnames(data_re)[which(colnames(data_re) == "ov_geh_re")] = "Overig"
    colnames(data_re)[which(colnames(data_re) == "kv_geh_re")] = "Krachtvoer"
    colnames(data_re)[which(colnames(data_re) == "rants_geh_re")] = "Rantsoen"
    
    library(tidyr)
    data_re_long = pivot_longer(data_re,
                                cols = c(Weidegras, Kuilgras, Snijmais, Overig, Krachtvoer, Rantsoen))
    
    plot = ggplot(data = data_re_long, aes(x= jaartal_factor, y = value, color = name, group = name)) +
      geom_point(size = 5) +
      geom_line(size = 1) +
      theme_bw() +
      xlab("Jaartal") +
      ylab("RE-gehalte [g/kg ds]") +
      theme(legend.title = element_blank(), legend.position = "top") 
    print(plot)
    ggsave( "re_gehaltes_producten.png", width = 20, height = 12, units = "cm")
    
  }
  
  
  
  #------------------------------BODEMOVERSCHOT MINUS NORM STIKSTOF ----------------------
  
  data = dataAggregated %>% select(jaartal, bodemoverschot_N_minus_norm, bodem_type)
  
  ####
  plot = ggplot(data = data, aes(x = as.factor(jaartal), y = bodemoverschot_N_minus_norm, fill = bodem_type)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(-250,250), expand = TRUE) +
    xlab("Jaar") +
    ylab(columnNames["bodemoverschot_N_minus_norm",2]) +
    theme_light() +
    theme(text = element_text(size=8)) +
    theme(legend.position="top") +
    scale_fill_discrete(name = columnNames["bodem_type",2])
  
  
  
  print(plot)
  ggsave("Bodemoverschot_stikstof_minus_norm_landgebruik.png", width = 16, height = 8, units = "cm")
  
  
  #----------------------------AANDEEL BEDRIJVEN NORM STIKSTOF-----------------------------j
  
  data = dataAggregated %>% 
    select(jaartal, bodemoverschot_N_minus_norm, bodem_type) %>%
    group_by(jaartal, bodem_type) %>%
    summarise(count = 100*(length(bodemoverschot_N_minus_norm[bodemoverschot_N_minus_norm < 0]) / length(bodemoverschot_N_minus_norm)))
  
  data2 = dataAggregated %>% 
    select(jaartal, bodemoverschot_N_minus_norm, bodem_type) %>%
    group_by(jaartal) %>%
    summarise(count = 100*(length(bodemoverschot_N_minus_norm[bodemoverschot_N_minus_norm < 0]) / length(bodemoverschot_N_minus_norm)))
  data2$bodem_type = "alle"
  
  dataTotaal = rbind(data,data2)
  
  plot = ggplot(data = dataTotaal, aes(fill = bodem_type, x=jaartal, y = count)) +
    geom_bar(stat="identity", width = 0.5, position = "dodge") +
    xlab("Jaar") +
    scale_x_continuous(breaks = 2013:2019, minor_breaks =  NULL) +
    ylab("Aandeel bedrijven [%]") +
    theme_light() +
    theme(legend.position="top") +
    scale_fill_discrete(name = columnNames["bodem_type",2])
  print(plot)
  ggsave("Aandeel_bedrijven_norm_stikstof.png", width = 16, height = 8, units = "cm")
  
  
  data = datasetRaw %>%
    filter(bodem_type == "zand") %>%
    group_by(jaartal, bodemoverschot_N_minus_norm_boolean) %>%
    summarise_all(mean, na.rm = T)
  
  data = data %>% select(jaartal, bodemoverschot_N_minus_norm_boolean, aandeel_zand, kring1_bodover, max_N_bodemoverschot, bodemoverschot_N_minus_norm, graspr_tmst_kgn, mais_tmst_kgn, opb_graspr_ds, opb_graspr_n, opb_mais_n, opb_mais_ds )
  
  #---------------------------BODEMOVERSCHOT FOSFAAT-----------------
  
  data = dataAggregated %>% select(jaartal, kring2_bodover, bodem_type) %>% 
    group_by(jaartal, bodem_type) %>%
    summarise_all(mean, na.rm = TRUE)
  
  ####
  plot = ggplot(data = data, aes(x = as.factor(jaartal), y = kring2_bodover, fill = bodem_type)) +
    #geom_hline(yintercept=0, color = "red", size=2) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_cartesian(ylim = c(-25,25), expand = TRUE) +
    xlab("Jaar") +
    ylab(columnNames["kring2_bodover",2]) +
    theme_light() +
    theme(text = element_text(size=8)) +
    theme(legend.position="top") +
    scale_fill_discrete(name = columnNames["bodem_type",2]) 
  
  
  
  print(plot)
  ggsave("Bodemoverschot_fosfaat_landgebruik.png", width = 16, height = 8, units = "cm")
  
  data = dataAggregated %>% select(jaartal, gk_geh_p, kv_geh_p, rants_geh_p) %>%
    group_by(jaartal) %>%
    summarise_all(mean, na.rm = TRUE)
  
  
  data = pivot_longer(data, cols = c(gk_geh_p, kv_geh_p, rants_geh_p))
  
  data$name = columnNames[data$name,2]
  
  plot = ggplot(data = data, aes(x = jaartal, y = value, fill = name, label = round(value,1))) +
    geom_bar(position = "dodge", stat = "identity", width = 0.75) +
    ylab("P-gehalte [g/kg ds]") +
    coord_cartesian(ylim = c(3,5.5), xlim = c(2012+0.5,2020-0.5), expand = FALSE) +
    scale_x_continuous(breaks = 2013:2019, minor_breaks =  NULL) +
    labs(fill = "H")+
    xlab("Jaar") +
    theme_light() +
    theme(legend.position="top") +
    theme(legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 1))
  #geom_text(size = 3, position = position_dodge(width = 0.75),vjust = -0.5)
  
  
  print(plot)
  ggsave("Rantsoen_P_gehaltes.png", width = 16, height = 8, units = "cm")
  
  #rants_geh_P
  #gk_geh_P
  #kv_geh_P
  
  barplot_dodge <- function(data, outputName, ylabel, xlabel, ymin = NULL, ymax = NULL){
    
    data = as.data.frame(data)
    
    xdata = data[,1]
    ydata = data[,3]
    ndata = data[,2]
    
    xmin = min(na.omit(xdata))
    xmax = max(na.omit(xdata))
    
    if(is.null(ymin)){
      ymin = 0.95*min(na.omit(ydata)) 
    }
    
    if(is.null(ymax)){
      ymax = 1.05*max(na.omit(ydata))
    }
    
    plot = ggplot(data = data, aes(x = xdata, y = ydata, fill = ndata)) +
      #Set barplot
      geom_bar(position = "dodge", stat = "identity", width = 0.75) +
      #Options for axis
      coord_cartesian(ylim = c(ymin,ymax), xlim = c( (xmin-0.5) , (xmax+0.5) ), expand = FALSE) +
      scale_x_continuous(breaks = xmin:xmax, minor_breaks =  NULL) +
      #Labels
      ylab(ylabel) +
      xlab(xlabel) +
      labs(fill = "H")+
      #Theme
      theme_light() +
      theme(legend.position="top") +
      theme(legend.title = element_blank()) +
      guides(fill = guide_legend(nrow = 1))
    print(plot)
    
    ggsave(paste(outputName,".png", sep = ""), width = 16, height = 8, units = "cm")
    
  }
  
  #Aandeel bedrijven overschot
  data = dataAggregated %>% 
    select(jaartal, bodemoverschot_P_minus_norm, bodem_type) %>%
    group_by(jaartal, bodem_type) %>%
    summarise(count = 100*(length(bodemoverschot_P_minus_norm[bodemoverschot_P_minus_norm < 0]) / length(bodemoverschot_P_minus_norm)))
  
  data2 = dataAggregated %>% 
    select(jaartal, bodemoverschot_P_minus_norm, bodem_type) %>%
    group_by(jaartal) %>%
    summarise(count = 100*(length(bodemoverschot_P_minus_norm[bodemoverschot_P_minus_norm < 0]) / length(bodemoverschot_P_minus_norm)))
  data2$bodem_type = "alle"
  
  dataTotaal = rbind(data,data2)
  
  
  #------------------------------------------BEP------------------------------------------
  
  data = dataAggregated %>% 
    select(jaartal, BEP_voordeel_boolean, bodem_type) %>%
    group_by(jaartal, bodem_type) %>%
    summarise_all(mean) 
  
  
  #PLOT VOOR ZAND
  data = avg_per_landgebruik %>% select(columnNames[c("jaartal", "bodem_type", "fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan"),2]) %>% 
    filter(bodem_type == "zand") %>%
    filter(jaartal > 2014)
  
  data = pivot_longer(data, cols = columnNames[c("fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan"),1] )
  data$name = columnNames[data$name,2]
  
  plot = ggplot() +
    geom_bar(data = data, aes(x= jaartal, y = value, fill = name), stat = "identity", position = "dodge")+
    scale_x_continuous(breaks = 2015:2019, minor_breaks =  NULL) +
    coord_cartesian(ylim = c(60,100), xlim = c(2014+0.5,2020-0.5), expand = FALSE) +
    xlab(columnNames["jaartal",2]) +
    ylab("Fosfaat [kg / ha]") +
    theme_light() +
    theme(legend.position="top") +
    scale_fill_discrete(name = "")
  
  print(plot)
  ggsave("Fosfaatnorm_fosfaatbemesting_zand.png", width = 16, height = 8, units = "cm")
  
  #PLOT VOOR KLEI
  data = avg_per_landgebruik %>% select(columnNames[c("jaartal", "bodem_type", "fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan"),2]) %>% 
    filter(bodem_type == "klei") %>%
    filter(jaartal > 2014)
  
  data = pivot_longer(data, cols = columnNames[c("fosfaatnorm_haspecifiek3", "gebrnorm2_ha", "kring2_bodaan"),1] )
  data$name = columnNames[data$name,2]
  
  plot = ggplot() +
    geom_bar(data = data, aes(x= jaartal, y = value, fill = name), stat = "identity", position = "dodge")+
    scale_x_continuous(breaks = 2015:2019, minor_breaks =  NULL) +
    coord_cartesian(ylim = c(60,100), xlim = c(2014+0.5,2020-0.5), expand = FALSE) +
    xlab(columnNames["jaartal",2]) +
    ylab("Fosfaat [kg / ha]") +
    theme_light() +
    theme(legend.position="top") +
    scale_fill_discrete(name = "")
  
  print(plot)
  ggsave("Fosfaatnorm_fosfaatbemesting_klei.png", width = 16, height = 8, units = "cm")
  ####
  
  #-----------------------------ON THE WAY TO PLANET PROOF----------------------------
  
  data = dataset_VKX %>% filter(jaartal == 2020) %>% dplyr::summarise(BeweidingBasis = length(PP_beweiding[PP_beweiding == 1]) / length(PP_beweiding),
                                                              BeweidingNiet = length(PP_beweiding[PP_beweiding == 0]) / length(PP_beweiding),
                                                              EiwitBasis = length(PP_eiwit_van_eigen_land[PP_eiwit_van_eigen_land == 1]) / length(PP_eiwit_van_eigen_land),
                                                              EiwitTop = length(PP_eiwit_van_eigen_land[PP_eiwit_van_eigen_land == 2]) / length(PP_eiwit_van_eigen_land),
                                                              EiwitNiet = length(PP_eiwit_van_eigen_land[PP_eiwit_van_eigen_land == 0]) / length(PP_eiwit_van_eigen_land),
                                                              StikstofBasis = length(PP_stikstof_bodemoverschot[PP_stikstof_bodemoverschot == 1]) / length(PP_stikstof_bodemoverschot),
                                                              StikstofTop = length(PP_stikstof_bodemoverschot[PP_stikstof_bodemoverschot == 2]) / length(PP_stikstof_bodemoverschot),
                                                              StikstofNiet = length(PP_stikstof_bodemoverschot[PP_stikstof_bodemoverschot == 0]) / length(PP_stikstof_bodemoverschot),
                                                              AmmoniakBasis = length(PP_ammoniak[PP_ammoniak == 1]) / length(PP_ammoniak),
                                                              AmmoniakTop = length(PP_ammoniak[PP_ammoniak == 2]) / length(PP_ammoniak),
                                                              AmmoniakNiet = length(PP_ammoniak[PP_ammoniak == 0]) / length(PP_ammoniak),
                                                              BlijvendGraslandBasis = length(PP_blijvend_grasland[PP_blijvend_grasland == 1]) / length(PP_blijvend_grasland),
                                                              BlijvendGraslandTop = length(PP_blijvend_grasland[PP_blijvend_grasland == 2]) / length(PP_blijvend_grasland),
                                                              BlijvendGraslandNiet = length(PP_blijvend_grasland[PP_blijvend_grasland == 0]) / length(PP_blijvend_grasland),
                                                              BroeikasBasis = length(PP_broeikasgas[PP_broeikasgas == 1]) / length(PP_broeikasgas),
                                                              BroeikasTop = length(PP_broeikasgas[PP_broeikasgas == 2]) / length(PP_broeikasgas),
                                                              BroeikasNiet = length(PP_broeikasgas[PP_broeikasgas == 0]) / length(PP_broeikasgas))
  
  
  
  
  dataLong = pivot_longer(data, names(data))
  dataLong$PP_categorie = ifelse(grepl("Beweiding",dataLong$name),"Beweiding",0)
  dataLong$PP_categorie = ifelse(grepl("Eiwit",dataLong$name),"Eiwit",dataLong$PP_categorie)
  dataLong$PP_categorie = ifelse(grepl("Stikstof",dataLong$name),"Stikstof",dataLong$PP_categorie)
  dataLong$PP_categorie = ifelse(grepl("Ammoniak",dataLong$name),"Ammoniak",dataLong$PP_categorie)
  dataLong$PP_categorie = ifelse(grepl("Blijvend",dataLong$name),"Blijvend Grasland",dataLong$PP_categorie)
  dataLong$PP_categorie = ifelse(grepl("Broeikas",dataLong$name),"Broeikasgas",dataLong$PP_categorie)
  
  dataLong$PP_klasse = ifelse(grepl("Basis", dataLong$name), "Voldoet Basisnorm", 0)
  dataLong$PP_klasse = ifelse(grepl("Top", dataLong$name), "Voldoet Topniveau", dataLong$PP_klasse)
  dataLong$PP_klasse = ifelse(grepl("Niet", dataLong$name), "Voldoet Niet", dataLong$PP_klasse)
  
  
  plot = ggplot(data = dataLong, aes(x = PP_categorie, y = 100*value, label = round(100*value,1), fill = factor(PP_klasse, levels = c("Voldoet Niet","Voldoet Topniveau","Voldoet Basisnorm")))) +
    geom_bar(position = "stack", stat = "identity", width = 0.5, alpha = 0.8) +
    ylab("Aandeel bedrijven [%]") +
    labs(fill = "H")+
    #scale_x_continuous(breaks = min(data$jaartal):max(data$jaartal), minor_breaks =  NULL) +
    xlab("Categorie PlanetProof") +
    theme_light() +
    theme(legend.position="top") +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size=8)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c(kleur_vka_groen, kleur_vka_oranje, kleur_vka_rood))
    
  
  
  print(plot)
  ggsave("Categorie_PP.png", width = 16, height = 8, units = "cm")
  
  
  
  
  #-----------------------------Excreties N & TAN------------
  
  data = datasetRaw #%>% filter(Grondgebruik == "all")
  
  data = data %>% 
    select(jaartal, excr_specinc1, tan_prod_bedr) %>%
    group_by(jaartal) %>%
    summarise_all(mean)
  data = pivot_longer(data, cols = c(excr_specinc1, tan_prod_bedr))
  
  data$name = columnNames[data$name,2]
  
  
  plot = ggplot(data = data, aes(x = jaartal, y = value, fill = name, label = round(value,0))) +
    geom_bar(stat="identity", width = 0.5, position = "dodge") +
    #scale_y_continuous(limits=c(min(as.numeric(dataPlot$Gemiddelde)),max(as.numeric(dataPlot$Gemiddelde))),oob = rescale_none) +
    scale_x_continuous(breaks = 2013:2019, minor_breaks =  NULL) +
    scale_y_continuous(limits =c(0,17500), expand =  c(0,0)) +
    labs(fill = "H")+
    xlab("Jaar") +
    ylab("Excretie per bedrijf [kg]") +
    theme_light() +
    theme(legend.position="top") +
    theme(legend.title = element_blank()) +
    geom_text(size = 3, position = position_dodge(width = 0.5),vjust = -0.5)
  
  print(plot)
  ggsave("Excretie_per_bedrijf.png", width = 16, height = 8, units = "cm")
  
  
}
