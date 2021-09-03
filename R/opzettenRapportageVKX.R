#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het maken van KLW databases op basis van KLW .dmpx files ÉN ledenlijsten VKA / VKO 

#INPUTS
#path_to_dataset --> string die path naar VKX dataset weergeeft.
#output_path --> string die path naar output locatie geeft

#getDataInFolder
#toevoegenKengetallenKLW

#Voor testen:
#path_to_dataset = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Database
#output_path = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Figuren

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
                        "https://raw.githubusercontent.com/jureekelder/VKA/main/R/toevoegenKengetallenKLW.R")
  
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  
  #Functie voor wegschrijven van tabellen
  standard_template_path = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Figuren/template.xlsx"
  write_to_excel <- function(df){
    
    kleur_vka_rood = rgb(167, 25, 48, maxColorValue = 255)
    kleur_vka_groen = rgb(0, 102, 67, maxColorValue = 255)
    
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
  
  #Laden libraries
  library(dplyr)  
  library(openxlsx)
  library(ggplot2)
  library(dplyr)
  options(scipen = 999) #Do not use 5e09 etc for large numbers.
  library(reshape2)
  library(reshape)
  library(ggforce)
  
  
  #Inlezen van dataset
  if(file.exists(path_to_dataset)){
    dataset_VKX = getDataInFolder(path_to_dataset)
    dataset_VKX = toevoegenKengetallenKLW(dataset_VKX)
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
  
  
  
  #DEEL 1 : BEDRIJFSONTWIKKELING
  
  dataset_VKX = dataset_VKX %>% mutate(bodem_type_zand = ifelse(grondsoort == "zand", 1, 0))
  dataset_VKX = dataset_VKX %>% mutate(bodem_type_klei = ifelse(grondsoort == "klei", 1, 0))
  
  
  parameters_bedrijf = matrix(ncol = 3, byrow = T, data = (c(
                         "nkoe", 0, "Aantal melkkoeien [-]",
                         "njongvee", 0, "Aantal jongvee [-]",
                         "jvper10mk", 1, "Jongvee / 10 melkkoeien [-]",
                         "melkprod", 0 , "Melkproductie bedrijf [-]",
                         "melkpkoe", 0, "Melk / koe / jaar [kg]",
                         "melkperha", 0, "Intensiteit [kg/ha]",
                         "fpcmperha", 0, "Intensiteit [kg FPCM/ha]",
                         "kvperbedrijf", 0, "Krachtvoerverbruik [kg]",
                         "bodem_type_zand", 2, "Aandeel zand [%]",
                         "bodem_type_klei", 2, "Aandeel klei [%]", #0 voor klei, 1 voor zand. Gemiddelde per jaar is dus aantal bedrijven (%).
                         
                         "kvper100kgmelk", 1, "Krachtvoerverbruik [kg/100 kg melk]"))) #Varkens, pluimvee)
  
  parameters_productie = matrix(ncol = 3, byrow = T, data = c(
    
    "melkpkoe", 0, "Melkproductie [kg/koe/jaar]",
    "fpcmkoejaar", 0, "FPCM [kg/koe/jaar]",
    "vet", 2, "Vetgehalte [%]",
    "eiwit", 2, "Eiwitgehalte [%]",
    "ureum", 1, "Ureumgehalte [mg/100 gr]",
    "fosfor", 0, "Fosforgehalte [mg/100 gr]"
    
  ))
  
  
  parameters_areaal = c("opp_totaal",
                       "oppgras",
                       "oppblijgras",
                       "opp_prgras",
                       "oppklaver",
                       "pcklaver",
                       
                       "opp332en335",
                       "opp_natuur",
                       "opp_mais",
                       "oppcontinu_b",
                       
                       "oppmais",
                       "oppoverig")
  
  parameters_vee = matrix(ncol = 3, byrow = T , data = c(
                     "efficientie_N", 2, "Stikstof efficientie veestapel [%]",
                     "efficientie_P", 2, "Fosfaat efficientie veestapel [%]",
                     "voereff_melk", 1, "Voerefficiëntie [kg melk / kg ds]",
                     "voereff_fpcm", 1, "Voerefficiëntie [kg fpcm / kg ds]",
                     "urenweidenmelkkoeienNA", 0, "Beweiding weidebedrijven [uren]",
                     "beweidenmelkkoeienboolean", 2, "Aandeel bedrijven beweiding [%]",
                     "zstvdagenNA", 0, "Zomerstalvoedering [dagen]",
                     "zstvdagenboolean", 2, "Aandeel bedrijven zomerstalvoedering [%]"
  )

                     
  )
  
  parameters_benutting = c("benut_n_bed",
                           'benut_n_vee',
                           "benut_n_bod",
                           "benut_p_bed",
                           "benut_p_vee",
                           "benut_p_bod"
                          
  )
  
  parameters_rantsoen = c("rants_verbruik",
                          "gr_verbruik",
                          "gk_verbruik",
                          "sm_verbruik",
                          "ov_verbruik",
                          "kv_verbruik",
                          "mp_verbruik",
                          "rants_aandeel",
                          "gr_aandeel",
                          "gk_aandeel",
                          "sm_aandeel",
                          "ov_aandeel",
                          "kv_aandeel",
                          "mp_aandeel",
                          
                          "rantsoen_re",
                          "rantsoen_p",
                          "rantsoen_vem",
                          "rantsoen_rekvem",
                          "rantsoen_pkvem",
                          "rantsoen_pcvg",
                          "rantsoen_pcgk",
                          "rantsoen_pcsm",
                          "rantsoen_pcovbp",
                          "rantsoen_pckvmp",
                          "rantsoen_pcre_vg",
                          "rantsoen_pcre_gk",
                          "rantsoen_pcre_sm",
                          "rantsoen_pcre_ovbp",
                          "rantsoen_pcre_kvmp"
                          
  )
                       

  
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
  
  parameters_ammoniak = c(
    
    "em_nh3_stal",
    "em_nh3_stalint",
    "em_nh3_ombouw",
    "em_nh3_omgras",
    "em_nh3_kmbouw",
    "em_nh3_kmgras",
    "em_nh3_beweid",
    "em_nh3_vrlweid",
    "em_nh3_vrloogst",
    "em_nh3_bedrijf",
    "em_nh3_hagrond",
    "em_nh3_tonmelk",
    "n_excretie_mlk",
    "n_excretie_ovg",
    "pctan_excr_mlk",
    "tan_prod_bedr", #OOK INCLUSIEF ANDERE DIEREN
    "verl_nh3stal_ha",
    "verl_nh3weid_ha",
    "verl_nh3bem_ha",
    "verl_nh3veld_ha",
    "emnh3_tot_bdr",
    "emnh3_tot_mlk"
    
    
    
    
  )
  
  parameters_aankoop_voer = c(
    "aankoop_aanleg_gk_hoev",
    "aankoop_aanleg_sm_hoev",
    "aankoop_aanleg_ov_hoev",
    "aankoop_aanleg_kv_hoev",
    "akvoer_n",
    "akvoer_P"

    
    
    
  )
  
  parameters_kunstmest = c("kmaan_kg",
                           "kmaan_namm",
                           "kmaan_nnit",
                           "kmaan_nure"
  )
  
  parameters_eiwit_eigen_teelt = c("pceigen_n",
                                   "pceigen_p",
                                   "pceigen_vem",
                                   "eiwiteig_tlt_vg",
                                   "eiwiteig_tlt_gk",
                                   "eiwiteig_tlt_sm",
                                   "eiwiteig_tlt_ov"
  )
  
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
  
  parameters_bemesting = c("graspr_tmst_kgn",
                           "graspr_tmst_kgp2o5",
                           "graspr_dmst_m3",
                           "graspr_dmst_kgn",
                           "graspr_dmst_kgp2o5",
                           "graspr_kmst_kgn",
                           "graspr_kmst_kgp2o5",
                           "graspr_wmst_kgn",
                           "graspr_wmst_kgp2o5",
                           'mais_tmst_kgn',
                           "mais_tmst_kgp2o5",
                           "mais_dmst_m3",
                           "mais_dmst_kgn",
                           "mais_dmst_kgp2o5",
                           "mais_kmst_kgn",
                           "mais_kmst_kgp2o5",
                           "graspr_bemest_dierlijk_N",
                           "graspr_totaal_kgn",
                           "graspr_totaal_kgp2o5",
                           "mais_bemest_dierlijk_N",
                           "mais_totaal_kgn",
                           "mais_totaal_kgp2o5"
                           
                           
  )
  
  
  
  parameters_opbrengst = c("opb_graspr_ds",
                           "opb_graspr_kvem",
                           'opb_graspr_n',
                           "opb_graspr_p2o5",
                           "opb_gras_ds",
                           "opb_gras_kvem",
                           "opb_gras_n",
                           "opb_gras_p2o5",
                           "opb_mais_ds",
                           "opb_mais_kvem",
                           "opb_mais_n",
                           "opb_mais_p2o5",
                           "opb_graspr_ds_per_N_bemest",
                           "opb_mais_ds_per_N_bemest",
                           "opb_graspr_vem_g_kg",
                           "opb_graspr_re_g_kg",
                           "opb_graspr_p_g_kg",
                           "opb_mais_vem_g_kg",
                           "opb_mais_re_g_kg",
                           "opb_mais_p_g_kg"
  )
  
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
  
  
  
  parameters_broeikasgassen = c("dzh_co2_melkvee",
                               "dzh_co2_pensferm", 
                               "dzh_co2_mestopsl", 
                               "dzh_co2_voerprod", 
                               "dzh_co2_energie", 
                               "dzh_co2_aanvoer")
  
  

  
                      
  
  data_KLW_VKX_Compleet = dataset_VKX
  
  
  #Tabel onderdeel algemeen
  
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
  
  
  

  dataset_VKX_gemiddeld_bedrijf = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_bedrijf)
  dataset_VKX_gemiddeld_bedrijf = dataset_VKX_gemiddeld_bedrijf %>% dplyr::mutate(`Percentage bedrijven zand [%]` = 100 * `Percentage bedrijven zand [%]`)
  write_to_excel(dataset_VKX_gemiddeld_bedrijf)
  
  dataset_VKX_gemiddeld_productie = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_productie)
  write_to_excel(dataset_VKX_gemiddeld_productie)
  
  dataset_VKX_gemiddeld_vee = bereken_gemiddelde_over_jaren(dataset_VKX, parameters_vee)
  write_to_excel(dataset_VKX_gemiddeld_vee)
  
  
  
    
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  colorPalette = RColorBrewer::brewer.pal(9, "Set1") #gg_color_hue(4)
  colorsGGplot = gg_color_hue(3)
  options(scipen = 999)
  
  #Grafieken
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
  
  
}