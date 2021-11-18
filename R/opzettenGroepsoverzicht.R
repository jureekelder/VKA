#Jur Eekelder; 12-11-2021
#jur.eekelder@demarke.eu
#Script voor het opzetten van groepsoverzichten, gebaseerd op kengetallen uit de KringloopWijzer

#INPUTS
#path_dataset --> string met path naar dataset KLW
#path_xml_files --> string met path naar mappen met input.xml files voor KLW. Hierop een script loslaten om bijproducten in te lezen.
#bijproducten_algmeen --> boolean: moeten de waardes van krachtvoer en bijproducten uit rantsoenberekening gehaald worden of specifiek uit input xml.

#BENODIGDE FUNCTIES:
#getDataInFolder
#XMLtoDataFrame (optioneel)


#Voor testen:
#path_dataset = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Dataset_VKA_2018_2020"
#output_folder = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst"

#Selecteren van kengetallen groepsoverzicht!
if(TRUE){
if(FALSE){
  kengetallenGroepsoverzicht = matrix(data = c(
    
    #META-data
    "Lidmaatschapsnummer", 0, TRUE,
    "Studiegroep", 0, TRUE,
    "Achternaam", 0, FALSE,
    "Tussenvoegsel", 0, FALSE,
    "Voornaam", 0, FALSE,
    "jaartal", 0, TRUE,
    
    #PAGINA 1
    "opp_totaal", 1, TRUE,
    "nkoe", 0, TRUE,
    "jvper10mk", 1, TRUE,
    "melkperha", 0 , TRUE,
    "melkprod", 0, TRUE,
    "urenweidenmelkkoeienNA", 0, TRUE,
    "verl_bodbal1_ha", 0 , TRUE,
    "kring2_bedbal_ovrbod", 0 , TRUE,
    "dzh_nh3_bedrha", 0 , TRUE,
    "pceigen_n", 0 , TRUE,
    "pceigen_n_buurt", 0 , TRUE,
    "rants_geh_re", 0 , TRUE,
    "dzh_co2_melkprod", 0 , TRUE,
    "opb_gras_ds", 0 , TRUE,
    "opb_mais_ds", 0 , TRUE,
    
    #PAGINA 2
    "opp_prgras", 1, TRUE,
    "opp_natuur", 1, TRUE,
    "opp_mais", 1, TRUE,
    "opp_overig", 1, TRUE,
    "opp_totaal", 1, TRUE,
    "aandeel_N_behoefte", 0, TRUE,
    "dzh_blijgras_aand", 0, TRUE,
    "grondsoort", 0, FALSE,
    "nkoe", 0 , TRUE,
    "npink", 0 , TRUE,
    "nkalf", 0 , TRUE,
    "jvper10mk", 1 , TRUE,
    "gveperha", 1 , TRUE,
    "dagenweidenmelkkoeienNA", 0 , TRUE,	
    "urenweidenmelkkoeienperdagNA", 1 , TRUE,
    "urenweidenmelkkoeienNA", 0 , TRUE,
    "dgnweidpiNA", 0 , TRUE,
    "zstvdagenNA", 0 , TRUE,
    
    #PAGINA 3
    "melkprod", 0 , TRUE,
    "fpcmperha", 0 , TRUE,
    "vet", 2 , TRUE,
    "eiwit", 2 , TRUE,
    "ureum", 0 , TRUE,
    "fosfor", 0 , TRUE,
    "melkpkoe", 0 , TRUE,
    "fpcmkoejaar", 0 , TRUE,
    "rants_geh_re", 0 , TRUE,
    "rants_geh_p", 1 , TRUE,
    "rants_geh_vem", 0 , TRUE,
    "rants_re_kvem", 0 , TRUE,
    "rants_p_kvem", 1 , TRUE,
    "kv_geh_re", 0 , TRUE,# g /kg wordt g / kg ds! in groepsoverzicht aanpassen!
    "kvper100kgmelk", 1 , TRUE,
    "kvperkoe", 0 , TRUE,
    
    #PAGINA 4
    "gr_aandeel", 0 , TRUE,
    "gk_aandeel", 0 , TRUE,
    "sm_aandeel", 0 , TRUE,
    "ov_aandeel", 0 , TRUE,
    "kv_aandeel", 0 , TRUE,
    "pcvoordeelspec1", 0 , TRUE,
    "pcvoordeelspec2", 0 , TRUE,
    "excretie1_melk", 1 , TRUE,
    "excretie2_melk", 1 , TRUE,
    "BEX_N_kg_koe", 0 , TRUE,
    "BEX_P_kg_koe", 0 , TRUE,
    "NP_BEX", 1 , TRUE,
    "voereff_fpcm", 2 , TRUE,
    "efficientie_N", 1 , TRUE,
    "efficientie_P", 1 , TRUE,
    
    #PAGINA 5
    "opb_graspr_ds", 0 , TRUE,
    "opb_graspr_kvem", 0 , TRUE,
    "opb_graspr_n", 0 , TRUE,
    "opb_graspr_p2o5", 0 , TRUE,
    "aanleg_gk_re", 0 , TRUE,
    "aanleg_gk_p", 1 , TRUE,
    "aanleg_gk_vem", 0 , TRUE,
    "opb_mais_ds", 0 , TRUE,
    "opb_mais_vem_g_kg", 0 , TRUE,
    "opb_mais_n", 0 , TRUE,
    "opb_mais_p2o5", 0 , TRUE,
    "N_generiek", 0 , TRUE,
    "BES_N_3jaar", 0 , TRUE, #alleen voor meest recente jaar weergeven! 
    "P_generiek", 0, TRUE,
    "BES_P_3jaar", 0 , TRUE, #alleen voor meest recente jaar weergeven! 
    "kring1_benut_tot", 0 , TRUE,
    "kring2_benut_tot", 0 , TRUE,
    "kring1_benut_bod", 0 , TRUE,
    "kring2_benut_bod", 0 , TRUE,
    
    #PAGINA 6
    "graspr_dmst_m3", 0 , TRUE,
    "graspr_dmst_kgn", 0 , TRUE,
    "graspr_wmst_kgn", 0 , TRUE,
    "graspr_kmst_kgn", 0 , TRUE,
    "graspr_totaal_kgn", 0 , TRUE,
    "graspr_dmst_kgp2o5", 0 , TRUE,
    "graspr_wmst_kgp2o5", 0 , TRUE,
    "graspr_kmst_kgp2o5", 0 , TRUE,
    "graspr_totaal_kgp2o5", 0 , TRUE,
    "mais_dmst_m3", 0 , TRUE,
    "mais_dmst_kgn", 0 , TRUE,
    "mais_kmst_kgn", 0 , TRUE,
    "mais_totaal_kgn", 0 , TRUE,
    "mais_dmst_kgp2o5", 0 , TRUE,
    "mais_kmst_kgp2o5", 0 , TRUE,
    "mais_totaal_kgp2o5", 0 , TRUE,
    "verl_bodbal1_ha", 0 , TRUE,
    "max_N_bodemoverschot", 0 , TRUE,
    
    #PAGINA 7
    "dzh_nh3_stalgve", 0 , TRUE,
    "dzh_nh3_bedrgve", 0 , TRUE,
    "em_nh3_stal_ha", 0 , TRUE, #Deze zit niet in dzh?
    "dzh_nh3_landha", 0 , TRUE,
    "dzh_nh3_bedrha", 0 , TRUE,
    "dzh_co2_pensferm", 0 , TRUE,
    "dzh_co2_mestopsl", 0 , TRUE,
    "dzh_co2_voerprod", 0 , TRUE,
    "dzh_co2_energie", 0 , TRUE,
    "dzh_co2_aanvoer", 0 , TRUE,
    "dzh_co2_melkprod", 0 , TRUE,
    "bkg_alloc_fpcm", 2 , TRUE,
    "bkg_alloc_vlees", 2 , TRUE 
    
    
  ), ncol = 3, byrow = T) 
  
} else {
  
  #MELK EN KLIMAAT / THEMABIJEENKOMST "INTEGRALE AANPAK STIKSTOF EN METHAAN".
  data_VKA_KLW_Totaal$som1 = data_VKA_KLW_Totaal$co2_pens_vg + data_VKA_KLW_Totaal$co2_pens_gk + data_VKA_KLW_Totaal$co2_pens_sm + data_VKA_KLW_Totaal$co2_pens_nt +data_VKA_KLW_Totaal$co2_pens_kv +data_VKA_KLW_Totaal$co2_pens_mp
  data_VKA_KLW_Totaal$som2 = data_VKA_KLW_Totaal$co2_stal_ch4_m + data_VKA_KLW_Totaal$co2_stal_n2o_d + data_VKA_KLW_Totaal$co2_stal_n2o_i
  data_VKA_KLW_Totaal$som3 = data_VKA_KLW_Totaal$co2_voer_n2o_wm + data_VKA_KLW_Totaal$co2_voer_n2o_km + data_VKA_KLW_Totaal$co2_voer_n20_om + data_VKA_KLW_Totaal$co2_voer_n2o_ve + data_VKA_KLW_Totaal$co2_voer_overig
  data_VKA_KLW_Totaal$som4 = data_VKA_KLW_Totaal$co2_ene_elek + data_VKA_KLW_Totaal$co2_ene_dies + data_VKA_KLW_Totaal$co2_ene_ngas + data_VKA_KLW_Totaal$co2_ene_prop + data_VKA_KLW_Totaal$co2_ene_olie + data_VKA_KLW_Totaal$co2_ene_prod
  data_VKA_KLW_Totaal$som5 = data_VKA_KLW_Totaal$co2_aanv_mech + data_VKA_KLW_Totaal$co2_aanv_drog + data_VKA_KLW_Totaal$co2_aanv_gras + data_VKA_KLW_Totaal$co2_aanv_mais + data_VKA_KLW_Totaal$co2_aanv_ovbp + data_VKA_KLW_Totaal$co2_aanv_kvmp + data_VKA_KLW_Totaal$co2_aanv_mest + data_VKA_KLW_Totaal$co2_aanv_vee + data_VKA_KLW_Totaal$co2_aanv_water + data_VKA_KLW_Totaal$co2_aanv_stroo +  data_VKA_KLW_Totaal$co2_aanv_gwbm +  data_VKA_KLW_Totaal$co2_aanv_afdek
  
  kengetallenGroepsoverzicht = matrix(data = c(
    
    #META-data
    "VKA-lidmaatschapsnummer", 0, TRUE,
    "Studie-groep", 0, TRUE,
    "Achternaam", 0, FALSE,
    "Tussen-voegsel", 0, FALSE,
    "Voornaam", 0, FALSE,
    "jaartal", 0, TRUE,
    
    #PAGINA 1
    "opp_totaal", 1, TRUE,
    "nkoe", 0, TRUE,
    "jvper10mk", 1, TRUE,
    "melkperha", 0 , TRUE,
    "melkprod", 0, TRUE,
    "urenweidenmelkkoeienNA", 0, TRUE,
    "verl_bodbal1_ha", 0 , TRUE,
    "kring2_bedbal_ovrbod", 0 , TRUE,
    "dzh_nh3_bedrha", 0 , TRUE,
    "pceigen_n", 0 , TRUE,
    "pceigen_n_buurt", 0 , TRUE,
    "rants_geh_re", 0 , TRUE,
    "dzh_co2_melkprod", 0 , TRUE,
    "opb_gras_ds", 0 , TRUE,
    "opb_mais_ds", 0 , TRUE,
    
    #PAGINA 2
    "opp_prgras", 1, TRUE,
    "opp_natuur", 1, TRUE,
    "opp_mais", 1, TRUE,
    "opp_overig", 1, TRUE,
    "opp_totaal", 1, TRUE,
    "aandeel_N_behoefte", 0, TRUE,
    "dzh_blijgras_aand", 0, TRUE,
    "grondsoort", 0, FALSE,
    "nkoe", 0 , TRUE,
    "npink", 0 , TRUE,
    "nkalf", 0 , TRUE,
    "jvper10mk", 1 , TRUE,
    "gveperha", 1 , TRUE,
    "dagenweidenmelkkoeienNA", 0 , TRUE,	
    "urenweidenmelkkoeienperdagNA", 1 , TRUE,
    "urenweidenmelkkoeienNA", 0 , TRUE,
    "dgnweidpiNA", 0 , TRUE,
    "zstvdagenNA", 0 , TRUE,
    
    #PAGINA 3
    "melkprod", 0 , TRUE,
    "fpcmperha", 0 , TRUE,
    "vet", 2 , TRUE,
    "eiwit", 2 , TRUE,
    "ureum", 0 , TRUE,
    "fosfor", 0 , TRUE,
    "melkpkoe", 0 , TRUE,
    "fpcmkoejaar", 0 , TRUE,
    "rants_geh_re", 0 , TRUE,
    "rants_geh_p", 1 , TRUE,
    "rants_geh_vem", 0 , TRUE,
    "rants_re_kvem", 0 , TRUE,
    "rants_p_kvem", 1 , TRUE,
    "kv_geh_re", 0 , TRUE,# g /kg wordt g / kg ds! in groepsoverzicht aanpassen!
    "kvper100kgmelk", 1 , TRUE,
    "kvperkoe", 0 , TRUE,
    
    #PAGINA 4
    "gr_aandeel", 0 , TRUE,
    "gk_aandeel", 0 , TRUE,
    "sm_aandeel", 0 , TRUE,
    "ov_aandeel", 0 , TRUE,
    "kv_aandeel", 0 , TRUE,
    "pcvoordeelspec1", 0 , TRUE,
    "pcvoordeelspec2", 0 , TRUE,
    "excretie1_melk", 1 , TRUE,
    "excretie2_melk", 1 , TRUE,
    "BEX_N_kg_koe", 0 , TRUE,
    "BEX_P_kg_koe", 0 , TRUE,
    "NP_BEX", 1 , TRUE,
    "voereff_fpcm", 2 , TRUE,
    "efficientie_N", 1 , TRUE,
    "efficientie_P", 1 , TRUE,
    
    #PAGINA 5
    "opb_graspr_ds", 0 , TRUE,
    "opb_graspr_kvem", 0 , TRUE,
    "opb_graspr_n", 0 , TRUE,
    "opb_graspr_p2o5", 0 , TRUE,
    "aanleg_gk_re", 0 , TRUE,
    "aanleg_gk_p", 1 , TRUE,
    "aanleg_gk_vem", 0 , TRUE,
    "opb_mais_ds", 0 , TRUE,
    "opb_mais_vem_g_kg", 0 , TRUE,
    "opb_mais_n", 0 , TRUE,
    "opb_mais_p2o5", 0 , TRUE,
    "kring1_benut_tot", 0 , TRUE,
    "kring2_benut_tot", 0 , TRUE,
    "kring1_benut_bod", 0 , TRUE,
    "kring2_benut_bod", 0 , TRUE,
    
    #PAGINA 6
    "graspr_dmst_m3", 0 , TRUE,
    "graspr_dmst_kgn", 0 , TRUE,
    "graspr_wmst_kgn", 0 , TRUE,
    "graspr_kmst_kgn", 0 , TRUE,
    "graspr_totaal_kgn", 0 , TRUE,
    "graspr_dmst_kgp2o5", 0 , TRUE,
    "graspr_wmst_kgp2o5", 0 , TRUE,
    "graspr_kmst_kgp2o5", 0 , TRUE,
    "graspr_totaal_kgp2o5", 0 , TRUE,
    "mais_dmst_m3", 0 , TRUE,
    "mais_dmst_kgn", 0 , TRUE,
    "mais_kmst_kgn", 0 , TRUE,
    "mais_totaal_kgn", 0 , TRUE,
    "mais_dmst_kgp2o5", 0 , TRUE,
    "mais_kmst_kgp2o5", 0 , TRUE,
    "mais_totaal_kgp2o5", 0 , TRUE,
    "verl_bodbal1_ha", 0 , TRUE,
    "max_N_bodemoverschot", 0 , TRUE,

    #PAGINA 7
    "dzh_nh3_stalgve", 0 , TRUE,
    "dzh_nh3_bedrgve", 0 , TRUE,
    "em_nh3_ha_stalopslag", 0 , TRUE, #Deze zit niet in dzh?
    "em_nh3_ha_mesttoediening", 0 , TRUE,
    "em_nh3_ha_beweid", 0 , TRUE,
    "em_nh3_hagrond", 0, TRUE,
    
    #"dzh_nh3_bedrha", 0 , TRUE,
    "dzh_co2_pensferm", 0 , TRUE,
    "dzh_co2_mestopsl", 0 , TRUE,
    "dzh_co2_voerprod", 0 , TRUE,
    "dzh_co2_energie", 0 , TRUE,
    "dzh_co2_aanvoer", 0 , TRUE,
    "dzh_co2_melkprod", 0 , TRUE,
    "bkg_alloc_fpcm", 2 , TRUE,
    "bkg_alloc_vlees", 2 , TRUE,
    
    #PAGINA 8 
    
    "co2_pens_vg", 0 , TRUE,
    "co2_pens_gk", 0 , TRUE,
    "co2_pens_sm", 0 , TRUE,
    "co2_pens_nt", 0 , TRUE,
    "co2_pens_kv", 0 , TRUE,
    "co2_pens_mp", 0 , TRUE,
    "som1", 0 , TRUE,
    "co2_stal_ch4_m", 0 , TRUE,
    "co2_stal_n2o_d", 0 , TRUE,
    "co2_stal_n2o_i", 0 , TRUE,
    "som2", 0 , TRUE,
    "co2_voer_n2o_wm", 0 , TRUE,
    "co2_voer_n2o_km", 0 , TRUE,
    "co2_voer_n20_om", 0 , TRUE,
    "co2_voer_n2o_ve", 0 , TRUE,
    "co2_voer_overig", 0 , TRUE,
    "som3", 0 , TRUE,
    "co2_ene_elek", 0 , TRUE,
    "co2_ene_dies", 0 , TRUE,
    "co2_ene_ngas", 0 , TRUE,
    "co2_ene_prop", 0 , TRUE,
    "co2_ene_olie", 0 , TRUE,
    "co2_ene_prod", 0 , TRUE,
    "som4", 0 , TRUE,
    "co2_aanv_mech", 0 , TRUE,
    "co2_aanv_drog", 0 , TRUE,
    "co2_aanv_gras", 0 , TRUE,
    "co2_aanv_mais", 0 , TRUE,
    "co2_aanv_ovbp", 0 , TRUE,
    "co2_aanv_kvmp", 0 , TRUE,
    "co2_aanv_mest", 0 , TRUE,
    "co2_aanv_vee", 0 , TRUE,
    "co2_aanv_water", 0 , TRUE,
    "co2_aanv_stroo", 0 , TRUE,
    "co2_aanv_gwbm", 0 , TRUE,
    "co2_aanv_afdek", 0 , TRUE,
    "som5", 0 , TRUE
    
  ), ncol = 3, byrow = T) 
  
}
}

opzettenGroepsoverzicht <- function(path_dataset, output_folder, kengetallenGroepsoverzicht, jaartal_start, jaartal_eind){
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Laden libraries
  library(dplyr)  
  library(openxlsx)
  
  #Het laden van benodigde functies die ook op GIT staan.
  library(devtools)
  scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R")
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  if(!is.numeric(jaartal_start)){
    stop("jaartal_start is geen numeric")
  }
  
  if(!is.numeric(jaartal_eind)){
    stop("jaartal_eind is geen numeric")
  }
  
  if(jaartal_start > jaartal_eind){
    stop("jaartal_start mag niet groter zijn dan jaartal_eind")
  }
  
  jaartallen = jaartal_start : jaartal_eind
  
  #Wat is de working directory:
  if(file.exists(output_folder)){
    setwd(output_folder)
  }
  
  outputToLog("Working Directory is", getwd())
  
  #Inlezen van Ledenlijst
  if(file.exists(path_dataset)){
    data_groepsoverizchten_input = getDataInFolder(path_dataset)
  } else {
    stop("Path naar groepsoverzichtdata bestaat niet!")
  }
  
  #Zijn de benodigde parameters aanwezig?
  namen_kengetallen = kengetallenGroepsoverzicht[,1]
  
  if (all(namen_kengetallen %in% colnames(data_groepsoverizchten_input))) {
    data_groepsoverizchten_input = data_groepsoverizchten_input[, data_groepsoverizchten_input]
  } else {
    
    undefined_kengetallen = setdiff(namen_kengetallen, colnames(data_groepsoverizchten_input))
    
    if(length(undefined_kengetallen) > 0){
      print(undefined_kengetallen)
    }
    
    stop("Databestand heeft niet de headers zoals gespecificeerd in template groepsoverzicht")
  }  

  rownames(kengetallenGroepsoverzicht) = namen_kengetallen
  
  data_groepsoverzicht_volledig = data_groepsoverizchten_input
  names(data_groepsoverzicht_volledig) = rownames(kengetallenGroepsoverzicht)
  
  #Verzekeren dat de kolomnamen uniek zijn!
  names(data_groepsoverzicht_volledig) = make.unique(names(data_groepsoverzicht_volledig),sep = ".")
  
  #Voor welke jaren gaan we het overzicht maken?
  data_groepsoverzicht_volledig = data_groepsoverzicht_volledig %>% 
    dplyr::filter(jaartal %in% c(jaartallen))
  
  #Boeren "ordenen" straks op intensiteit. Bereken nu alvast de gemiddelde intensiteit van de boeren.
  data_groepsoverzicht_volledig = data_groepsoverzicht_volledig %>% dplyr::group_by(Lidmaatschapsnummer) %>% dplyr::mutate(melkperha_avg = mean(melkperha, na.rm = T))
  
  studiegroepen = as.vector(na.omit(unique(data_groepsoverzicht_volledig$Studiegroep)))
  
  specifieke_groep = NULL
  
  for(g in studiegroepen){
    
    #Itereren over alle groepen, of enkel de specifieke groep?
    if(!is.null(specifieke_groep)){
      if(g != specifieke_groep){
        next
      }
    }
    
    data_groep_iteratie = data_groepsoverzicht_volledig %>% dplyr::filter(Studiegroep == g)
    
    
    #Alle groepsleden in deze groep
    groepsleden = unique(data_groep_iteratie$Lidmaatschapsnummer)
    
    #Zijn er in deze groep deelnemers met een dubbele Achternaam?
    dubbeleAchternaamBoolean = FALSE
    #Zijn er leden met dezelfde achternaam?
    if(length(groepsleden) > length(unique(dataStudiegroep$Achternaam))){
      
      #Ja, er zijn leden met dezelfde achternaam. Welke precies?
      dubbele_achternaam = pull(data_groep_iteratie %>% 
        dplyr::group_by(Achternaam) %>% 
        dplyr::summarise(count = dplyr::n()) %>%
        dplyr::filter(count > 1) %>%
        dplyr::select(Achternaam))
      
      for(naam in as.character(dubbele_achternaam)){
        
        #Welke boeren hebben deze achternaam?
        Pseudo_achternaam = data_groep_iteratie %>%
          dplyr::filter(grepl(naam, Achternaam)) %>%
          dplyr::mutate(Achternaam2 = paste(Achternaam, " (", Voornaam, ")", sep = "")) %>%
          dplyr::select(Lidmaatschapsnummer, Achternaam2)
        
        dubbeleAchternaamBoolean = TRUE
        
      }
      
    }
    
    #Achternaam hernoemen, toevoegen van tussenvoegsel
    data_groep_iteratie$Achternaam = ifelse(is.na(data_groep_iteratie$Tussenvoegsel), data_groep_iteratie$Achternaam, paste(data_groep_iteratie$Tussenvoegsel," ",data_groep_iteratie$Achternaam, sep = ""))
    
    #Toevoegen van "dummy" deelnemer. Dit wordt gebruikt als groepsgemiddelde.
    groepsleden_nummer_achternaam = data_groep_iteratie %>% dplyr::select(Lidmaatschapsnummer, Achternaam, Voornaam)
    dummy_groepslid = data.frame(Lidmaatschapsnummer = 0, Achternaam = "Groepsgemiddelde", Voornaam = "Groepsgemiddelde")
    groepsleden_nummer_achternaam = rbind(groepsleden_nummer_achternaam, dummy_groepslid )
    
    data_groep_iteratie = data_groep_iteratie %>% 
      dplyr::arrange(melkperha_avg) %>%
      dplyr::select(!Tussenvoegsel) %>%
      dplyr::select(!Studiegroep) 

    #Welke boeren doen mee met 3 jaar data?
    data_groep_iteratie_3jaar = data_groep_iteratie %>%
      dplyr::group_by(Lidmaatschapsnummer) %>%
      dplyr::mutate(count = dplyr::n()) %>%
      dplyr::mutate(volledige_data = ifelse(count == length(jaartallen), 1, 0))
    

    #Bereken het groepsgemiddelde per jaar!
    
    #Als deze functie geen data heeft dan is het resultaat NaN. We willen liever NA (makkelijker Excel). 
    #Daarvoor is deze methodiek: https://stackoverflow.com/questions/52490552/r-convert-nan-to-na
    is.nan.data.frame <- function(x){
      do.call(cbind, lapply(x, is.nan))
    }
    
    data_groep_gemiddelde = data_groep_iteratie_3jaar %>% dplyr::filter(volledige_data > 0) %>%
      dplyr::group_by(jaartal) %>%
      dplyr::summarise_all(mean, na.rm = TRUE) 
    
    data_groep_gemiddelde[is.nan(data_groep_gemiddelde)] = NA
    data_groep_gemiddelde$Achternaam = "Groepsgemiddelde"
    data_groep_gemiddelde$volledige_data = 1
    data_groep_gemiddelde$Lidmaatschapsnummer = 0
    data_groep_gemiddelde$melkperha_avg = 100000 #Zodat groepsgemiddelde altijd bovenaan komt te staan
    data_groep_gemiddelde = data_groep_gemiddelde %>% select(Achternaam, everything())
    
    data_groep_totaal = merge(data_groep_iteratie,data_groep_gemiddelde, all = TRUE)
    
    #Bereken het meerjarig gemiddelde per lid (en dus ook groepsgemiddelde meerjarig)
    
    data_groep_totaal_gemiddeldes = data_groep_totaal %>% 
      dplyr::filter(volledige_data > 0) %>%
      dplyr::group_by(Lidmaatschapsnummer) %>%
      dplyr::summarise_all(mean, na.rm = TRUE)
    
    #Toevoegen van dummy jaartal als "3-jarig gemiddelde". Gemiddelde over jaren 2018,2019 en 2020 wordt dan "2018-2020".
    data_groep_totaal_gemiddeldes$jaartaldummy = paste(as.character((jaartal_start)),"-",as.character((jaartal_eind)),sep = "")
    #Voor deze "jaartaldummy" zorgen we dat het jaartal groter is dan het maximale jaartal in de data. Zo kunnen we straks "ordenen" op basis van jaartallen, en komt het gemiddelde onderaan te staan.
    data_groep_totaal_gemiddeldes$jaartal = 9999
    
    #Zorg dat in de originele data ook een kolom "jaartaldummy" komt.
    data_groep_totaal$jaartaldummy = as.character(data_groep_totaal$jaartal)
    
    #Alles samenvoegen; het groepsgemiddelde en de gemiddeldes van de leden
    
    data_groep_final = merge(data_groep_totaal, data_groep_totaal_gemiddeldes, all = TRUE)
    
    #We willen enkel het meest recente jaar én het x-jarig gemiddelde weergeven. Dat betekent jaartal_eind en jaartaldummy (9999).
    #Voor de bedrijven die niet compleet zijn willen we wél alle jaren weergeven.
    
    #Het echte jaartal is opgeslagen in "jaartaldummy". We gebruiken jaartal nu om de selectie op te baseren
    data_groep_final$jaartal = ifelse(data_groep_final$volledige_data < 1, 9999, data_groep_final$jaartal)
    data_groep_final = data_groep_final %>% dplyr::filter(jaartal %in% c(9999, jaartal_eind))
    
    #Data ordenen op basis van intensiteit
    data_groep_final = data_groep_final %>% dplyr::arrange(desc(melkperha_avg), jaartal)
    
    #NaN naar NA
    data_groep_final[is.nan(data_groep_final)] <- NA
    
    #Voor welke kengetallen willen we geen NA maar "0" weergeven?
    kengetallen_0 = c(
      "urenweidenmelkkoeienNA",
      "urenweidenmelkkoeienNA.1",
      "zstvdagenNA",
      "dagenweidenmelkkoeienNA",
      "dgnweidpiNA"
    )
    
    #Functie om van NA naar "0" te gaan.
    NAtoZero <- function(x){
      outputColumn = ifelse(is.na(x),0,x)
      return(outputColumn)
    }
    
    data_groep_final = data_groep_final %>% dplyr::mutate(across(kengetallen_0, NAtoZero))
    

      
    #Zet wel weer naar NA voor jaren waar er geen KLW beschikbaar is, 
    #anders staat er "0" waar helemaal geen KLW data is
    index_geen_KLW = is.na(data_groep_final$nkoe)
    data_groep_final$urenweidenmelkkoeienNA[index_geen_KLW] = NA
    data_groep_final$urenweidenmelkkoeienNA.1[index_geen_KLW] = NA
    data_groep_final$zstvdagenNA[index_geen_KLW] = NA
    data_groep_final$dagenweidenmelkkoeienNA[index_geen_KLW] = NA
    data_groep_final$dgnweidpiNA[index_geen_KLW] = NA
    
    #Logica om lege regels (NA) tussen de boeren toe te voegen.
    
    data_NA = as.data.frame(matrix(data = NA, nrow = 1, ncol = ncol(data_groep_final)))
    names(data_NA) = names(data_groep_final)
    
    dataStudiegroep_NA = NULL
    lidnummerPrevious = data_groep_final[1, "Lidmaatschapsnummer"]
    previousRow = 1
    
    #FOR-loop om lege regels tussen de boeren in te voegen op basis van verspringende jaartallen.
    for(r in 1:nrow(data_groep_final)){
      
      lidnummer = data_groep_final[r, "VKA-lidmaatschapsnummer"]
      
      if(is.na(lidnummer)){
        lidnummer = lidnummerPrevious
      }
      
      #Een nieuw VKA-lid gevonden? Of zijn we bij de laatste
      if(lidnummer != lidnummerPrevious || r==nrow(data_groep_final) ) {
        
        #In geval van de laatste rij
        if(r==nrow(data_groep_final)){
          dataStudiegroep_NA = rbind(dataStudiegroep_NA, data_NA, data_groep_final[previousRow:(r),])
          
        } else { #Anders: 
          dataStudiegroep_NA = rbind(dataStudiegroep_NA, data_NA, data_groep_final[previousRow:(r-1),])
        }
        
        previousRow = r
        
      }
      
      lidnummerPrevious = lidnummer
      
    }
    
    a = data.frame(test = c(1,1,1,2,2,2,3,3,4,4,4))
    b = a %>% dplyr::group_by(test) %>% mutate(count = n())
  }
  
  
}