#Jur Eekelder; 31-08-2021
#jur.eekelder@demarke.eu
#Script voor het maken van produceren van Voerwinst
#Idee gebaseerd op Marcel van Ittersum (mm.vanittersum@countus.nl) en Jaap Gielen (jaap.gielen@demarke.eu)

#INPUTS
#path_dataset --> string met path naar dataset KLW
#path_xml_files --> string met path naar mappen met input.xml files voor KLW. Hierop een script loslaten om bijproducten in te lezen.
#bijproducten_algmeen --> boolean: moeten de waardes van krachtvoer en bijproducten uit rantsoenberekening gehaald worden of specifiek uit input xml.

#BENODIGDE FUNCTIES:
#getDataInFolder
#XMLtoDataFrame (optioneel)


#Voor testen:
#path_xml_files = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst/Input_XML_2018_2020"
#path_dataset = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Dataset_VKA_2018_2020"
#output_folder = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst"


berekenVoerwinst <- function(path_dataset = NULL, dataset_VKX = NULL, path_xml_files = NULL, output_folder = NULL, bijproducten_algemeen = TRUE, produce_plots = FALSE){
  
  output_file_string = "zonder_XML"
  
  if(!bijproducten_algemeen){
    
    output_file_string = "met_XML"
    
    if(is.null(path_xml_files)){
      stop("Bijproducten moeten uit input XML komen maar path is niet opgegeven")
    }
    
    if(!file.exists(path_xml_files)){
      stop("Path naar input XML folder bestaat niet")
    }
  }
  
  #Laden libraries
  library(dplyr)  
  library(openxlsx)
  
  #Het laden van benodigde functies die ook op GIT staan.
  library(devtools)
  scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R",
                        "https://raw.githubusercontent.com/jureekelder/VKA/main/R/XMLtoDataFrame.R")
  
  
  for(script in scripts_to_source){
    source_url(script)
  }
  
  
  #Inlezen van dataset
  if(!is.null(path_dataset)){
    if(file.exists(path_dataset)){
      dataset = getDataInFolder(path_dataset)
    } else {
      stop("Path naar dataset bestaat niet!")
    }
  } else {
    warning("Geen dataset wordt ingelezen vanuit bestand")
    
    if(is.null(dataset_VKX)){
      stop("Geen VKX dataset gegeven!, maar ook geen input locatie voor ophalen van data")
    } else {
      dataset = dataset_VKX
    }
    
  }
  
  if(!is.null(output_folder)){
    #Opzetten output path
    if(file.exists(output_folder)){
      setwd(output_folder)
    } else {
      warning("Path naar voor output bestaat niet!")
      produce_plots = FALSE
    }
  } else {
    warning("Geen output path opgegeven, dan worden er ook geen plots gemaakt.")
    produce_plots = FALSE
  }
  
  #Parameters
  prijs_vet = 293
  prijs_eiwit = 586
  prijs_lactose = 59
  procent_lactose = 4.51
  netto_weidepremie = 1.50
  criterium_weidegang = 720
  
  procent_sterfte_koeien = 4
  procent_sterfte_nuka = 11
  
  prijs_aankoop_koeien = 1400
  prijs_aankoop_jongvee_kl1jaar = 103
  prijs_aankoop_jongvee_gr1jaar = 1250
  prijs_aankoop_nuka = 103
  
  prijs_verkoop_koeien = 650
  prijs_verkoop_jongvee_kl1jaar = 103
  prijs_verkoop_jongvee_gr1jaar = 490
  prijs_verkoop_nuka = 103
  
  stikstof_gehalte_mest = 4
  
  GVE_melkkoeien_factor = 1
  GVE_jongvee_kl1jaar_factor = 0.23
  GVE_jongvee_gr1jaar_factor = 0.53
  
  kosten_dm_gras = 3
  kosten_dm_bouw = 3.5
  kosten_kmst_n = 0.87
  kosten_kmst_p = 0.81
  
  kosten_zaad_gras = 25
  kosten_zaad_mais = 225
  
  kosten_gbm_gras = 14
  kosten_gbm_mais = 76
  
  kosten_oogst_mais = 491
  kosten_oogst_graskuil = 0.07
  
  prijs_aankoop_graskuil = 0.1760
  prijs_verkoop_graskuil = 0.0940
  prijs_aankoop_maiskuil = 0.1500
  prijs_verkoop_maiskuil = 0.1120
  prijs_aankoop_overig_ruwvoer = 0.2000
  prijs_aankoop_melkpoeder = 2.000
  
  kosten_overig_gras = 29+9+13+16+28+12 + kosten_zaad_gras + kosten_gbm_gras
  kosten_overig_mais = 134+63+80+40+63+28+10+45+65 + kosten_zaad_mais + kosten_gbm_mais + kosten_oogst_mais
  
  prijs_kvem_bijproduct = 17
  correctie_re_dve_bijproduct = 0.91
  prijs_dve_bijproduct = 85
  procent_voederwaarde_prijs_bijproduct = 0.74
  
  prijs_kvem_krachtvoer = 17
  correctie_re_dve_krachtvoer = 0.70
  prijs_dve_krachtvoer = 85

  ds_gehalte_krachtvoer = 0.897
  ds_gehalte_melkpoeder = 0.963
  ds_gehalte_overig_ruwvoer = 0.90
  
  #Deze aanpassen naar VKA gemiddelde correcitefactor_krachtvoer.
  gemiddeld_VKA_kv_geh_vem = 1113 * ds_gehalte_krachtvoer
  gemiddeld_VKA_kv_geh_re = 210 * ds_gehalte_krachtvoer
  
  voederwaardeprijs_krachtvoer = (gemiddeld_VKA_kv_geh_vem / 1000) * prijs_kvem_krachtvoer + (gemiddeld_VKA_kv_geh_re * correctie_re_dve_krachtvoer / 1000) * prijs_dve_krachtvoer
  
  norm_prijs_krachtvoer = 27.50 #op basis van KWIN
  
  correctiefactor_krachtvoer = norm_prijs_krachtvoer / voederwaardeprijs_krachtvoer
  procent_bedrijfsspecifiek_krachtvoer = 1
  toeslag_mineralen = 0.4875
  prijs_mineralen = 0.600 # per kg product
  


  
  prijs_overig_ruwvoer_ds = 130/(1000*0.9)
  
  
  #Kolommen die we nodig hebben
  kolommen_voerwinst = c("jaartal",
                     "kvk_nummer",
                     "Lidmaatschapsnummer",
                     "melk_bedr",
                     "melk_vet",
                     "melk_eiwit",
                     "urenweidenmelkkoeien",
                     "nkoe_ak",
                     "nkalf_ak",
                     "npink_ak",
                     "nnuka_ak",
                     "nkoe_vk",
                     "nkalf_vk",
                     "npink_vk",
                     "nnuka_vk",
                     "nkoe",
                     "npink",
                     "nkalf",
                     "oppgras",
                     "opp_prgras",
                     "opp_natuur",
                     "opp_mais",
                     "opp_overig",
                     "opp_totaal",
                     "opb_gras_ds",
                     "opb_graspr_ds",
                     "opb_natuur_ds",
                     "opb_mais_ds",
                     "opb_gras_bruto",
                     "opb_graspr_bruto",
                     "opb_natuur_bruto",
                     "opb_mais_bruto",
                     "gr_verbruik",
                     "gk_verbruik",
                     "sm_verbruik",
                     "ov_verbruik",
                     "kv_verbruik",
                     "mp_verbruik",
                     "gebrnorm1",
                     "gebrnorm2",
                     "excr_spec1",
                     "excr_spec2",
                     "mais_dmst_m3",
                     "akker_dmst_m3",
                     "natuur_kmst_kgn",
                     "graspr_kmst_kgn",
                     "natuur_kmst_kgp2o5",
                     "graspr_kmst_kgp2o5",
                     "mais_kmst_kgn",
                     "mais_kmst_kgp2o5",
                     "ov_geh_vem",
                     "ov_geh_re",
                     "kv_geh_vem",
                     "kv_geh_re",
                     "weidka_dgn",
                     "weidpi_dgn",
                    
                     #Parameters voor vergelijkingen
                     "pceigen_n",
                     "verl_bodbal1_ha",
                     "kvper100kgmelk",
                     "rants_geh_re",
                     "dzh_co2_melkprod",
                     "em_nh3_tonmelk", 
                     "melkperha",
                     "jvper10mk",
                     "aanleg_gk_hoev", 
                     "gk_geh_re",
                     "aanleg_gk_re",
                     "opb_graspr_re_g_kg",
                     "efficientie_N",
                     "voereff_melk",
                     "rants_re_kvem",
                     "gk_re_kvem",
                     "kv_re_kvem",
                     "pceigen_n_buurt",
                     "benut_n_bod"
                     
                     
                     
                     
                     
                     )
  
  if(all(kolommen_voerwinst %in% colnames(dataset))){
    dataset_voerwinst = dataset[, kolommen_voerwinst]
  } else {
    stop("Input dataset heeft niet de juiste headers")
  }
  
  
  
  bereken_melkprijs <- function(procent_vet, procent_eiwit, prijs_vet, prijs_eiwit, prijs_lactose, procent_lactose, netto_weidepremie, uren_weidegang, criterium_weidegang){
    
    #Berekening basis melkprijs
    melkprijs  = procent_vet / 100 * prijs_vet + procent_eiwit / 100 * prijs_eiwit + procent_lactose / 100 * prijs_lactose
    
    #Als weide-uren > criterium, dan de weidepremie toevoegen aan melkprijs
    if(uren_weidegang >= criterium_weidegang){
      melkprijs = melkprijs + netto_weidepremie
    }
    
    return(melkprijs)
    
  }
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::rowwise() %>% dplyr::mutate(melkprijs = bereken_melkprijs(
    procent_vet = melk_vet,
    procent_eiwit = melk_eiwit,
    prijs_vet = prijs_vet,
    prijs_eiwit = prijs_eiwit,
    prijs_lactose = prijs_lactose,
    procent_lactose = procent_lactose,
    netto_weidepremie = netto_weidepremie,
    uren_weidegang = urenweidenmelkkoeien,
    criterium_weidegang = criterium_weidegang
  ))

  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(melkgeld = melkprijs * melk_bedr / 100)
  

  # AAN- EN AFVOER VEE
  #
  # De sterfte was niet opgenomen waardoor afgevoerde dode dieren geld opleverden. In nieuwere versies van de klw is levend en dood afvoeren gescheiden. 
  # Dat is nog niet overal zo dus daarom hier op basis van aannames.
  # Uit cijfers van 2020 van Countus blijkt de sterfte onder melkkoeien 4% en onder kalveren 11%
  # Dit alles moet gecorrigeerd worden op de afvoercijfers. 
  # Voor koeien is het gemiddeld aantal aanwezige aantal koeien keer het sterftepercentage. Dat aantal is van de verkoop koeien afgehaald
  # Bij kalveren is het 11% sterfte van de geboren kalveren. Die zijn onbekend. Daarom aangenomen dat de afgevoerde kalveren + het aanwezige jongvee <1jr het aantal geboren kalveren is.
  # Daar het sterfte percentage van is het aantal gestorven nuka’s. Die zijn van de verkochte kalveren af gehaald. Soms is er geen verkoop van nuka’s ingevoerd. Die staan dan onder JV <1jr.  
  # Het komt dan voor dat er een negatief aantal bij nuka’s staat. Dat is niet erg want nuka’s en JV <1jr hebben dezelfde verkoopprijs. In totaal verkoop vee wordt het gesaldeerd.
  # 
  
  #Mutaties Vee
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(sterfte_koeien = nkoe * (procent_sterfte_koeien / 100))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(nnuka_njongvee_kleenjr = (nkalf_vk + nnuka_vk + nkalf)  )
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(sterfte_nuka = nnuka_njongvee_kleenjr * (procent_sterfte_nuka / 100))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_koeien = ifelse( ((nkoe_vk - sterfte_koeien) > nkoe_vk), 0, (nkoe_vk - sterfte_koeien)))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_jongvee_kl1jaar = nkalf_vk)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_jongvee_gr1jaar = npink_vk)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_nuka = nnuka_vk - sterfte_nuka)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_koeien_waarde = verkoop_koeien * prijs_verkoop_koeien)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_jongvee_kl1jaar_waarde = verkoop_jongvee_kl1jaar * prijs_verkoop_jongvee_kl1jaar)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_jongvee_gr1jaar_waarde = verkoop_jongvee_kl1jaar * prijs_verkoop_jongvee_gr1jaar)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_nuka_waarde = verkoop_nuka * prijs_verkoop_nuka)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verkoop_dieren_waarde = verkoop_koeien_waarde + verkoop_jongvee_kl1jaar_waarde + verkoop_jongvee_gr1jaar_waarde + verkoop_nuka_waarde)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(aankoop_koeien_waarde = nkoe_ak * prijs_aankoop_koeien)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(aankoop_jongvee_kl1jaar_waarde = nkalf_ak * prijs_aankoop_jongvee_kl1jaar)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(aankoop_jongvee_gr1jaar_waarde = npink_ak * prijs_aankoop_jongvee_gr1jaar)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(aankoop_nuka_waarde = nnuka_ak * prijs_aankoop_nuka)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(aankoop_dieren_waarde = aankoop_koeien_waarde + aankoop_jongvee_kl1jaar_waarde + aankoop_jongvee_gr1jaar_waarde + aankoop_nuka_waarde)
  
  
  #Arealen
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(opp_bouwland = opp_mais + opp_overig)
  
  #Opbrengsten gewassen netto
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(opb_gras_totaal_ds = oppgras * opb_gras_ds)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(opb_mais_totaal_ds = ifelse(opp_mais > 0, opp_mais * opb_mais_ds, NA))
  
  #Verbruik producten aan het voerhek --> vóór vervoederingsverliezen
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(gr_verbruik_voerhek = ifelse(gr_verbruik > 0, gr_verbruik / 1.0, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(gk_verbruik_voerhek = ifelse(gk_verbruik > 0, gk_verbruik / 0.95, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(sm_verbruik_voerhek = ifelse(sm_verbruik > 0, sm_verbruik / 0.95, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ov_verbruik_voerhek = ifelse(ov_verbruik > 0, ov_verbruik / 0.97, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_verbruik_voerhek = ifelse(kv_verbruik > 0, kv_verbruik / 0.98, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mp_verbruik_voerhek = ifelse(mp_verbruik > 0, mp_verbruik / 0.98, 0))
  
  #Verbruik producten zoals het in de kuil gaat --> vóór conserveringsverliezen
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(gr_verbruik_kuil = ifelse(gr_verbruik_voerhek > 0, gr_verbruik_voerhek / 1.0, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(gk_verbruik_kuil = ifelse(gk_verbruik_voerhek > 0, gk_verbruik_voerhek / 0.90, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(sm_verbruik_kuil = ifelse(sm_verbruik_voerhek > 0, sm_verbruik_voerhek / 0.96, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ov_verbruik_kuil = ifelse(ov_verbruik_voerhek > 0, ov_verbruik_voerhek / 0.96, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_verbruik_kuil = ifelse(kv_verbruik_voerhek > 0, kv_verbruik_voerhek / 1.0, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mp_verbruik_kuil = ifelse(mp_verbruik_voerhek > 0, mp_verbruik_voerhek / 1.0, 0))
  
  #Excretie en bemestingsruimte
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(excretie_NP = excr_spec1 / excr_spec2)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_stikstof = gebrnorm1 / stikstof_gehalte_mest)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_fosfaat = excr_spec2 / (stikstof_gehalte_mest / excretie_NP))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_gebruikt = min(ton_dm_stikstof, ton_dm_fosfaat))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_melkkoeien = nkoe * GVE_melkkoeien_factor)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_jongvee_kl1jr = nkalf * GVE_jongvee_kl1jaar_factor)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_jongvee_gr1jr = npink * GVE_jongvee_gr1jaar_factor)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_totaal = GVE_melkkoeien + GVE_jongvee_gr1jr + GVE_jongvee_kl1jr)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(uren_weiden_jongvee_kl1jr = weidka_dgn * 24)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(uren_weiden_jongvee_gr1jr = weidpi_dgn * 24)
  
  uren_in_jaar = 365*24
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_weidegang = (urenweidenmelkkoeien / uren_in_jaar) * nkoe + (uren_weiden_jongvee_kl1jr / uren_in_jaar) * nkalf + (uren_weiden_jongvee_gr1jr / uren_in_jaar) * npink)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(GVE_weidegang_aandeel = GVE_weidegang / GVE_totaal)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_uitgereden = ton_dm_gebruikt * (1 - GVE_weidegang_aandeel))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_maisland = ifelse(opp_mais > 0, ifelse(mais_dmst_m3 > 0, opp_mais * mais_dmst_m3, 0), 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_akkerland = ifelse(opp_overig > 0, ifelse(akker_dmst_m3 > 0, opp_overig * akker_dmst_m3, 0), 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_bouwland = ton_dm_maisland + ton_dm_akkerland)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(ton_dm_grasland = ton_dm_uitgereden - ton_dm_bouwland)
  
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_gras_totaal = ton_dm_grasland * kosten_dm_gras + (graspr_kmst_kgn * kosten_kmst_n + graspr_kmst_kgp2o5 * kosten_kmst_p + kosten_overig_gras) * oppgras)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_gras_totaal_kg_ds = kosten_gras_totaal / opb_gras_totaal_ds)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_mais_totaal = ton_dm_maisland * kosten_dm_bouw + (mais_kmst_kgn * kosten_kmst_n + mais_kmst_kgp2o5 * kosten_kmst_p + kosten_overig_mais) * opp_mais)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_mais_totaal = ifelse(kosten_mais_totaal > 1, kosten_mais_totaal, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_mais_totaal_kg_ds = kosten_mais_totaal / opb_mais_totaal_ds)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(grasproductie_graskuil = opb_gras_totaal_ds - gr_verbruik)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mutatie_graskuil = grasproductie_graskuil - gk_verbruik_kuil)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mutatie_maiskuil = opb_mais_totaal_ds - sm_verbruik_kuil)
  
  
  #Kosten grasproducten in rantsoen
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_eigen_graskuil = ifelse(mutatie_graskuil > 0, (grasproductie_graskuil - mutatie_graskuil) * (kosten_gras_totaal_kg_ds + kosten_oogst_graskuil), grasproductie_graskuil * (kosten_gras_totaal_kg_ds + kosten_oogst_graskuil )))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_aankoop_graskuil = ifelse(mutatie_graskuil < 0, abs(mutatie_graskuil) * prijs_aankoop_graskuil, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_vers_gras = ifelse(gr_verbruik_kuil > 0, gr_verbruik_kuil * kosten_gras_totaal_kg_ds, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_gras_rantsoen = kosten_eigen_graskuil + kosten_aankoop_graskuil + kosten_vers_gras)
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(saldo_verkoop_overschot_gras = ifelse(mutatie_graskuil > 0, mutatie_graskuil * (prijs_verkoop_graskuil - (kosten_gras_totaal_kg_ds + kosten_oogst_graskuil)) , 0))

  #Kosten maisproducten in rantsoen
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_eigen_maiskuil = ifelse(mutatie_maiskuil > 0, (opb_mais_totaal_ds - mutatie_graskuil) * kosten_mais_totaal_kg_ds, opb_mais_totaal_ds * kosten_mais_totaal_kg_ds ))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_aankoop_maiskuil = ifelse(mutatie_maiskuil < 0, abs(mutatie_maiskuil) * prijs_aankoop_maiskuil, 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_mais_rantsoen = kosten_eigen_maiskuil + kosten_aankoop_maiskuil)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_mais_rantsoen = ifelse(is.na(kosten_mais_rantsoen), 0, kosten_mais_rantsoen))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(saldo_verkoop_overschot_mais = ifelse(mutatie_maiskuil > 0, mutatie_maiskuil * (prijs_verkoop_maiskuil - kosten_mais_totaal_kg_ds), 0))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_overig_ruwvoer = (ov_verbruik_kuil * prijs_overig_ruwvoer_ds))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_geh_vem_product = kv_geh_vem * ds_gehalte_krachtvoer)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_geh_re_product = kv_geh_re * ds_gehalte_krachtvoer)
  
  
  
  if(bijproducten_algemeen){
    
    #Kosten bijproducten
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_bijproducten_ton_ds = ((ov_geh_vem * prijs_kvem_bijproduct + ov_geh_re * correctie_re_dve_bijproduct* prijs_dve_bijproduct)/100) * procent_voederwaarde_prijs_bijproduct )
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_bijproducten_totaal = (kosten_bijproducten_ton_ds * ov_verbruik_kuil / 1000))
    
    #Kosten krachtvoer
    
    #HOE STAAT BIJ ANDERE JAREN HET GEHALTE KRACHTVOER?
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_verbruik_kuil_kg_product = kv_verbruik_kuil / ds_gehalte_krachtvoer)
    #dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_verbruik_kuil_kg_product = kv_verbruik_kuil / 1)
    
    #Bedrijfsspecifieke kv prijs is gebaseerd op ds.
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_prijs_bedrijf = ((((kv_geh_vem_product/1000 * prijs_kvem_krachtvoer) + (((kv_geh_re_product * correctie_re_dve_krachtvoer) / 1000) * prijs_dve_krachtvoer)) * correctiefactor_krachtvoer * procent_bedrijfsspecifiek_krachtvoer) + (norm_prijs_krachtvoer * (1-procent_bedrijfsspecifiek_krachtvoer))) + toeslag_mineralen)
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_kosten_totaal = (kv_verbruik_kuil_kg_product * kv_prijs_bedrijf / 100))
  
    #Kosten melkpoeder  
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mp_verbruik_kuil_kg_product = mp_verbruik_kuil / ds_gehalte_melkpoeder)
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mp_kosten_totaal = (mp_verbruik_kuil_kg_product * prijs_aankoop_melkpoeder))
    
  } else {
    
    dataset_xml =  XMLtoDataFrame(path_xml_files)
    
    #Zorg dat numerieke kolommen als numeriek worden opgeslagen!
    is_all_numeric <- function(x) {
      !any(is.na(suppressWarnings(as.numeric(na.omit(
        x
      ))))) & is.character(x)
    }
    
    dataset_xml = dataset_xml %>% dplyr::mutate_if(is_all_numeric, as.numeric)
    
    #Samenvoegen met grote dataset:
    dataset_voerwinst = inner_join(dataset_voerwinst, dataset_xml, by = c("jaartal", "kvk_nummer"))
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verbruik_Overigrvbp_hoog = ifelse(verbruik_Overigrvbp_hoog < 1, NA, verbruik_Overigrvbp_hoog))
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verbruik_Overigrvbp_laag = ifelse(verbruik_Overigrvbp_laag < 1, NA, verbruik_Overigrvbp_laag))
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verbruik_Krachtvoer_hoog = ifelse(verbruik_Krachtvoer_hoog < 1, NA, verbruik_Krachtvoer_hoog))
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(verbruik_Krachtvoer_laag = ifelse(verbruik_Krachtvoer_laag < 1, NA, verbruik_Krachtvoer_laag))
    
    #HOOG VEM BIJPRODUCTE
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_bijproducten_hoog_vem_totaal = 
                                                              ifelse(is.na(verbruik_Overigrvbp_hoog),0, verbruik_Overigrvbp_hoog / 1000 * (verbruik_re_Overigrvbp_hoog * correctie_re_dve_bijproduct * prijs_dve_bijproduct + verbruik_vem_Overigrvbp_hoog * prijs_kvem_bijproduct) / 100 * procent_voederwaarde_prijs_bijproduct))
    #LAAG VEM BIJPRODUCTEN
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_bijproducten_laag_vem_totaal = 
                                                              ifelse(is.na(verbruik_Overigrvbp_laag),0,verbruik_Overigrvbp_laag * prijs_overig_ruwvoer_ds))
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_bijproducten_totaal = kosten_bijproducten_hoog_vem_totaal + kosten_bijproducten_laag_vem_totaal )
    
    #HOOG VEM KRACHTVOER
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_krachtvoer_hoog_vem_totaal = 
                                                              ifelse(is.na(verbruik_Krachtvoer_hoog), 0, verbruik_Krachtvoer_hoog / 1000 * (verbruik_re_Krachtvoer_hoog * correctie_re_dve_krachtvoer * prijs_dve_krachtvoer + verbruik_vem_Krachtvoer_hoog * prijs_kvem_krachtvoer) / 100 * correctiefactor_krachtvoer))
    
    #LAAG VEM KRACHTVOER --> MINERALEN
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_krachtvoer_laag_vem_totaal = 
                                                              ifelse(is.na(verbruik_Krachtvoer_laag), 0, verbruik_Krachtvoer_laag * prijs_mineralen)) #verbruik_Krachtvoer_laag * ds_gehalte_ * prijs_overig_ruwvoer_ds)
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kv_kosten_totaal = kosten_krachtvoer_hoog_vem_totaal + kosten_krachtvoer_laag_vem_totaal )
    
    dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(mp_kosten_totaal = 0 )
    
  }
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_rantsoen = kosten_gras_rantsoen + kosten_mais_rantsoen + kosten_overig_ruwvoer +  kosten_bijproducten_totaal + kv_kosten_totaal + mp_kosten_totaal)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(kosten_rantsoen_100kgmelk = kosten_rantsoen / melk_bedr * 100)
  
  #Berekening Voerwinst
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(totaal_opbrengsten = melkgeld + verkoop_dieren_waarde)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(totaal_kosten = kosten_rantsoen + aankoop_dieren_waarde)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(voerwinst_totaal = totaal_opbrengsten - totaal_kosten)
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(voerwinst_ha = round(voerwinst_totaal/opp_totaal , 0))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(voerwinst_100kgmelk = round(voerwinst_totaal / melk_bedr * 100, 2))
  
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(saldo_ruwvoer_verkoop = saldo_verkoop_overschot_gras + saldo_verkoop_overschot_mais)

  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(jaartal_factor = as.factor(jaartal))
  
  if(FALSE){
  #Welke bedrijven zijn voor 3 jaar vertegenwoordigd?
  
  dataset_voerwinst_3jaren = pull(dataset_voerwinst %>% 
    dplyr::group_by(Lidmaatschapsnummer) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::filter(count == 3) %>%
    dplyr::select(Lidmaatschapsnummer))
  
  dataset_voerwinst_gem = dataset_voerwinst %>%
    dplyr::filter(Lidmaatschapsnummer %in% dataset_voerwinst_3jaren) %>%
    dplyr::group_by(Lidmaatschapsnummer) %>%
    dplyr::summarise_all(mean, na.rm = T)
  
  dataset_voerwinst_gem = dataset_voerwinst_gem %>% dplyr::mutate(beweiding = ifelse(urenweidenmelkkoeien > 0, "Met weidegang", "Zonder weidegang"))
  dataset_voerwinst = dataset_voerwinst %>% dplyr::mutate(beweiding = ifelse(urenweidenmelkkoeien > 0, "Met weidegang", "Zonder weidegang"))
  
  
  dataset_voerwinst_gem$jaartal_factor = "Gemiddelde 2018-2020"
  
  dataset_voerwinst_alles = rbind(dataset_voerwinst, dataset_voerwinst_gem)
  
  #Analyse
  library(broom)
  
  fitted_models = dataset_voerwinst_gem %>% dplyr::group_by(beweiding) %>% do(model = lm(voerwinst_ha ~ melkperha, data = .))
  print(fitted_models)
  
  #Uitschakelen intensiteitseffect
  if(FALSE){
  lm.model = lm(voerwinst_ha ~ melkperha, data = dataset_voerwinst_gem %>% dplyr::filter(urenweidenmelkkoeien > 0))
  predicted_df_beweiding = data.frame(regressielijn = predict(lm.model, dataset_voerwinst_gem), melkperha =   dataset_voerwinst_gem$melkperha)
  print(summary(lm.model))
  
  lm.model = lm(voerwinst_ha ~ melkperha, data = dataset_voerwinst_gem %>% dplyr::filter(urenweidenmelkkoeien < 1))
  predicted_df_geen_beweiding = data.frame(regressielijn = predict(lm.model, dataset_voerwinst_gem), melkperha =   dataset_voerwinst_gem$melkperha)
  print(summary(lm.model))
  }
  }

  if(produce_plots){
    
    #Kleuren van VKA
    kleur_vka_rood = rgb(167, 25, 48, maxColorValue = 255)
    kleur_vka_groen = rgb(0, 102, 67, maxColorValue = 255)
    
  library(ggplot2)
  library(ggforce)
    
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = melkperha, y = voerwinst_ha, color = beweiding)) +
    theme_bw() +
    geom_point(alpha = 0.6) +
    #geom_smooth(method = "lm", se = FALSE)  +
    geom_abline(intercept = fitted_models$model[[1]]$coefficients[1], slope = fitted_models$model[[1]]$coefficients[2], color = kleur_vka_groen, size = 1) +
    geom_abline(intercept = fitted_models$model[[2]]$coefficients[1], slope = fitted_models$model[[2]]$coefficients[2], color = kleur_vka_rood, size = 1) +
    theme(legend.title = element_blank()) +
    xlab("Intensiteit [kg melk / ha]") +
    ylab("Voerwinst [euro / ha]") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6), limits = c(10000,30000)) +
    
    scale_color_manual(values = c(kleur_vka_groen, kleur_vka_rood)) +
    theme(legend.position = "top")
  
    #geom_line(color = "red", data = predicted_df_beweiding, aes(x = melkperha, y = regressielijn)) +
    #geom_line(color = "orange", data = predicted_df_geen_beweiding, aes(x = melkperha, y = regressielijn))
  print(plot)
  ggsave("melkperha_voerwinstha_opstallers.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(y = voerwinst_100kgmelk, x = melkperha, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Intensiteit [kg melk / ha]") +
    ylab("Voerwinst [euro / 100 kg melk]") +
    xlim(8000,35000) +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
    
    
  print(plot)
  ggsave( "intensiteit_vs_voerwinst.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = pceigen_n, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk]") +
    ylab("Aandeel eiwit van eigen land [%]") +
    xlim(10,30) +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_eiwit_eigen_land.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_ha, y = pceigen_n, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / ha]") +
    ylab("Aandeel eiwit van eigen land [%]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_ha_eiwit_eigen_land.png", width = 20, height = 12, units = "cm")
  
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_ha, y = verl_bodbal1_ha, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("Stikstofbodemoverschot [kg N / ha]") +
    xlab("Voerwinst [euro / ha]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_ha_n_overschot.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = kvper100kgmelk, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("Krachtvoerverbruik [kg / 100 kg melk]") +
    xlab("Voerwinst [euro / 100 kg melk]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_kvverbruik.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_ha, y = rants_geh_re, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("RE-gehalte rantsoen [g/kg ds]") +
    xlab("Voerwinst [euro / ha]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_rantsoenre.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = dzh_co2_melkprod, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("BKG emissies [g CO2-eq/kg FPCM]") +
    xlab("Voerwinst [euro / 100 kg melk]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6), limits = c(900,1300)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_bkg.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = em_nh3_tonmelk, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("NH3 emissie [kg / 1000 kg melk]") +
    xlab("Voerwinst [euro / 100 kg melk]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_ammoniak.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_ha, y = jvper10mk, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("Jongveebzetting [jv / 10]") +
    xlab("Voerwinst [euro / 100 kg melk]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_ha_jvper10mk.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_ha, y = opb_graspr_ds, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab("Opbrengst productiegras [kg ds]") +
    xlab("Voerwinst [euro / 100 kg melk]") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_ha_opb_graspr_ds.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = efficientie_N, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk") +
    ylab("Stikstofefficientie veestapel [%]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_efficientieN.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = voereff_melk, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk") +
    ylab("Voerefficientie veestapel [kg melk / kg ds]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_voerefficientie.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = rants_re_kvem, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk") +
    ylab("RE/kVEM verhouding rantsoen [-]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_rants_re_kvem.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = pceigen_n_buurt, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk") +
    ylab("Eiwit eigen land + buurtaankoop [%]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_pceigen_n_buurt.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst_gem, aes(x = voerwinst_100kgmelk, y = benut_n_bod, color = as.factor(jaartal_factor)))+
    theme_bw() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Voerwinst [euro / 100 kg melk") +
    ylab("Bodembenutting stikstof [%]")  +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
    scale_color_manual(values = c(kleur_vka_rood))
  print(plot)
  ggsave( "voerwinst_100kgmelk_benut_n_bod.png", width = 20, height = 12, units = "cm")
  
  
  plot = ggplot(data = dataset_voerwinst, aes(x = as.factor(jaartal), y = voerwinst_100kgmelk)) +
    theme_bw() +
    geom_violin(aes(fill = as.factor(jaartal))) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    ylab("Voerwinst [euro / 100 kg melk]") +
    xlab("Jaartal") +
    stat_summary( fun = mean, 
                  geom = "point",
                  size = 4,
                  color = "black",
                  na.rm = T
    ) +
    stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,1), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
  print(plot)
  ggsave( "voerwinst_100kgmelk_verdelig.png", width = 20, height = 12, units = "cm")
  
  plot = ggplot(data = dataset_voerwinst, aes(x = as.factor(jaartal), y = voerwinst_ha)) +
    theme_bw() +
    geom_violin(aes(fill = as.factor(jaartal))) +
    geom_sina(color = "lightgrey", size = 0.5) +
    theme(legend.position = "none") +
    ylab("Voerwinst [euro / 100 kg melk]") +
    xlab("Jaartal") +
    stat_summary( fun = mean, 
                  geom = "point",
                  size = 4,
                  color = "black",
                  na.rm = T
    ) +
    stat_summary (fun = mean, geom = "text", aes(label = format(round(..y..,0), big.mark = ".", scientific = FALSE)), size = 5, color = "black", vjust = -1 )
  print(plot)
  ggsave( "voerwinst_ha_verdelig.png", width = 20, height = 12, units = "cm")
  
  write.xlsx(dataset_voerwinst_alles, paste("data_voerwinst_",output_file_string,".xlsx",sep=""), asTable = T, overwrite = T)
  write.xlsx(dataset_xml, "data_input_xml.xlsx", asTable = T, overwrite = T)
  }
  
  return(as.data.frame(dataset_voerwinst))
  
}

#Voor testen:
if(FALSE){
path_xml_files = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst/Input_XML_2018_2020"
path_dataset = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Dataset_VKA_2018_2020"
output_folder = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/Rapportage_VKA_2020/Voerwinst"

output = berekenVoerwinst(path_dataset = path_dataset, path_xml_files = path_xml_files, output_folder = output_folder, bijproducten_algemeen = F, produce_plots = T)

}

if(FALSE){
output_A = output

data_merge = inner_join(resultaat_algemeen, resultaat_xml, by = c("jaartal", "kvk_nummer"), suffix = c(".XML",".EXC"))

plot = ggplot(data = data_merge, aes(x = voerwinst_ha.XML, y = voerwinst_ha.EXC))+
  theme_bw() +
  geom_point( color = kleur_vka_groen, alpha = 0.6) +
  xlab("Voerwinst INPUT XML [euro / ha] ") +
  ylab("Voerwinst EXC [euro / ha] ") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE), breaks = pretty_breaks(n = 6)) +

  geom_abline(intercept =  0, slope = 1, color = kleur_vka_rood, size = 1)
print(plot)
ggsave("Vergelijking_XML_EXC.png", width = 20, height = 12, units = "cm")

data_merge$diff = (data_merge$voerwinst_ha.XML - data_merge$voerwinst_ha.EXC) 
sqrt(mean(data_merge$diff^2))
mean((data_merge$diff / data_merge$voerwinst_ha.EXC * 100))

cor(data_merge$voerwinst_ha.EXC, data_merge$voerwinst_ha.XML, method = c( "spearman"))
}
