#Jur Eekelder; 23-08-2021
#jur.eekelder@demarke.eu
#Script voor het toeoegen van extra kengetaleln aan KLW databases.

#INPUTS
#database_klw --> dataframe met KLW kengetaleln

#TOELICHTING
#Veel kengetallen hebben niet op iedere boer betrekking, en kunnen zo verkeerde resultaten opleveren als het gemiddelde wordt bepaald.
#Voorbeeld: niet iedere boer heeft vers gras in het rantsoen. Het kengetal "gr_aandeel" zal dan 0 zijn.
#Als dan het gemiddelde "gr_aandeel" over de hele dataset wordt berekend zitten daar eigenlijk 2 groepen in, de groep die wél vers gras voert, 
#en de groep die geen vers gras voert. Het gemiddelde kan dan een vertekend beeld geven.

#In dit script worden voor dergelijke kengetallen wat extra kengetallen toegevoegd:
#<kengetal>NA --> hier wordt er een kengetal toegevoegd waarbij de waarde "0" (of een andere arbitrair getal) wordt gebruikt als drempelwaarde. 
#Bij bijvoorbeeld vers gras, wordt het kengetal gr_aandeelNA gelijk gestelt aan "gr_aandeel" als de boer vers gras voert, en anders ("gr_aandeel" = 0) wordt het NA. 
#Waarom? Dan kan er makkelijk met functies worden gewerkt zoals mean(., na.rm = T). Zo kan snel het gemiddelde worden berekend van de groep die er écht toe doet.

#Daarbij is het interessant om te weten wat het aandeel [%] is van bedrijven dat bijvoorbeeld vers gras voert. Daarvoor wordt het volgend kengetal toegevoegd:
#<kengetal>boolean --> dit getal wordt simpelweg "1" of "0". Als een bedrijf bijvoorbeeld vers gras voert wordt het een "1", en anders een "0".
#Door al deze enen en nullen te middelen geeft dat het aandeel van de bedrijven dat het wél of niet toepast.


toevoegenKengetallenKLW <- function(database_klw){
  
  #Voor het sommeren van kolommen, hoe gaan we om met kolommen die ook NA kunnen bevatten?
  addColumns <- function(data, y){
    
    index_all_NA = apply(data[,y], 1, function(z) all(is.na(z)))
    
    result = rowSums(data[,y], na.rm = TRUE)
    
    result[index_all_NA] = NA
    
    return(result)
    
  }
  
  #Afkorten
  data = database_klw
  
  #JONGVEE
  data$njongvee = data$npink + data$nkalf
  data$njvtienmk = round(10 * data$njongvee / data$nkoe , 1)
  data$njvtienmkdrie = data$njvtienmk
  data$njvtienmkdrieboolean = data$njvtienmk
  data$njvtienmkdrieboolean[which(data$njvtienmk<3)] = 0 
  data$njvtienmkdrieboolean[which(data$njvtienmk>=3)] = 1
  data$njvtienmkdrie[which(data$njvtienmk<3)] = NA
  
  #BEX
  data$BEX_N_kg_koe = data$excr_spec1 / data$nkoe
  data$BEX_P_kg_koe = data$excr_spec2 / data$nkoe
  
  #N/P verhoudingen
  data$NP_BEX = data$excr_spec1 / data$excr_spec2
  data$NP_forfaitair = data$excr_forf1 / data$excr_forf2
  
  #BES
  data$N_generiek = data$gebrnorm1 / data$opp_totaal
  data$P_generiek = data$gebrnorm2 / data$opp_totaal
  data$BES_N_3jaar = data$fosfaatnorm_haspecifiek3 * data$NP_BEX #BEP-gemiddelde van 3 jaar vermenigvuldigd met N/P verhouding
  data$BES_P_3jaar = data$fosfaatnorm_haspecifiek3
  
  #BEDRIJFSKENMERKEN
  data$aandeel_N_behoefte = data$oppgras / data$opp_totaal * 100
  data$fpcmkoejaar = data$melkpkoe * (0.337 + 0.116*data$vet + 0.06 *data$eiwit)
  data$intensiteit = cut(data$melkperha, c(0, 12500,15000,17500,20000,22500,25000,90000), labels = c("<12500","12500-15000","15000-17500","17500-20000","20000-22500","22500-25000",">25000"))
  data$kvperkoe = data$melkpkoe/100 * data$kvper100kgmelk
  data$gveperha = data$gve_melkvee / data$opp_totaal
  data$fpcmperha = data$bkg_prod_fpcm / data$opp_totaal
  
  #OPPERVLAKTES
  data$opp_maisNA = ifelse(data$opp_mais > 0, data$opp_mais, NA)
  data$opp_maisboolean = ifelse(is.na(data$opp_maisNA),0,1)
  data$opp_natuurNA = ifelse(data$opp_natuur > 0, data$opp_natuur, NA)
  data$opp_natuurboolean = ifelse(is.na(data$opp_natuurNA),0,1)
  data$opp_overigNA = ifelse(data$opp_overig > 0, data$opp_overig, NA)
  data$opp_overigboolean = ifelse(is.na(data$opp_overigNA),0,1)
  
  #RANTSOEN
  data$ov_mp_aandeel = data$ov_aandeel + data$mp_aandeel
  
  data$gr_aandeelNA = ifelse(data$gr_aandeel > 0, data$gr_aandeel, NA)
  data$gr_aandeelboolean = ifelse(is.na(data$gr_aandeelNA),0,1)
  
  data$sm_aandeelNA = ifelse(data$sm_aandeel > 0, data$sm_aandeel, NA)
  data$sm_aandeelboolean = ifelse(is.na(data$sm_aandeelNA),0,1)
  
  data$ov_mp_aandeel_aandeelNA = ifelse(data$ov_mp_aandeel > 0, data$ov_mp_aandeel, NA)
  data$ov_mp_aandeelboolean = ifelse(is.na( data$ov_mp_aandeelNA),0,1)
  
  #AANKOOP EN AANDELEN RANTSOEN
  
  data$aankoop_aanleg_gk_hoevNA = ifelse(data$aankoop_aanleg_gk_hoev > 0 , data$aankoop_aanleg_gk_hoev, NA)
  data$aankoop_aanleg_gk_hoevboolean = ifelse(is.na(data$aankoop_aanleg_gk_hoevNA), 0,1)
  
  data$aankoop_aanleg_sm_hoevNA = ifelse(data$aankoop_aanleg_sm_hoev > 0 , data$aankoop_aanleg_sm_hoev, NA)
  data$aankoop_aanleg_sm_hoevboolean = ifelse(is.na(data$aankoop_aanleg_sm_hoevNA), 0,1)
  
  data$aankoop_aanleg_ov_hoevNA = ifelse(data$aankoop_aanleg_ov_hoev > 0 , data$aankoop_aanleg_ov_hoev, NA)
  data$aankoop_aanleg_ov_hoevboolean = ifelse(is.na(data$aankoop_aanleg_ov_hoevNA), 0,1)
  
  data$aankoop_aanleg_kv_hoevNA = ifelse(data$aankoop_aanleg_kv_hoev > 0 , data$aankoop_aanleg_kv_hoev, NA)
  data$aankoop_aanleg_kv_hoevboolean = ifelse(is.na(data$aankoop_aanleg_kv_hoevNA), 0,1)
  
  data$aankoop_aanleg_gk_hoev_rantsoen = ifelse(is.na(data$aankoop_aanleg_gk_hoevNA), NA, data$aankoop_aanleg_gk_hoev / data$gk_verbruik * 100)
  data$aankoop_aanleg_sm_hoev_rantsoen = ifelse(is.na(data$aankoop_aanleg_sm_hoevNA), NA, data$aankoop_aanleg_sm_hoev / data$sm_verbruik * 100)
  
  
  parameters_aankoop_voer = matrix(ncol = 3, byrow = T, data =  c(
    "aankoop_aanleg_gk_hoev", 0, "Aankoop graskuil [kg ds]",
    "aankoop_aanleg_sm_hoev", 0, "Aankoop snijmais [kg ds]",
    "aankoop_aanleg_ov_hoev", 0, "Aankoop overig ruwvoer en bijproducten [kg ds]",
    "aankoop_aanleg_kv_hoev", 0, "Aankoop krachtvoer [kg ds]",
    "akvoer_n", 0, "Voeraankoop stikstof [kg N / ton melk]",
    "akvoer_p", 0 , "Voeraankoop fosfor [kg P / ton melk]"
    
  ))
  
  #BEWEIDING
  data$dagenweidenmelkkoeien = data$dgnweidb + data$dgnweido + data$dgncombib + data$dgncombio
  data$dagenweidenmelkkoeienNA = ifelse(data$dagenweidenmelkkoeien < 1, NA, data$dagenweidenmelkkoeien)
  
  data$urenweidenmelkkoeien = data$dgnweidb * data$uurweidb + data$dgnweido * data$uurweido + data$dgncombib * data$uurcombib + data$dgncombio * data$uurcombio
  data$urenweidenmelkkoeienNA = ifelse(data$urenweidenmelkkoeien < 0.1, NA, data$urenweidenmelkkoeien)
  
  data$dgnweidpiNA = ifelse(data$dgnweidpi < 0.1, NA, data$dgnweidpi)
  
  data$urenweidenmelkkoeienperdag = data$urenweidenmelkkoeien / data$dagenweidenmelkkoeien
  data$urenweidenmelkkoeienperdag = ifelse(is.nan(data$urenweidenmelkkoeienperdag),0,data$urenweidenmelkkoeienperdag)
  data$urenweidenmelkkoeienperdagNA = ifelse(data$urenweidenmelkkoeienperdag < 0.1, 0, data$urenweidenmelkkoeienperdag)
  
  data$beweidenmelkkoeienboolean = ifelse(is.na(data$dagenweidenmelkkoeienNA), 0 ,1)
  
  #ZOMERSTALVOEREN
  data$zstvdagen = data$dgncombib + data$dgncombio + data$dgnzstvb + data$dgnzstvo
  data$zstvdagenNA = ifelse(data$zstvdagen < 1, NA, data$zstvdagen)
  data$zstvdagenboolean = ifelse(data$zstvdagen > 1, 1, 0)
  
  #EiIWIT VAN EIGEN LAND
  data$eiwiteigenlandcategorie = cut(data$pceigen_n, c(0, 45, 50, 55, 60, 65, 70, 75, 80, 200), labels = c("<45","45-50","50-55","55-60","60-65","65-70","70-75", "75-80", ">80"))
  data$aanvoer_N_overig_voer = (data$aankoop_aanleg_ov_hoev / 1000) * (data$aankoop_aanleg_ov_re / 6.25)
  data$aanvoer_N_ruwvoer_overig = (data$kring1_bedbal_aanrv * data$opp_totaal) 
  data$aanvoer_N_gras_mais = ifelse(data$aanvoer_N_overig_voer > data$aanvoer_N_ruwvoer_overig, 0 , 0.95 * (data$aanvoer_N_ruwvoer_overig - data$aanvoer_N_overig_voer))
  data$gevoerd_N = data$rants_verbruik * data$rants_geh_re / 6.25 / 1000
  data$aandeel_aanvoer_N_gras_mais = data$aanvoer_N_gras_mais / data$gevoerd_N * 100
  data$aandeel_N_eigen_aanvoer_mais_gras = data$pceigen_n + data$aandeel_aanvoer_N_gras_mais
  data$eiwiteigenlandcategoriebuurt = cut(100*data$aandeel_N_eigen_aanvoer_mais_gras, c(0, 45, 50, 55, 60, 65, 70, 75, 80, 200), labels = c("<45","45-50","50-55","55-60","60-65","65-70","70-75", "75-80", ">80"))
  data$pceigen_n_buurt = data$aandeel_N_eigen_aanvoer_mais_gras
  data$pceigen_n_buurt = data$pceigen_n_buurt
  data$pceigen_n = data$pceigen_n 
  data$pceigen_n_65_boolean = ifelse(data$pceigen_n > 64.9, 1, 0)
  data$pceigen_n_buurt_65_boolean = ifelse(data$pceigen_n_buurt > 64.9, 1, 0)
  
  #Stikstof en fosfaat overschotten
  factorNH3toN = (14/17)
  
  #AMMONIAK - DASHBOARD KLIMAAT
  data$dzh_nh3n_bedrha = round(data$dzh_nh3_bedrha * factorNH3toN, 1)
  data$dzh_nh3_bedrtot = (data$dzh_nh3_bedrha * data$opp_totaal)
  
  #AMMONIAK - KLW
  data$em_nh3_tonmelk_stalopslag = data$em_nh3_stal / data$melkprod * 1000
  data$em_nh3_tonmelk_drijfmest =  (data$em_nh3_ombouw	+ data$em_nh3_omgras) / data$melkprod * 1000
  data$em_nh3_tonmelk_kunstmest =  (data$em_nh3_kmbouw	+ data$em_nh3_kmgras) / data$melkprod * 1000
  data$em_nh3_tonmelk_beweid = data$em_nh3_beweid / data$melkprod * 1000

  
  
  #BEMESTINGEN
  data$graspr_bemest_dierlijk_N = addColumns(data, c("graspr_dmst_kgn","graspr_wmst_kgn"))
  data$graspr_totaal_kgn = addColumns(data, c("graspr_dmst_kgn", "graspr_wmst_kgn", "graspr_kmst_kgn"))
  data$graspr_totaal_kgp2o5 =  addColumns(data, c("graspr_dmst_kgp2o5",  "graspr_wmst_kgp2o5",  "graspr_kmst_kgp2o5"))
  
  data$mais_bemest_dierlijk_N = data$mais_dmst_kgn
  data$mais_totaal_kgn =  addColumns(data, c("mais_dmst_kgn" ,  "mais_kmst_kgn"))
  data$mais_totaal_kgp2o5 =  addColumns(data, c("mais_dmst_kgp2o5",  "mais_kmst_kgp2o5"))
  
  data$totaal_bemest_dierlijk_N =  addColumns(data, c("graspr_bemest_dierlijk_N",  "mais_bemest_dierlijk_N"))
  data$totaal_bemest_kunstmest_N =  addColumns(data, c("graspr_kmst_kgn",  "mais_kmst_kgn"))
  data$totaal_bemest_N =  addColumns(data, c("totaal_bemest_dierlijk_N",  "totaal_bemest_kunstmest_N"))
  
  #GEWASOPBRENGSTEN (1 kg N = 6,25 kg RE), (1 kg P2O5 = 1/(2.2915) kg P)
  data$opb_graspr_ds_per_N_bemest = data$opb_graspr_ds / data$graspr_tmst_kgn 
  data$opb_mais_ds_per_N_bemest = data$opb_mais_ds / data$mais_tmst_kgn
  
  data$opb_graspr_vem_g_kg = data$opb_graspr_kvem / data$opb_gras_ds * 1000 
  data$opb_graspr_re_g_kg = data$opb_graspr_n / data$opb_gras_ds * 1000 *6.25
  data$opb_graspr_p_g_kg = data$opb_graspr_p2o5 / data$opb_gras_ds * 1000 / 2.291
  
  data$opb_mais_vem_g_kg = data$opb_mais_kvem / data$opb_mais_ds * 1000 
  data$opb_mais_re_g_kg = data$opb_mais_n / data$opb_mais_ds * 1000 *6.25
  data$opb_mais_p_g_kg = data$opb_mais_p2o5 / data$opb_mais_ds * 1000 / 2.291
  
  
  #Landgebruik en grondsoort
  
  #Oppervlakte per gewas en bodemtype
  data$ha_gras_veen = data$bodemg_pcveen * data$oppgras / 100
  data$ha_gras_klei = data$bodemg_pcklei * data$oppgras / 100
  data$ha_gras_zand1 = data$bodemg_pczand1 * data$oppgras / 100
  data$ha_gras_zand2 = data$bodemg_pczand2 * data$oppgras / 100
  data$ha_gras_zand3 = data$bodemg_pczand3 * data$oppgras / 100
  data$ha_gras_zand = data$ha_gras_zand1 + data$ha_gras_zand2 + data$ha_gras_zand3
  
  data$ha_mais_veen = data$bodemm_pcveen * data$oppmais / 100
  data$ha_mais_klei = data$bodemm_pcklei * data$oppmais / 100
  data$ha_mais_zand1 = data$bodemm_pczand1 * data$oppmais / 100
  data$ha_mais_zand2 = data$bodemm_pczand2 * data$oppmais / 100
  data$ha_mais_zand3 = data$bodemm_pczand3 * data$oppmais / 100
  data$ha_mais_zand = data$ha_mais_zand1 + data$ha_mais_zand2 + data$ha_mais_zand3
  
  data$ha_akker_veen = data$bodema_pcveen * data$oppoverig / 100
  data$ha_akker_klei = data$bodema_pcklei * data$oppoverig / 100
  data$ha_akker_zand1 = data$bodema_pczand1 * data$oppoverig / 100
  data$ha_akker_zand2 = data$bodema_pczand2 * data$oppoverig / 100
  data$ha_akker_zand3 = data$bodema_pczand3 * data$oppoverig / 100
  data$ha_akker_zand = data$ha_akker_zand1 + data$ha_akker_zand2 + data$ha_akker_zand3
  
  data$aandeel_veen = round( (data$ha_gras_veen + data$ha_mais_veen + data$ha_akker_veen)/ data$opp_totaal * 100 , 1)
  data$aandeel_klei = round( (data$ha_gras_klei + data$ha_mais_klei + data$ha_akker_klei)/ data$opp_totaal * 100 , 1)
  data$aandeel_zand = round( (data$ha_gras_zand + data$ha_mais_zand + data$ha_akker_zand)/ data$opp_totaal * 100 , 1)
  
  data$aandeel_zand1 = round( (data$ha_gras_zand1 + data$ha_mais_zand1 + data$ha_akker_zand1)/ data$opp_totaal * 100 , 1)
  data$aandeel_zand2 = round( (data$ha_gras_zand2 + data$ha_mais_zand2 + data$ha_akker_zand2) / data$opp_totaal * 100 , 1)
  data$aandeel_zand3 = round( (data$ha_gras_zand3 + data$ha_mais_zand3 + data$ha_akker_zand3)/ data$opp_totaal * 100 , 1)
  
  
  data$grondsoort = "onbekend"
  data[data$aandeel_veen>50, "grondsoort"] = "veen"
  data[data$aandeel_zand>50, "grondsoort"] = "zand"
  data[data$aandeel_klei>50, "grondsoort"] = "klei"
  
  #Maximaal N-bodemoverschot
  
  data$max_N_bodemoverschot = (
    data$ha_gras_veen*334 +
      data$ha_gras_klei*296 +
      data$ha_gras_zand1*156 +
      data$ha_gras_zand2*105 +
      data$ha_gras_zand3*88 +
      
      data$ha_mais_veen*334 +
      data$ha_mais_klei*112 +
      data$ha_mais_zand1*106 +
      data$ha_mais_zand2*65 +
      data$ha_mais_zand3*51 +
      
      data$ha_akker_veen*334 +
      data$ha_akker_klei*112 +
      data$ha_akker_zand1*106 +
      data$ha_akker_zand2*65 +
      data$ha_akker_zand3*51 
  ) / data$opp_totaal
  
  data$bodemoverschot_N_minus_norm = data$verl_bodbal1_ha - data$max_N_bodemoverschot # verl_bodbal1_ha of  kring1_bodover
  data$bodemoverschot_N_minus_norm_boolean = ifelse(data$bodemoverschot_N_minus_norm < 0, 1, 0)
  
  #voor grasland:
  data$max_N_bodemoverschot_gras = (
    data$ha_gras_veen*334 +
      data$ha_gras_klei*296 +
      data$ha_gras_zand1*156 +
      data$ha_gras_zand2*105 +
      data$ha_gras_zand3*88 
    
  ) / (data$ha_gras_veen + data$ha_gras_klei + data$ha_gras_zand1 + data$ha_gras_zand2 + data$ha_gras_zand3)
  
  #voor maisland
  data$max_N_bodemoverschot_mais = (
    
    data$ha_mais_veen*334 +
      data$ha_mais_klei*112 +
      data$ha_mais_zand1*106 +
      data$ha_mais_zand2*65 +
      data$ha_mais_zand3*51 
    
  ) / (data$ha_mais_veen + data$ha_mais_klei + data$ha_mais_zand1 + data$ha_mais_zand2 + data$ha_mais_zand3)
  
  #Fosfaat bodembalans
  data$bodemoverschot_P_norm_boolean = ifelse(data$kring2_bodover<0,1,0)
  data$kring2_bodaan = data$kring2_bodaan_dm + data$kring2_bodaan_km + data$kring2_bodaan_wm
  
  #BEP
  data$BEP_voordeel_boolean = ifelse(data$fosfaatnorm_voordeel > 0, 1, 0)
  data$gebrnorm2_ha = data$gebrnorm2 / data$opp_totaal
  
  #ON THE WAY TO PLANETPROOF
  data$PP_beweiding = ifelse( (data$urenweidenmelkkoeien>719 & data$dagenweidenmelkkoeien > 119), 1, 0)
  data$PP_eiwit_van_eigen_land = ifelse( data$dzh_eiwit_pceig > 60, 2, ifelse(data$dzh_eiwit_pceig > 50, 1,0))
  data$PP_stikstof_bodemoverschot = ifelse( data$dzh_nbodem_over < 140, 2, ifelse(data$dzh_nbodem_over < 150, 1,0))
  data$PP_ammoniak = ifelse( data$dzh_nh3_bedrha < 75, 2, ifelse(data$dzh_nh3_bedrha < 80, 1,0))
  data$PP_blijvend_grasland = ifelse( data$dzh_blijgras_aand > 60, 2, ifelse(data$dzh_blijgras_aand > 40, 1,0))
  data$PP_broeikasgas = ifelse( data$dzh_co2_melkprod < 1100, 2, ifelse(data$dzh_co2_melkprod < 1200, 1,0))
  
  data$empty = ""
  
  return(data)
  
}