setwd('C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/Samenwerking MV-AK/Huidige situatie')

##### Data inlezen #####
regios <- read.csv('Gebieden_in_Nederland_2019_10082021_130901.csv',sep=";",header=F,skip=5)
colnames(regios)<-c('gem','LB_code','LB_naam','LG_code','LG_naam','LD_code','LD_naam','PV_code','PV_naam')

landbouwdata_cbs <- read.csv('Landbouw__gemeente_10082021_142626.csv',sep=";")

# Selecteren Achterhoekse gemeenten
gem_achterhoek <- regios$gem[regios$LB_code=='LB2508']
variabelen_opp <- colnames(landbouwdata_cbs)[grepl("Oppervlakte",colnames(landbouwdata_cbs))]
variabelen_aantal <- colnames(landbouwdata_cbs)[grepl("Aantal",colnames(landbouwdata_cbs))]

# Subset met informatie over veehouderij en akkerbouw bedrijven in de Achterhoek
landbouwdata_subset <- landbouwdata_cbs[landbouwdata_cbs$Regio.s %in% c("Nederland","Achterhoek (LB)",gem_achterhoek),
                                        c("?..Perioden", "Regio.s",variabelen_aantal,variabelen_opp)]
##### Adjust dataset #####
# opp. in ha
landbouwdata_subset[,colnames(landbouwdata_subset)%in%variabelen_opp] <- 
  landbouwdata_subset[,colnames(landbouwdata_subset)%in%variabelen_opp]/100

# nieuwe variabelen
landbouwdata_subset$Akkerbouw.Oppervlakte.are <- landbouwdata_subset$Akkerbouw.Oppervlakte.Akkerbouwgroenten..are. +
  (landbouwdata_subset$Tuinbouw.onder.glas.Oppervlakte.Fruit.onder.glas..m2./100) +
  (landbouwdata_subset$Tuinbouw.onder.glas.Oppervlakte.Glasgroenten..m2./100) +
  (landbouwdata_subset$Tuinbouw.overig.Oppervlakte..hoeveelheid.Paddenstoelenteelt.Champignons..m2./100) +
  landbouwdata_subset$Tuinbouw.open.grond.Oppervlakte.Bloembollen.en..knollen..are. + 
  landbouwdata_subset$Tuinbouw.open.grond.Oppervlakte.Tuinbouwgroenten..are.

variabelen_opp2 <- c("Grondgebruik.Oppervlakte.Cultuurgrond..are.","Akkerbouw.Oppervlakte.are",
                     "Akkerbouw.Oppervlakte.Granen..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Blijvend.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Natuurlijk.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Tijdelijk.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Groenvoedergewassen..are.")

##### Check data #####
# Check of som van aantal bedrijven in de gemeenten = totaal aantal bedrijven in de achterhoek
sum(landbouwdata_subset[landbouwdata_subset$?..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s %in% gem_achterhoek,3])

landbouwdata_subset[landbouwdata_subset$?..Perioden == 2020 & 
                      landbouwdata_subset$Regio.s == "Achterhoek (LB)",3]

# Check som van opp
sum(landbouwdata_subset[landbouwdata_subset$?..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s == "Nederland",
                        colnames(landbouwdata_subset) %in% variabelen_opp2[2:7]])

sum(landbouwdata_subset[landbouwdata_subset$?..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s == "Nederland",
                        colnames(landbouwdata_subset) %in% variabelen_opp2[1]])

##### Write data #####
# CSV file met aantal data
write.csv(t(landbouwdata_subset[landbouwdata_subset$Regio.s %in% c("Nederland","Achterhoek (LB)"),
                                c("?..Perioden", "Regio.s",variabelen_aantal)]),
          file="Landbouw_gemeente_subset_aantalAchterhoek.csv")

# CSV file met opp. data
write.csv(t(landbouwdata_subset[landbouwdata_subset$Regio.s %in% c("Nederland","Achterhoek (LB)"),
                                c("?..Perioden", "Regio.s",variabelen_opp2)]),
          file="Landbouw_gemeente_subset_oppAchterhoek.csv")



#####
rm(list=ls())

##### Analyse VKA data ####

path_to_dataset = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database/dataset_VKA_2013_2020.xlsx"
output_path = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/Samenwerking MV-AK/VKA data"

#Inlezen van dataset
dataset <- read_excel(path_to_dataset)

#Set working directory
if(file.exists(output_path)){
  setwd(output_path)
} else {
  warning("Path naar voor output bestaat niet!")
}

# Parameters
verliezen_veld_gras = 0.95
verliezen_veld_mais = 0.98

verliezen_cons_gras = 0.96
verliezen_cons_mais = 0.96

#Hoe veel ruwvoer wordt er geoogst?
dataset$oogst_gras = dataset$opb_graspr_ds * dataset$opp_prgras * verliezen_veld_gras
dataset$oogst_mais = dataset$opb_mais_ds * dataset$opp_mais * verliezen_veld_mais
dataset$oogst_ruwvoer = dataset$oogst_gras + dataset$oogst_mais

#Hoeveel ruwvoer wordt er verbruikt?
dataset$verbruik_ruwvoer = dataset$gk_verbruik + dataset$sm_verbruik

#Hoeveel ruwvoer wordt er aangekocht?
dataset$aankoop_ruwvoer = dataset$aankoop_aanleg_gk_hoev * verliezen_cons_gras + dataset$aankoop_aanleg_sm_hoev * verliezen_cons_mais

#Hoeveel ruwvoer wordt er afgevoerd?
dataset$afvoer_gras = dataset$afv_gkp1_hoev + dataset$afv_gkp2_hoev + dataset$afv_gkp3_hoev + dataset$afv_gkp4_hoev
dataset$afvoer_mais = dataset$afv_gkp1_hoev + dataset$afv_sm2_hoev + dataset$afv_sm3_hoev + dataset$afv_sm4_hoev

#Hoeveel ruwvoer wordt er aangelegd
dataset$aanleg_ruwvoer = dataset$oogst_ruwvoer + dataset$aankoop_ruwvoer - dataset$afvoer_gras - dataset$afvoer_mais
dataset$aanleg_ruwvoer_2 = dataset$aanleg_gk_hoev + dataset$aanleg_sm_hoev

dataset$aandeel_oogst_gras_gebruik = (dataset$gk_verbruik / 0.95) / dataset$opb_graspr_ds #waar komt 0.95 vandaan?
dataset$aandeel_oogst_mais_gebruik = (dataset$sm_verbruik / 0.95) / dataset$opb_mais_ds

# Want de ruwvoeropbrengst wordt in de KLW uit de voeding berekend en niet via de meting van de graskuil. Daar zit een denkfout, dus de kans is heel klein dat die kengetallen naadloos op elkaar aansluiten.
# 


#dataselectie
columns = c(
  "Achternaam",
  "jaartal",
  "nkoe",
  "nkalf", #<1 jaar
  "npink", #>1 jaar
  "jvper10mk",
  "noverig", #overige graasdieren
  "nintensief",#varkens, pluimvee
  "gve_melkvee",
  
  "melkprod",
  "melkperha",
  "opp_totaal", #totaal opp bedrijf
  "opp_prgras", #opp productie grasland
  "opp_natuur", #opp natuurgrasland
  "opp_mais", #opp snijmaisland
  "opp_overig", #opp overig bouwland (geen gras- of mais)
  
#  "kvperbedrijf", #opname krachtvoer totaal
#  "kvper100kgmelk",
#  "bpperbedrijf", #opname bijproducten totaal
  
  "rants_geh_re",
  "rants_verbruik", #totaal voeropname veestapel (melk- en jongvee)
  "gr_verbruik", #vers gras voeropname veestapel
  "gk_verbruik", #graskuil voeropname veestapel
  "sm_verbruik", #snijmais voeropname veestapel
  "ov_verbruik", #overig ruwvoer en bijproducten voeropname veestapel
  "kv_verbruik", #krachtvoer voeropname veestapel
  
  "gr_aandeel", #% in totale rantsoen
  "gk_aandeel",
  "sm_aandeel",
  "ov_aandeel",
  "kv_aandeel",
  
  "gk_bv_hoev", #graskuil beginvoorraad
  "gk_ev_hoev", #graskuil eindvoorraad
  "aanleg_gk_hoev", #aanleg graskuil
  "aankoop_aanleg_gk_hoev", #aankoop graskuil
  "afv_gkn1_hoev", #Afvoer uit beginvoorraad
  "afv_gkn2_hoev", #Afvoer uit voorraad aanleg
  "afv_gkn3_hoev", #Afvoer niet bex ingekuild
  "afv_gkn4_hoev", #Afvoer niet bex vers gras
  "gk_mut_hoev", #mutatie graskuil

  "sm_bv_hoev", #snijmais beginvoorraad
  "sm_ev_hoev", #snijmais eindvoorraad
  "aanleg_sm_hoev", #aanleg snijmais
  "aankoop_aanleg_sm_hoev", #aankoop snijmais
  "afv_sm1_hoev", #Afvoer uit beginvoorraad
  "afv_sm2_hoev", #Afvoer uit voorraad aanleg
  "afv_sm3_hoev", #Afvoer niet bex ingekuild
  "afv_sm4_hoev", #Afvoer niet bex vers gras
  "sm_mut_hoev", #mutatie snijmais
  
  "ov_bv_hoev", #overige gewassen beginvoorraad
  "ov_ev_hoev", #overige gewassen eindvoorraad
  "aanleg_ov_hoev", #aanleg overige gewassen
  "aankoop_aanleg_ov_hoev", #aankoop overige gewassen
  "afv_ov1_hoev", #Afvoer uit beginvoorraad
  "afv_ov2_hoev", #Afvoer uit voorraad aanleg
  "afv_ov3_hoev", #Afvoer akkerbouwgewassen niet bex
  "afv_ov4_hoev", #Afvoer overige gewassen niet bex
  "ov_mut_hoev", #mutatie
  
  "bv_kv_hoev", #beginvoorraad krachtvoer
  "ev_kv_hoev", 
  "kv_mut_hoev",
  "aankoop_aanleg_kv_hoev",
  "aanleg_kv_hoev",
  
  "kmaan_kg", #aanvoer kunstmest
  "kmafv_kg", #afvoer kunstmest
  
  #percentage eigen geteeld N vs. voerverbruik N en dat vergelijken met aandeel eigen N rantsoen
  #% eigen teelt > eigen teelt in rantsoen: veel ruwvoer & veel aankoop krachtvoer

  "eiwiteig_tlt_vg", #eigen teelt N: vers gras (kg N)
  "eiwiteig_tlt_gk", #eigen teelt N: grasproducten
  "eiwiteig_tlt_sm", #eigen teelt N: snijmais
  "eiwiteig_tlt_ov", #eigen teelt N: akkerbouw veevoer

  "eiwiteig_vbr_vg", #verbruik N: vers gras
  "eiwiteig_vbr_gk", #verbruik N: grasproducten
  "eiwiteig_vbr_sm", #verbruik N: snijmais
  "eiwiteig_vbr_ov", #verbruik N: overige ruwvoer en natte bijproducten
  "eiwiteig_vbr_kv", #verbruik N: krachtvoeders

  "rants_geh_n", #N-gehalte rantsoen, totaal (g/kg ds)
  "rants_verbruik", #totaal rantsoen (kg ds)
  "gr_geh_n", #N-gehalte rantsoen, vers gras
  "gk_geh_n", #N-gehalte rantsoen, graskuil
  "sm_geh_n", #N-gehalte rantsoen, snijmais
  "ov_geh_n", #N-gehalte rantsoen, overige ruwvoer en bijproducten
  "kv_geh_n", #N-gehalte rantsoen, krachtvoer

  "pceigen_n", #Voerverbruik, aandeel eigen voer, stikstof
  "pceigen_n_buurt",
  "akvoer_n" #Aankoop voer per ton melk, stikstof
  
)

if(all(columns %in% colnames(dataset))){
  dataset_samenwerking = dataset[, columns]
} else {
  stop("Input dataset heeft niet de juiste headers")
}


dataset_samenwerking$eiwiteig_tlt_totaal <- dataset_samenwerking$eiwiteig_tlt_vg + dataset_samenwerking$eiwiteig_tlt_gk + 
  dataset_samenwerking$eiwiteig_tlt_sm + dataset_samenwerking$eiwiteig_tlt_ov

dataset_samenwerking$eiwiteig_vbr_totaal <- dataset_samenwerking$eiwiteig_vbr_vg + dataset_samenwerking$eiwiteig_vbr_gk + 
  dataset_samenwerking$eiwiteig_vbr_sm + dataset_samenwerking$eiwiteig_vbr_ov + dataset_samenwerking$eiwiteig_vbr_kv

dataset_samenwerking$eiwiteig_pc_tlt_vbr <- dataset_samenwerking$eiwiteig_tlt_totaal/dataset_samenwerking$eiwiteig_vbr_totaal * 100

dataset_samenwerking$rants_kgn <- dataset_samenwerking$rants_geh_n * dataset_samenwerking$rants_verbruik / 1000

dataset_samenwerking$verschil_pceigen_n_rantsoen <- dataset_samenwerking$pceigen_n - dataset_samenwerking$rants_geh_n

#percentage eigen geteeld N vs. voerverbruik N (=pceigen_n) en dat vergelijken met aandeel eigen N rantsoen (=rants_geh_n)
#% eigen teelt > eigen teelt in rantsoen: veel ruwvoer & veel aankoop krachtvoer
kolommen_test <- c(
"Achternaam","jaartal",
"eiwiteig_pc_tlt_vbr",
"pceigen_n", #Voerverbruik, aandeel eigen voer, stikstof
"rants_geh_n", #N-gehalte rantsoen, totaal
"rants_verbruik",
"rants_kgn",
"verschil_pceigen_n_rantsoen"
)

summary(dataset_samenwerking[,colnames(dataset_samenwerking)%in%kolommen_test])
View(dataset_samenwerking[dataset_samenwerking$pceigen_n > 100,])
