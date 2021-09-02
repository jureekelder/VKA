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

#Puinhoop van Jur:

verliezen_veld_gras = 0.95
verliezen_veld_mais = 0.98

verliezen_cons_gras = 0.96
verliezen_cons_mais = 0.96

#Hoe veel ruwvoer wordt er geoogst?
x$oogst_gras = x$opb_graspr_ds * x$opp_prgras * verliezen_veld_gras
x$oogst_mais = x$opb_mais_ds * x$opp_mais * verliezen_veld_mais
x$oogst_ruwvoer = x$oogst_gras + x$oogst_mais

#Hoeveel ruwvoer wordt er verbruikt?
x$verbruik_ruwvoer = x$gk_verbruik + x$sm_verbruik

#Hoeveel ruwvoer wordt er aangekocht?
x$aankoop_ruwvoer = x$aankoop_aanleg_gk_hoev * verliezen_cons_gras + x$aankoop_aanleg_sm_hoev * verliezen_cons_mais

x$afvoer_gras = x$afv_gkp1_hoev + x$afv_gkp2_hoev + x$afv_gkp3_hoev + x$afv_gkp4_hoev
x$afvoer_mais = x$afv_gkp1_hoev + x$afv_sm2_hoev + x$afv_sm3_hoev + x$afv_sm4_hoev

#Hoeveel ruwvoer wordt er aangelegd
x$aanleg_ruwvoer = x$oogst_ruwvoer + x$aankoop_ruwvoer - x$afvoer_gras - x$afvoer_mais
x$aanleg_ruwvoer_2 = x$aanleg_gk_hoev + x$aanleg_sm_hoev

x$aandeel_oogst_gras_gebruik = (x$gk_verbruik / 0.95) / x$opb_graspr_ds
x$aandeel_oogst_mais_gebruik = (x$sm_verbruik / 0.95) / x$opb_mais_ds



#dataselectie
columns = c(
  "Achternaam",
  "jaartal",
  "nkoe",
  "nkalf",
  "npink",
  "jvper10mk",
  "noverig",
  "gve_melkvee",
  
  "kvperbedrijf",
  "kvper100kgmelk",
  "bpperbedrijf",
  
  
  "rants_geh_re",
  "rants_verbruik",
  "gr_verbruik",
  "gk_verbruik",
  "sm_verbruik",
  "ov_verbruik",
  "kv_verbruik",
  
  "gr_aandeel",
  "gk_aandeel",
  "sm_aandeel",
  "ov_aandeel",
  "kv_aandeel",
  "mp_aandeel",
  
  
  "melkprod",
  "melkperha",
  "oppgras",
  "oppmais",
  "oppoverig",
  "opp_prgras",
  "opp_natuur",
  
  "gk_ev_hoev",
  "ev_gk_hoev",
  "bv_gk_hoev",
  "gk_bv_hoev",
  "aanleg_gk_hoev",
  "aankoop_aanleg_gk_hoev",
  "afv_gkp1_hoev",
  #AFvoer uit beginvoorraad
  "afv_gkp2_hoev",
  #Afvoer uit voorraad aanleg
  "afv_gkp3_hoev",
  #Afvoer niet bex ingekuild
  "afv_gkp4_hoev",
  
  "afv_ov1_hoev",
  #Afvoer overig uit begin
  "afv_ov2_hoev",
  #Afvoer overig uit voorraad_aanleg
  "afv_ov3_hoev",
  #Afvoer akkerbouwgewassen
  "afv_ov4_hoev",
  #Afvoer overige gewassen
  
  
  "gk_mut_hoev",
  "gr_opngraas_ds",
  
  "sm_ev_hoev",
  "ev_sm_hoev",
  "bv_sm_hoev",
  "sm_bv_hoev",
  "aanleg_sm_hoev",
  "aankoop_aanleg_sm_hoev",
  "afv_sm1_hoev",
  #Afvoer uit beginvoorraad
  "afv_sm2_hoev",
  #Afvoer uit voorraad aanleg
  "sm_mut_hoev",
  
  "ev_ov_hoev",
  "bv_ov_hoev",
  "aanleg_ov_hoev",
  "aankoop_aanleg_ov_hoev",
  "ov_mut_hoev",
  
  "ev_kv_hoev",
  "bv_kv_hoev",
  "kv_mut_hoev",
  "aankoop_aanleg_kv_hoev",
  "aanleg_kv_hoev",
  
  "kmaan_kg",
  "kmafv_kg",
  
  "pceigen_n",
  "pceigen_n_buurt",
  "akvoer_n",
  
  "oogst_gras",
  "oogst_mais",
  "oogst_ruwvoer",
  "verbruik_ruwvoer",
  "aankoop_ruwvoer",
  "afvoer_gras",
  "afvoer_mais",
  "aanleg_ruwvoer",
  "aanleg_ruwvoer_2",
  
  
  "opb_gras_ds",
  "opb_mais_ds",
  
  
  "strverbruik_kg",
  
  
  
  "dmgraasafv_ton"
  
  
  
)

# # Jouw formule hieronder gaat niet helemaal goed.
# Want de ruwvoeropbrengst wordt in de KLW uit de voeding berekend en niet via de meting van de graskuil. Daar zit een denkfout, dus de kans is heel klein dat die kengetallen naadloos op elkaar aansluiten.
# 
#   Je zou het percentage eigen geteeld voer tov voerverbruik voor N, P en VEM kunnen nemen (pagina voeding) en vervolgens vergelijken met het aandeel N van eigen teelt in het rantsoen (of P of VEM). Als het percentage eigen geteeld (veel) hoger is dan het aandeel eigen teelt in het rantsoen, dan heb je waarschijnlijk een bedrijf dat veel ruwvoer (over) heeft en toch veel krachtvoer voert.
# 
