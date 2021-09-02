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
                                        c("ï..Perioden", "Regio.s",variabelen_aantal,variabelen_opp)]
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
sum(landbouwdata_subset[landbouwdata_subset$ï..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s %in% gem_achterhoek,3])

landbouwdata_subset[landbouwdata_subset$ï..Perioden == 2020 & 
                      landbouwdata_subset$Regio.s == "Achterhoek (LB)",3]

# Check som van opp
sum(landbouwdata_subset[landbouwdata_subset$ï..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s == "Nederland",
                        colnames(landbouwdata_subset) %in% variabelen_opp2[2:7]])

sum(landbouwdata_subset[landbouwdata_subset$ï..Perioden == 2020 & 
                          landbouwdata_subset$Regio.s == "Nederland",
                        colnames(landbouwdata_subset) %in% variabelen_opp2[1]])

##### Write data #####
# CSV file met aantal data
write.csv(t(landbouwdata_subset[landbouwdata_subset$Regio.s %in% c("Nederland","Achterhoek (LB)"),
                                c("ï..Perioden", "Regio.s",variabelen_aantal)]),
          file="Landbouw_gemeente_subset_aantalAchterhoek.csv")

# CSV file met opp. data
write.csv(t(landbouwdata_subset[landbouwdata_subset$Regio.s %in% c("Nederland","Achterhoek (LB)"),
                                c("ï..Perioden", "Regio.s",variabelen_opp2)]),
          file="Landbouw_gemeente_subset_oppAchterhoek.csv")



#####
rm(list=ls())
