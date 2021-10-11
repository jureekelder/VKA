#Rianne van Binsbergen; 14-09-2021
#rianne.vanbinsbergen@demarke.eu
#Script voor het maken van figuren en tabellen tbv Pilot samenwerking melkvee-akkerbouw in De Achterhoek 

setwd('C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/Samenwerking MV-AK/Huidige situatie')

##### CBS data inlezen #####
regios <- read.csv('Gebieden_in_Nederland_2019_10082021_130901.csv',sep=";",header=F,skip=5)
colnames(regios)<-c('gem','LB_code','LB_naam','LG_code','LG_naam','LD_code','LD_naam','PV_code','PV_naam')

landbouwdata_cbs <- read.csv('Landbouw__gemeente_10082021_142626.csv',sep=";")
colnames(landbouwdata_cbs)[1] <- "Perioden"

# Selecteren Achterhoekse gemeenten
gem_achterhoek <- regios$gem[regios$LB_code=='LB2508']
variabelen_opp <- colnames(landbouwdata_cbs)[grepl("Oppervlakte",colnames(landbouwdata_cbs))]
variabelen_aantal <- colnames(landbouwdata_cbs)[grepl("Aantal",colnames(landbouwdata_cbs))]

# Subset met informatie over veehouderij en akkerbouw bedrijven in de Achterhoek
landbouwdata_cbs <- landbouwdata_cbs[landbouwdata_cbs$Regio.s %in% c("Nederland","Achterhoek (LB)",gem_achterhoek),
                                        c("Perioden", "Regio.s",variabelen_aantal,variabelen_opp)]
##### Adjust dataset #####
# opp. in ha
landbouwdata_cbs[,colnames(landbouwdata_cbs)%in%variabelen_opp] <- 
  landbouwdata_cbs[,colnames(landbouwdata_cbs)%in%variabelen_opp]/100

# nieuwe variabelen
landbouwdata_cbs$Akkerbouw.Oppervlakte.are <- landbouwdata_cbs$Akkerbouw.Oppervlakte.Akkerbouwgroenten..are. +
  (landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Fruit.onder.glas..m2./100) +
  (landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Glasgroenten..m2./100) +
  (landbouwdata_cbs$Tuinbouw.overig.Oppervlakte..hoeveelheid.Paddenstoelenteelt.Champignons..m2./100) +
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Bloembollen.en..knollen..are. + 
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Tuinbouwgroenten..are.

variabelen_opp2 <- c("Grondgebruik.Oppervlakte.Cultuurgrond..are.","Akkerbouw.Oppervlakte.are",
                     "Akkerbouw.Oppervlakte.Granen..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Blijvend.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Natuurlijk.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Tijdelijk.grasland..are.",
                     "Grasland.en.groenvoedergewassen.Oppervlakte.Groenvoedergewassen..are.")

##### Check data #####
# Check of som van aantal bedrijven in de gemeenten = totaal aantal bedrijven in de achterhoek
sum(landbouwdata_cbs[landbouwdata_cbs$Perioden == 2020 & 
                          landbouwdata_cbs$Regio.s %in% gem_achterhoek,3])

landbouwdata_cbs[landbouwdata_cbs$Perioden == 2020 & 
                      landbouwdata_cbs$Regio.s == "Achterhoek (LB)",3]

# Check som van opp
sum(landbouwdata_cbs[landbouwdata_cbs$Perioden == 2020 & 
                          landbouwdata_cbs$Regio.s == "Nederland",
                        colnames(landbouwdata_cbs) %in% variabelen_opp2[2:7]])

sum(landbouwdata_cbs[landbouwdata_cbs$Perioden == 2020 & 
                          landbouwdata_cbs$Regio.s == "Nederland",
                        colnames(landbouwdata_cbs) %in% variabelen_opp2[1]])

#### Excel file met aantal data ####
#load libraries
#install.packages("writexl")
library(writexl)

tabel_aantal <- data.frame(matrix(NA,nrow=14,ncol=5))
colnames(tabel_aantal) <- c("Grondgebruik","n_NL","perc_NL","n_8rhk","perc_8rhk")
tabel_aantal$Grondgebruik <- c("Akkerbouw en tuinbouw",           #1
                               "Akkerbouwgroenten",               #2
                               "Bloembollen en -knollen",         #3
                               "Tuinbouwgroenten",                #4
                               "Fruit onder glas",                #5
                               "Glasgroenten",                    #6
                               "Overige - champignons",           #7
                               "Granen",                          #8
                               "Grasland en groenvoedergewassen", #9
                               "Blijvend grasland",               #10
                               "Natuurlijk grasland",             #11
                               "Tijdelijk grasland",              #12
                               "Groenvoedergewassen",             #13
                               "Totaal")                          #14
#Nederland
tabel_aantal$n_NL[2] <- 
  landbouwdata_cbs$Akkerbouw.Aantal.bedrijven.Akkerbouwgroenten..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[3] <- 
  landbouwdata_cbs$Tuinbouw.open.grond.Aantal.bedrijven.Bloembollen.en..knollen..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[4] <- 
  landbouwdata_cbs$Tuinbouw.open.grond.Aantal.bedrijven.Tuinbouwgroenten..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                    landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[5] <- 
  landbouwdata_cbs$Tuinbouw.onder.glas.Aantal.bedrijven.Fruit.onder.glas..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                    landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[6] <- 
  landbouwdata_cbs$Tuinbouw.onder.glas.Aantal.bedrijven.Glasgroenten..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[7] <- 
  landbouwdata_cbs$Tuinbouw.overig.Aantal.bedrijven.Paddenstoelenteelt.Champignons..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                              landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[8] <- 
  landbouwdata_cbs$Akkerbouw.Aantal.bedrijven.Granen..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                        landbouwdata_cbs$Perioden == 2020]

tabel_aantal$n_NL[10] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Blijvend.grasland..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                          landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[11] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Natuurlijk.grasland..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                            landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[12] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Tijdelijk.grasland..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_NL[13] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Groenvoedergewassen..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                   landbouwdata_cbs$Perioden == 2020]

tabel_aantal$n_NL[14] <- 
  landbouwdata_cbs$Grondgebruik.Aantal.bedrijven.Cultuurgrond..aantal.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                   landbouwdata_cbs$Perioden == 2020]
#Achterhoek
tabel_aantal$n_8rhk[2] <- 
  landbouwdata_cbs$Akkerbouw.Aantal.bedrijven.Akkerbouwgroenten..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[3] <- 
  landbouwdata_cbs$Tuinbouw.open.grond.Aantal.bedrijven.Bloembollen.en..knollen..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[4] <- 
  landbouwdata_cbs$Tuinbouw.open.grond.Aantal.bedrijven.Tuinbouwgroenten..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                    landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[5] <- 
  landbouwdata_cbs$Tuinbouw.onder.glas.Aantal.bedrijven.Fruit.onder.glas..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                    landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[6] <- 
  landbouwdata_cbs$Tuinbouw.onder.glas.Aantal.bedrijven.Glasgroenten..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[7] <- 
  landbouwdata_cbs$Tuinbouw.overig.Aantal.bedrijven.Paddenstoelenteelt.Champignons..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                              landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[8] <- 
  landbouwdata_cbs$Akkerbouw.Aantal.bedrijven.Granen..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                landbouwdata_cbs$Perioden == 2020]

tabel_aantal$n_8rhk[10] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Blijvend.grasland..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                          landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[11] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Natuurlijk.grasland..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                            landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[12] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Grasland.Tijdelijk.grasland..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_aantal$n_8rhk[13] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Aantal.bedrijven.Groenvoedergewassen..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                   landbouwdata_cbs$Perioden == 2020]

tabel_aantal$n_8rhk[14] <- 
  landbouwdata_cbs$Grondgebruik.Aantal.bedrijven.Cultuurgrond..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                         landbouwdata_cbs$Perioden == 2020]

#Percentages
for (icol in c(3,5)){
  for (irow in c(2:8,10:13)){
    tabel_aantal[irow,icol] <- round(tabel_aantal[irow,icol-1]/tabel_aantal[14,icol-1] * 100,1)
  }
}

#Schrijven naar Excel
write_xlsx(tabel_aantal,"Aantal_bedrijven_grondgebruik_2020.xlsx")

#### Excel file met opp data ####
#load libraries
#install.packages("writexl")
library(writexl)

tabel_opp <- data.frame(matrix(NA,nrow=8,ncol=5))
colnames(tabel_opp) <- c("Grondgebruik","opp_NL","perc_NL","opp_8rhk","perc_8rhk")
tabel_opp$Grondgebruik <- c("Akkerbouw en tuinbouw (excl. granen)", #1
                            "Granen",                               #2
                            "Blijvend grasland",                    #3
                            "Natuurlijk grasland",                  #4
                            "Tijdelijk grasland",                   #5
                            "Groenvoedergewassen",                  #6
                            "Overige",                              #7
                            "Totaal")                               #8
#Nederland
tabel_opp$opp_NL[1] <- sum(
  landbouwdata_cbs$Akkerbouw.Oppervlakte.Akkerbouwgroenten..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                           landbouwdata_cbs$Perioden == 2020], 
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Bloembollen.en..knollen..are.[landbouwdata_cbs$Regio.s == "Nederland" &
                                                                                   landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Tuinbouwgroenten..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                    landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Fruit.onder.glas..m2.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                    landbouwdata_cbs$Perioden == 2020], 
  landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Glasgroenten..m2..[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.overig.Aantal.bedrijven.Paddenstoelenteelt.Champignons..aantal.[landbouwdata_cbs$Regio.s == "Nederland" &
                                                                                              landbouwdata_cbs$Perioden == 2020]
)
tabel_opp$opp_NL[2] <- 
  landbouwdata_cbs$Akkerbouw.Oppervlakte.Granen..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                landbouwdata_cbs$Perioden == 2020]

tabel_opp$opp_NL[3] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Blijvend.grasland..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                          landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_NL[4] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Natuurlijk.grasland..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                            landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_NL[5] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Tijdelijk.grasland..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                           landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_NL[6] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Groenvoedergewassen..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                                                   landbouwdata_cbs$Perioden == 2020]

tabel_opp$opp_NL[8] <- 
  landbouwdata_cbs$Grondgebruik.Oppervlakte.Cultuurgrond..are.[landbouwdata_cbs$Regio.s == "Nederland" & 
                                                                         landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_NL[7] <- tabel_opp$opp_NL[8] - sum(tabel_opp$opp_NL[1:6])


#Achterhoek (LB)
tabel_opp$opp_8rhk[1] <- sum(
  landbouwdata_cbs$Akkerbouw.Oppervlakte.Akkerbouwgroenten..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                   landbouwdata_cbs$Perioden == 2020], 
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Bloembollen.en..knollen..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" &
                                                                                   landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.open.grond.Oppervlakte.Tuinbouwgroenten..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                            landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Fruit.onder.glas..m2.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                           landbouwdata_cbs$Perioden == 2020], 
  landbouwdata_cbs$Tuinbouw.onder.glas.Oppervlakte.Glasgroenten..m2..[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                        landbouwdata_cbs$Perioden == 2020],
  landbouwdata_cbs$Tuinbouw.overig.Aantal.bedrijven.Paddenstoelenteelt.Champignons..aantal.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" &
                                                                                              landbouwdata_cbs$Perioden == 2020]
)
tabel_opp$opp_8rhk[2] <- 
  landbouwdata_cbs$Akkerbouw.Oppervlakte.Granen..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                        landbouwdata_cbs$Perioden == 2020]

tabel_opp$opp_8rhk[3] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Blijvend.grasland..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                  landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_8rhk[4] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Natuurlijk.grasland..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                    landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_8rhk[5] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Grasland.Tijdelijk.grasland..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                                   landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_8rhk[6] <- 
  landbouwdata_cbs$Grasland.en.groenvoedergewassen.Oppervlakte.Groenvoedergewassen..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                                           landbouwdata_cbs$Perioden == 2020]

tabel_opp$opp_8rhk[8] <- 
  landbouwdata_cbs$Grondgebruik.Oppervlakte.Cultuurgrond..are.[landbouwdata_cbs$Regio.s == "Achterhoek (LB)" & 
                                                                 landbouwdata_cbs$Perioden == 2020]
tabel_opp$opp_8rhk[7] <- tabel_opp$opp_8rhk[8] - sum(tabel_opp$opp_8rhk[1:6])

#Percentages
for (icol in c(3,5)){
  for (irow in 1:7){
    tabel_opp[irow,icol] <- round(tabel_opp[irow,icol-1]/tabel_opp[8,icol-1] * 100,1)
  }
}

#Afronden opp
tabel_opp$opp_NL <- round(tabel_opp$opp_NL,0)
tabel_opp$opp_8rhk <- round(tabel_opp$opp_8rhk,0)

#Schrijven naar Excel
write_xlsx(tabel_opp,"Opp_bedrijven_grondgebruik_2020.xlsx")

#### Waffle chart ####
library(ggplot2)
#install.packages("waffle")
library(waffle)

kleuren_dm = c(rgb(111, 103, 11, maxColorValue = 255), #bruin
               rgb(220, 206, 22, maxColorValue = 255), #geel (De Marke)
               rgb(0, 75, 35, maxColorValue = 255), #donker groen (De Marke)
               rgb(0, 117, 55, maxColorValue = 255), #groen
               rgb(0, 183, 87, maxColorValue = 255), #licht groen
               rgb(245, 239, 156, maxColorValue = 255), #licht geel
               rgb(191, 191, 191, maxColorValue = 255)) #licht grijs


tabel_opp$perc_NL[1:7]*4
percentages_NL <- c("Akkerbouw en tuinbouw (excl. granen)"=28, #1
                    "Granen"=38,                               #2
                    "Natuurlijk grasland"=17,                  #4
                    "Blijvend grasland"=153,                    #3
                    "Tijdelijk grasland"=45,                   #5
                    "Groenvoedergewassen"=46,                  #6
                    "Overige"=73                              #7
                    )

png("waffle_Nederland.png", width=16, height=11, units = "cm", res=200)
waffle(percentages_NL,rows=20,size=0.5,colors = kleuren_dm,flip=T,
       title = "Grondgebruik Nederland (2020)",
       xlab="1 blok = 0.5%")
dev.off()

tabel_opp$perc_8rhk[1:7]*4
percentages_8rhk <- c("Akkerbouw en tuinbouw (excl. granen)"=2, #1
                    "Granen"=23,                               #2
                    "Natuurlijk grasland"=7,                  #4
                    "Blijvend grasland"=199,                    #3
                    "Tijdelijk grasland"=68,                   #5
                    "Groenvoedergewassen"=77,                  #6
                    "Overige"=24                              #7
)

png("waffle_Achterhoek.png", width=16, height=11, units = "cm", res=200)
waffle(percentages_8rhk,rows=20,size=0.5,colors = kleuren_dm,flip=T,
       title = "Grondgebruik Achterhoek (2020)",
       xlab="1 blok = 0.5%")
dev.off()


#####
rm(list=ls())


#### Inlezen VKA data 20120-2020 ####
#Het laden van benodigde functies die ook op GIT staan.
library(devtools)
scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/VKA/main/R/opzettenDatabaseVKX.R")

for(script in scripts_to_source){
  source_url(script)
}

#Eenmalig dataset voor 2013-2020 aanmaken

#path_ledendata = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/Ledenlijst"
#path_dumpfiles = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database/DUMP_FILES"
#output_folder = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database"

#opzettenDataBaseVKX(type = "VKA",jaartal_start = 2013,jaartal_eind = 2020,
#                    path_ledendata = path_ledendata,
#                    path_dumpfiles = path_dumpfiles,
#                    output_folder = output_folder)

#Daarna dataset inlezen met read_excel functie (readxl package)


path_to_dataset = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database/dataset_VKA_2013_2020.xlsx"
output_path = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/Samenwerking MV-AK/VKA data"

#load libraries
library(readxl)
library(writexl)

# Functies
outputToLog <- function(name, quantity) {
  cat(paste(name, ": \n"))
  cat(paste(toString(quantity), "\n"))
  cat("\n")
}
BerekenRuwvoerVoorraad <- function(dataset){
  # Jur Eekelder
  
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

  # Ruwvoeropbrengst wordt in de KLW uit de voeding berekend en niet via de meting van de graskuil. 
  # Daar zit een denkfout, dus de kans is heel klein dat die kengetallen naadloos op elkaar aansluiten.
}

kleuren_dm = c(rgb(111, 103, 11, maxColorValue = 255), #bruin
               rgb(220, 206, 22, maxColorValue = 255), #geel (De Marke)
               rgb(0, 75, 35, maxColorValue = 255), #donker groen (De Marke)
               rgb(0, 117, 55, maxColorValue = 255), #groen
               rgb(0, 183, 87, maxColorValue = 255), #licht groen
               rgb(245, 239, 156, maxColorValue = 255), #licht geel
               rgb(191, 191, 191, maxColorValue = 255)) #licht grijs


#Set working directory
if(file.exists(output_path)){
  setwd(output_path)
} else {
  warning("Path naar voor output bestaat niet!")
}
outputToLog("Working Directory is", getwd())

#Inlezen van dataset
dataset <- read_excel(path_to_dataset)
outputToLog("Number of variables", ncol(dataset))
outputToLog("Number of observarions", nrow(dataset))
outputToLog("Data for the periods", unique(dataset$jaartal))
outputToLog("Number of farms", length(unique(dataset$PK_VKX)))

#### Oppervlakte/grondgebruik ####
tabel_opp <- data.frame(matrix(NA,nrow=5*8,ncol=4))
colnames(tabel_opp) <- c("Grondgebruik","jaar","opp_VKA","perc_VKA")
type_grond <-c("Overig bouwland (geen gras- of snijmais)", #1
               "Productie grasland",                       #2
               "Natuurlijk grasland",                      #3
               "Snijmaisland",                             #4
               "Totaal")                                   #5
tabel_opp$Grondgebruik <- rep(type_grond,8)
tabel_opp$jaar <- rep(2013:2020, each=5)

for (igrond in 1:5){
  for (year in 2013:2020){
    if(igrond == 1){
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <- 
        sum(dataset$opp_overig[dataset$jaartal == year])
    } else if (igrond == 2){
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <- 
        sum(dataset$opp_prgras[dataset$jaartal == year])
    } else if (igrond == 3){
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <- 
        sum(dataset$opp_natuur[dataset$jaartal == year])
    } else if (igrond == 4){
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <- 
        sum(dataset$opp_mais[dataset$jaartal == year])
    } else if (igrond == 5){
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <- 
        sum(dataset$opp_totaal[dataset$jaartal == year])
    }
  }
}

#Percentages
for (igrond in 1:4){
  for (year in 2013:2020){
    tabel_opp$perc_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] <-
      round(tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[igrond]] / 
      tabel_opp$opp_VKA[tabel_opp$jaar == year & tabel_opp$Grondgebruik == type_grond[5]] * 100,1)
  }
}

#Schrijven naar Excel
write_xlsx(tabel_opp,"Opp_grondgebruik_VKA.xlsx")

#### Waffle charts####
library(ggplot2)
#install.packages("waffle")
library(waffle)

year = 2013
print(year)
print(tabel_opp$perc_VKA[tabel_opp$jaar == year] * 4)
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=7,
                "Productie grasland"=303,
                "Natuurlijk grasland"=5,
                "Snijmaisland"=85)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2014
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=5,
                "Productie grasland"=320,
                "Natuurlijk grasland"=4,
                "Snijmaisland"=71)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2015
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=4,
                "Productie grasland"=324,
                "Natuurlijk grasland"=6,
                "Snijmaisland"=66)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2016
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=5,
                "Productie grasland"=327,
                "Natuurlijk grasland"=4,
                "Snijmaisland"=64)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2017
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=6,
                "Productie grasland"=326,
                "Natuurlijk grasland"=4,
                "Snijmaisland"=64)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()
 
year = 2018
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=7,
                "Productie grasland"=321,
                "Natuurlijk grasland"=6,
                "Snijmaisland"=66)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2019
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=5,
                "Productie grasland"=325,
                "Natuurlijk grasland"=5,
                "Snijmaisland"=65)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
       xlab="1 blok = 0.5%")
dev.off()

year = 2020
perc_vector<- c("Overig bouwland (geen gras- of snijmais)"=6,
                    "Productie grasland"=324,
                    "Natuurlijk grasland"=6,
                    "Snijmaisland"=64)
print(sum(perc_vector))
png(paste0("waffle_VKA_",year,".png"), width=16, height=11, units = "cm", res=200)
waffle(perc_vector,rows=20,size=0.5,colors = kleuren_dm[c(2,4,3,6)],flip=T,
       title = paste0("Grondgebruik VKA leden (",year,")"),
        xlab="1 blok = 0.5%")
dev.off()

#### Stacked Bar Plot ####
# library
library(ggplot2)

png("stacked_bar_plot_grondgebruik.png", width=16, height=12, units = "cm", res=200)
ggplot(tabel_opp[tabel_opp$Grondgebruik != 'Totaal',], aes(fill=Grondgebruik, y=opp_VKA, x=jaar)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = kleuren_dm[c(3,2,4,6)]) +
  scale_x_continuous(breaks = 2013:2020) +
  labs(x = "Jaar",y = "Oppervlakte in ha") +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )
dev.off()

#### Rantsoen samenstelling ####
rantsoen <- function(dataset,sub){
  
tabel_rantsoen <- data.frame(matrix(NA,nrow=5*8,ncol=7))
colnames(tabel_rantsoen) <- c("Voersoort","Jaar","Gemiddelde","SD","Min","Max","verbruik")
rantsoen_aandeel <-c("Vers gras",       #1
               "Graskuil",              #2
               "Snijmais",              #3
               "Overige bijproducten",  #4
               "Krachtvoer")            #5
tabel_rantsoen$Voersoort <- rep(rantsoen_aandeel,8)
tabel_rantsoen$Jaar <- rep(2013:2020, each=5)

for (irantsoen in 1:5){
  for (year in 2013:2020){
    if(irantsoen == 1){
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],3] <- 
        mean(dataset$gr_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],4] <- 
        sd(dataset$gr_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],5] <- 
        min(dataset$gr_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],6] <- 
        max(dataset$gr_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],7] <- 
        mean(dataset$gr_verbruik[dataset$jaartal == year])
    } else if(irantsoen == 2){
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],3] <- 
        mean(dataset$gk_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],4] <- 
        sd(dataset$gk_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],5] <- 
        min(dataset$gk_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],6] <- 
        max(dataset$gk_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],7] <- 
        mean(dataset$gk_verbruik[dataset$jaartal == year])
    } else if(irantsoen == 3){
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],3] <- 
        mean(dataset$sm_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],4] <- 
        sd(dataset$sm_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],5] <- 
        min(dataset$sm_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],6] <- 
        max(dataset$sm_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],7] <- 
        mean(dataset$sm_verbruik[dataset$jaartal == year])
    } else if(irantsoen == 4){
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],3] <- 
        mean(dataset$ov_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],4] <- 
        sd(dataset$ov_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],5] <- 
        min(dataset$ov_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],6] <- 
        max(dataset$ov_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],7] <- 
        mean(dataset$ov_verbruik[dataset$jaartal == year])
    } else if(irantsoen == 5){
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],3] <- 
        mean(dataset$kv_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],4] <- 
        sd(dataset$kv_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],5] <- 
        min(dataset$kv_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],6] <- 
        max(dataset$kv_aandeel[dataset$jaartal == year])
      tabel_rantsoen[tabel_rantsoen$Jaar == year & tabel_rantsoen$Voersoort == rantsoen_aandeel[irantsoen],7] <- 
        mean(dataset$kv_verbruik[dataset$jaartal == year])
    }
  }
}

#Schrijven naar Excel
write_xlsx(tabel_rantsoen,paste0("rantsoen_aandeel_VKA_",sub,".xlsx"))

#Bar plot
png(paste0("stacked_bar_plot_rantsoen_",sub,".png"), width=16, height=12, units = "cm", res=200)
  print(ggplot(tabel_rantsoen, aes(fill=Voersoort, y=Gemiddelde, x=Jaar)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = kleuren_dm[c(3,7,6,2,4)]) +
  scale_x_continuous(breaks = 2013:2020) +
  labs(x = "Jaar",y = "Gemiddeld rantsoen (%)") +
  theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ))
dev.off()

png(paste0("stacked_bar_plot_rantsoen_verbuik_",sub,".png"), width=16, height=12, units = "cm", res=200)
  print(ggplot(tabel_rantsoen, aes(fill=Voersoort, y=verbruik, x=Jaar)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = kleuren_dm[c(3,7,6,2,4)]) +
  scale_x_continuous(breaks = 2013:2020) +
  scale_y_continuous(breaks = c(0,250000,500000,750000,1000000), labels = c(0,250,500,750,1000)) +
  labs(x = "Jaar",y = "Gemiddelde voeropname jongvee en melkvee (1000 kg ds)") +
  theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ))
dev.off()
}

# volledige set
rantsoen(dataset,"all")

# bedrijven met bijproducten
subset <- unique(dataset$PK_VKX[dataset$ov_aandeel >= 10])
rantsoen(dataset[dataset$PK_VKX %in% subset,],"ov10")

# bedrijven met laag krachtvoer
subset <- unique(dataset$PK_VKX[dataset$kv_aandeel < 20])
rantsoen(dataset[dataset$PK_VKX %in% subset,],"kv20")



#### N-teelt vs. verbruik ####
#dataselectie
columns = c(
  "Achternaam",
  "Plaats",
  "PK_VKX",
  "jaartal",
  "nkoe",
  "nkalf", #<1 jaar
  "npink", #>1 jaar
  "melkprod",
  "melkperha",
  
  "opp_totaal", #totaal opp bedrijf
  "opp_prgras", #opp productie grasland
  "opp_natuur", #opp natuurgrasland
  "opp_mais", #opp snijmaisland
  "opp_overig", #opp overig bouwland (geen gras- of mais)
  
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
  "pceigen_n" #Voerverbruik, aandeel eigen voer, stikstof
)

if(all(columns %in% colnames(dataset))){
  dataset_stikstof = dataset[, columns]
} else {
  stop("Input dataset heeft niet de juiste headers")
}

dataset_stikstof$eiwiteig_tlt_totaal <- dataset_stikstof$eiwiteig_tlt_gk +
  dataset_stikstof$eiwiteig_tlt_sm + dataset_stikstof$eiwiteig_tlt_ov + 
  dataset_stikstof$eiwiteig_tlt_vg

dataset_stikstof$eiwiteig_vbr_totaal <- dataset_stikstof$eiwiteig_vbr_gk + 
  dataset_stikstof$eiwiteig_vbr_sm + dataset_stikstof$eiwiteig_vbr_ov +
  dataset_stikstof$eiwiteig_vbr_vg + dataset_stikstof$eiwiteig_vbr_kv

dataset_stikstof$eiwiteig_vbr_ruwvoer <- dataset_stikstof$eiwiteig_vbr_gk + 
  dataset_stikstof$eiwiteig_vbr_sm + dataset_stikstof$eiwiteig_vbr_ov +
  dataset_stikstof$eiwiteig_vbr_vg

dataset_stikstof$eiwiteig_tlt_vorigjaar <- NA
farms <- unique(dataset$PK_VKX)
for (ifarm in 1:length(farms)){
  for (iyear in 2014:2020) {
    dataset_stikstof$eiwiteig_tlt_vorigjaar[dataset_stikstof$PK_VKX == farms[ifarm] & 
                                              dataset_stikstof$jaartal == iyear] <- 
      dataset_stikstof$eiwiteig_tlt_totaal[dataset_stikstof$PK_VKX == farms[ifarm] &  
                                             dataset_stikstof$jaartal == iyear-1]
  }
}


#verbruik N - teelt N (negatief is meer teelt dan verbruik)
dataset_stikstof$eiwiteig_vbrmintlt <- dataset_stikstof$eiwiteig_vbr_ruwvoer - 
  dataset_stikstof$eiwiteig_tlt_totaal
dataset_stikstof$eiwiteig_vbrmintlt_ha <- dataset_stikstof$eiwiteig_vbrmintlt / 
  dataset_stikstof$opp_totaal

#verbruik N - teelt N jaar ervoor (meestal wordt ruwvoer in het jaar ervoor geproduceerd)
dataset_stikstof$eiwiteig_vbrmintlt_vorigjaar <- dataset_stikstof$eiwiteig_vbr_ruwvoer - 
  dataset_stikstof$eiwiteig_tlt_vorigjaar
dataset_stikstof$eiwiteig_vbrmintlt_vorigjaar_ha <- dataset_stikstof$eiwiteig_vbrmintlt_vorigjaar / 
  dataset_stikstof$opp_totaal

View(dataset_stikstof[dataset_stikstof$eiwiteig_vbrmintlt_ha < 0 & 
                        dataset_stikstof$eiwiteig_vbrmintlt_vorigjaar_ha < 0,
                      c(1,2,5,9,10,23,31,32,33,34)])
summary(dataset_stikstof$eiwiteig_vbrmintlt_vorigjaar_ha)

plot(dataset_stikstof$eiwiteig_vbr_ruwvoer,dataset_stikstof$eiwiteig_tlt_totaal,
     ylab="eigen teelt N: totaal (kg N)", xlab = "verbruik N: ruwvoer (kg N)",
     xlim = lim, ylim = lim)
abline(a=0,b=1,col="red")

# library
library(ggplot2)

lim <- c(min(dataset_stikstof$eiwiteig_tlt_totaal,dataset_stikstof$eiwiteig_vbr_ruwvoer),
         max(dataset_stikstof$eiwiteig_tlt_totaal,dataset_stikstof$eiwiteig_vbr_ruwvoer))

png("verbruik_vs_teelt.png", width=16, height=12, units = "cm", res=200)
ggplot(dataset_stikstof, aes(y=eiwiteig_tlt_totaal, x=eiwiteig_vbr_ruwvoer, colour=as.factor(jaartal))) + 
  geom_point() +
  geom_abline(slope=1,intercept = 0) +
  scale_x_continuous(name = "Verbruik N: ruwvoer (kg N)", limits = lim) +
  scale_y_continuous(name = "Eigen teelt N: totaal (kg N)", limits = lim)
dev.off()

#### ####
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
