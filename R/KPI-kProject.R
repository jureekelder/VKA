#Rianne van Binsbergen; 13-09-2021
#rianne.vanbinsbergen@demarke.eu
#Script voor het maken van figuren en tabellen tbv KPI-k Gebiedspilot De Achterhoek 


#Het laden van benodigde functies die ook op GIT staan.
library(devtools)
scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/VKA/main/R/opzettenDatabaseVKX.R")

for(script in scripts_to_source){
  source_url(script)
}

#### Inlezen VKA data 2020 ####
#Eenmalig dataset voor 2020 aanmaken

#path_ledendata = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - dataset/Ledenlijst"
#path_dumpfiles = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - dataset/KLW_Database/DUMP_FILES"
#output_folder = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - dataset/KLW_Database"

#opzettenDataBaseVKX(type = "VKA",jaartal_start = 2020,jaartal_eind = 2020,
#                    path_ledendata = path_ledendata,
#                    path_dumpfiles = path_dumpfiles,
#                    output_folder = output_folder)

#Daarna dataset inlezen met read_excel functie (readxl package)

#### Notitie 1 - koppelen data ####
# Functies
subsetKPIdata <- function(input_VKA,input_VALA,output_folder,
                          output_file_VKA,output_file_VALA){
  
  #load libraries
  library(readxl)
  library(writexl)
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }

  #Set working directory
  if(file.exists(output_folder)){
    setwd(output_folder)
    } else {
      warning("Path naar voor output bestaat niet!")
    }
  outputToLog("Working Directory is", getwd())
  

  #Inlezen VKA data
  if(file.exists(input_VKA)){
    datasetVKA <- read_excel(input_VKA)
  } else {
    stop("Filename dataset VKA bestaat niet!")
  }
  
  outputToLog("Aantal observaties in subset VKA", nrow(datasetVKA))
  outputToLog("Aantal variables in volledige dataset", ncol(datasetVKA))
  
  datasetVKA$bkg_totaal <- datasetVKA$bkg_pens_vg1 + datasetVKA$bkg_pens_gk1 + datasetVKA$bkg_pens_sm1 + 
    datasetVKA$bkg_pens_ovbp1 + datasetVKA$bkg_pens_kvmp1 + datasetVKA$bkg_pens_ovg1 + datasetVKA$bkg_stop_meth1 + 
    datasetVKA$bkg_stop_lach1 + datasetVKA$bkg_voer_gras1 + datasetVKA$bkg_voer_mais1 + datasetVKA$bkg_voer_akker1 + 
    datasetVKA$bkg_ene_elek1 + datasetVKA$bkg_ene_gas1 + datasetVKA$bkg_ene_dies1 + datasetVKA$bkg_ene_ovfos1 + 
    datasetVKA$bkg_ene_prod1 + datasetVKA$bkg_aanv_ene1 + datasetVKA$bkg_aanv_grsm1 + datasetVKA$bkg_aanv_ovbp1 + 
    datasetVKA$bkg_aanv_kvmp1 + datasetVKA$bkg_aanv_mest1 + datasetVKA$bkg_aanv_ove1
  
  
  #Inlezen kvk VALA
  if(file.exists(input_VALA)){
    datasetVALA <- read_excel(input_VALA)
  } else {
    stop("Filename dataset VALA bestaat niet!")
  }
  
  outputToLog("Aantal observaties in volledige VALA dataset", nrow(datasetVALA))
  
  #Selectie subdata
  columns = c(
    "kvk_nummer",
    "jaartal",
    "nkoe",
    "nkalf", #<1 jaar
    "npink", #>1 jaar
    "noverig", #overige graasdieren
    "nintensief",#varkens, pluimvee
    
    "melkprod",
    "melkperha",
    "opp_totaal", #totaal opp bedrijf
    "opp_prgras", #opp productie grasland
    "opp_natuur", #opp natuurgrasland
    "opp_mais", #opp snijmaisland
    "opp_overig", #opp overig bouwland (geen gras- of mais)
  
    "kring1_bedbal_afvtot", #Kringloop stikstof, bedrijfsbalans, afvoer stikstof totaal (kg/ha)
    "kring1_bedbal_aantot", #Kringloop stikstof, bedrijfsbalans, aanvoer stikstof totaal (kg/ha)
    "efficientie_N", #Efficiëntie veestapel stikstof (%)
    "benut_n_bed", #Benutting N  of bedrijfsniveau (%)
    
    "emnh3_tot_bdr", #Ammoniakemissie totaal bedrijf (kg)
    
    "kring2_bedbal_aantot", #Kringloop fosfaat, bedrijfsbalans, aanvoer fosfaat totaal (kg/ha)
    "kring2_bedbal_afvtot", #Kringloop fosfaat, bedrijfsbalans, afvoer fosfaat totaal (kg/ha)
    "efficientie_P", #Efficiëntie veestapel fosfaat (%)
    "benut_p_bed", #Benutting P  of bedrijfsniveau (%)
    
    "bkg_totaal", #Broeikasgassen bedrijf (kg CO2-eq)
  
    "osontw_bodem", #Aanvoer effectieve organische stof (kg/ha)
    
    "dzh_blijgras_aand", #Aandeel blijvend grasland (%)
    
    "weidmk_dgn", #Beweiding koeien, aantal dagen met weidegang
    "weidpi_dgn", #Beweiding pinken, aantal dagen met weidegang
    "weidka_dgn" #Beweiding kalveren, aantal dagen met weidegang
    
  )

  if(all(columns %in% colnames(datasetVKA))){
    datasetVKA = datasetVKA[, columns]
    datasetVKA = datasetVKA[complete.cases(datasetVKA),]
    datasetVKA = cbind("nr"=row.names(datasetVKA),datasetVKA)
  } else {
    stop("Input dataset heeft niet de juiste headers")
  }
  
  outputToLog("Aantal observaties in VKA subset", nrow(datasetVKA))
  outputToLog("Aantal variables in VKA subset", ncol(datasetVKA))
  
  #Write data to Excel (not kvk nummer)
  write_xlsx(datasetVKA[,-grep("kvk_nummer", names(datasetVKA))],output_file_VKA)
  
  outputToLog("Subset geschreven in",output_file_VKA)

  
  #Merge VKA en VALA
  datasetVALA$kvkVALA <- as.numeric(datasetVALA$`KvK VALA`)
  subsetVKA_VALA <- merge(datasetVKA[,c("kvk_nummer","nr","opp_totaal")],
                          datasetVALA[,c("KvK VALA","kvkVALA")], 
                          by.y = "kvkVALA", by.x = "kvk_nummer", all = F)
  
  outputToLog("Aantal observaties in merged dataset VKA en VALA", nrow(subsetVKA_VALA))
  
  #Write to Excel
  write_xlsx(subsetVKA_VALA,output_file_VALA)
  
  outputToLog("VALA subset geschreven in",output_file_VALA)

}

defineKLWKPI <- function(working_dir,input_file_VKA){
  #load libraries
  library(readxl)
  library(writexl)
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Set working directory
  if(file.exists(working_dir)){
    setwd(working_dir)
  } else {
    warning("Path naar voor output bestaat niet!")
  }
  outputToLog("Working Directory is", getwd())
  
  
  #Inlezen VKA data
  if(file.exists(input_file_VKA)){
    datasetVKA <- read_excel(input_file_VKA)
  } else {
    stop("Filename dataset VKA bestaat niet!")
  }
  
  outputToLog("Aantal observaties in dataset VKA", nrow(datasetVKA))
  outputToLog("Aantal variables in dataset VKA", ncol(datasetVKA))
  
  datasetVKA$nr <- as.numeric(datasetVKA$nr)
  
  datasetVKA$kpi_n <- datasetVKA$kring1_bedbal_afvtot / datasetVKA$kring1_bedbal_aantot
  datasetVKA$kpi_nbenut <-datasetVKA$benut_n_bed
  datasetVKA$kpi_nh3 <- datasetVKA$emnh3_tot_bdr / datasetVKA$opp_totaal
  datasetVKA$kpi_p <- datasetVKA$kring2_bedbal_aantot - datasetVKA$kring2_bedbal_afvtot
  datasetVKA$kpi_voedsel <- datasetVKA$opp_overig / datasetVKA$opp_totaal
  datasetVKA$kpi_bkg <- datasetVKA$bkg_totaal / datasetVKA$opp_totaal
  datasetVKA$kpi_os <- datasetVKA$osontw_bodem
  datasetVKA$kpi_blijgras <- datasetVKA$dzh_blijgras_aand
  datasetVKA$kpi_weidmk <- datasetVKA$weidmk_dgn
  
  datasetVKA$melkperkoe <- datasetVKA$melkprod / datasetVKA$nkoe
  
  return(datasetVKA)
}


## Maak subset bestanden (éénmalig) ##
input_file_VKA = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database/dataset_VKA_2020_2020.xlsx"
working_dir = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/KPI-k gebiedspilot/Notitie1/Nieuw"
output_file_VKA = "VKAdata_notitie1.xlsx"
input_file_VALA = "../KvK_VALA 2020.xlsx"
output_file_VALA = "VKA_VALA_KvK_opp.xlsx"

subsetKPIdata(input_file_VKA,input_file_VALA,working_dir,
              output_file_VKA,output_file_VALA)

## KringloopWijzer KPI's ##
working_dir = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/KPI-k gebiedspilot/Notitie1/Nieuw"
input_file_VKA = "VKAdata_notitie1.xlsx"

datasetVKA <- defineKLWKPI(working_dir,input_file_VKA)

## Koppelen aan VALA data ##
working_dir = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/KPI-k gebiedspilot/Notitie1/Nieuw"
input_file_VALA = "../KPI project 6 okt 2021 SCAN-GIS 2020_def.xlsx"
sheet_VALA = "KPItotaal"
output_file_KPI = "Gecombineerde_dataset_KPIs.xlsx"

mergeVKA_VALA <- function(datasetVKA,working_dir,input_file_VALA,sheet_VALA,output_file_KPI) {
  #load libraries
  library(readxl)
  library(writexl)
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Set working directory
  if(file.exists(working_dir)){
    setwd(working_dir)
  } else {
    warning("Path naar voor output bestaat niet!")
  }
  outputToLog("Working Directory is", getwd())
  
  
  #Inlezen VALA data
  if(file.exists(input_file_VALA)){
    datasetVALA <- read_excel(input_file_VALA,sheet = sheet_VALA)
  } else {
    stop("Filename dataset VALA bestaat niet!")
  }
  
  outputToLog("Aantal observaties in dataset VALA", nrow(datasetVALA))
  outputToLog("Aantal variables in dataset VALA", ncol(datasetVALA))
  
  datasetVALA <- datasetVALA[!(datasetVALA$Deelnemer == "Gemiddelde"),]
  datasetVALA$Deelnemer <- as.numeric(datasetVALA$Deelnemer)
  
  kpiVALA <- as.data.frame(matrix(NA,length(unique(datasetVALA$Deelnemer)),6))
  colnames(kpiVALA)<-c("nr","kpi_kg","kpi_nl","kpi_nlw",
                       "kpi_gb","kpi_gbw")
  kpiVALA$nr <- unique(datasetVALA$Deelnemer)
  
  isEmpty <- function(x) {
    return(length(x)==0)
  }
  
  for (inr in 1:nrow(kpiVALA)) {
    if(!isEmpty(datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                           datasetVALA$KPI == "Graslandbeheer"])){
      kpiVALA$kpi_kg[inr] <- datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                                         datasetVALA$KPI == "Graslandbeheer"]*100
    }
    
    if(!isEmpty(datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                              datasetVALA$KPI == "Natuur en Landschap"])){
      kpiVALA$kpi_nl[inr] <- datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                                             datasetVALA$KPI == "Natuur en Landschap"]*100
      kpiVALA$kpi_nlw[inr] <- datasetVALA$`% KPI score waarde`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                                              datasetVALA$KPI == "Natuur en Landschap"]*100
    }
    
    if(!isEmpty(datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                              datasetVALA$KPI == "groenblauwe dooradering"])){
      kpiVALA$kpi_gb[inr] <- datasetVALA$`% KPI score opp`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                                              datasetVALA$KPI == "groenblauwe dooradering"]*100
      kpiVALA$kpi_gbw[inr] <- datasetVALA$`% KPI score waarde`[datasetVALA$Deelnemer == kpiVALA$nr[inr] &
                                                              datasetVALA$KPI == "groenblauwe dooradering"]*100
    }
  }
 
  datasetKPI <- merge(datasetVKA,kpiVALA,by="nr",all = T)
  
  outputToLog("Aantal observaties in merged dataset VKA en VALA", nrow(datasetKPI))
  outputToLog("Aantal variabelen in merged dataset VKA en VALA", ncol(datasetKPI))
  
  #Write to Excel
  write_xlsx(datasetKPI,output_file_KPI)
  
  outputToLog("KPI dataset geschreven in",output_file_KPI)
  
  return(datasetKPI)
  
}

datasetKPI <- mergeVKA_VALA(datasetVKA,working_dir,input_file_VALA,sheet_VALA,output_file_KPI)


#### Notitie 1 - analyseren data ####
#Inlezen data (als datasetKPI nog niet in Environment)
library(readxl)
setwd("C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/KPI-k gebiedspilot/Notitie1/Nieuw")
datasetKPI <- read_excel("Gecombineerde_dataset_KPIs.xlsx")

#Functies
kleuren_dm = c(rgb(0, 75, 35, maxColorValue = 255),rgb(220, 206, 22, maxColorValue = 255),
               rgb(165, 165, 165, maxColorValue = 255),"black")

summarystats <- function(dataset,varname,output_file){
  #load libraries
  #install.packages("writexl")
  library(writexl)
  
  #Algemene functies
  outputToLog <- function(name, quantity) {
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #install.packages("psych")
  library(psych)
  sum.table <- describe(dataset)
  sum.table$name <- row.names(sum.table)
  
  stats <- c("name","n","mean","sd","min","max")
  
  #Statistics for variables not equal to 0
  dataset[dataset == 0] <- NA
  sum.table.ne0 <- describe(dataset)
  sum.table.ne0$name <- paste0(row.names(sum.table.ne0),".ne0")
  
  #Write to Excel
  write_xlsx(rbind(sum.table[varname,stats],sum.table.ne0[varname,stats]),output_file)
  
  outputToLog("Summary statistics staan in",output_file)
}

makehistograms <- function(dataset,varname){
  
  #voor testen
  #dataset <- read_excel(output_file)
  #varname <- kpi_columns
  #ivar = 1
  
  ikleur = 0
  
  for (ivar in 1:length(varname)) {
    
    png(paste0("Histogram_",varname[ivar],".png"), width=12, height=12, units = "cm", res=200)
    
    if(ikleur < 3){
      ikleur = ikleur + 1
    }else{
      ikleur = 1
    }
    
    x <- matrix(unlist(dataset[,varname[ivar]]))
    hist(x, plot = T, col = kleuren_dm[ikleur],xlab=varname[ivar],ylab="Aantal",main="")
    
    dev.off()
  }
}

makescatterplot <- function(dataset, varname.x, varname.y){
 
  #testen
  #varname.x <- "kpi_n"
  #varname.y <- "kpi_nbenut"
  #dataset <- datasetKPI
  
  png(paste0("Scatter_",varname.x,"_",varname.y,".png"), width=12, height=12, units = "cm", res=200)
  
  plot(dataset[,varname.x],dataset[,varname.y],col = kleuren_dm[1],pch=20,
       xlab=varname.x,ylab=varname.y,main="")
  z <- lm(dataset[,varname.y] ~ dataset[,varname.x])
  abline(z, col= kleuren_dm[2])
  
  dev.off()
  
  return(paste("correlation =",cor(dataset[,varname.x],dataset[,varname.y])))
  
}

correlationplots <- function(dataset,varname,filename,factorcolor){
  
  #voor testen
  #dataset <- read_excel(output_file)
  #varname <- kpi_columns
  #filename <- "corrplots.pdf"
  
 # Function to add histograms
  panel.hist <- function(x) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    his <- hist(x, plot = FALSE)
    breaks <- his$breaks
    nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = kleuren_dm[3])
  }
  
  # Function to add correlation coefficients
  # install.packages("RColorBrewer") 
  # Needed to get color gradient
  library(RColorBrewer)
  
  
  cols = brewer.pal(11, "RdBu")   # goes from red to white to blue
  pal = colorRampPalette(cols)
  cor_colors = data.frame(correlation = seq(-1,1,0.01), 
                          correlation_color = pal(201)[1:201])  # assigns a color for each r correlation value
  cor_colors$correlation_color = as.character(cor_colors$correlation_color)
  
  panel.cor <- function(x, y, digits = 2, cex.cor) {
    par(usr = c(0, 1, 0, 1))
    u <- par('usr') 
    names(u) <- c("xleft", "xright", "ybottom", "ytop")
    r <- cor(x, y)
    
    bgcolor = cor_colors[2+(-r+1)*100,2]    # converts correlation into a specific color
    do.call(rect, c(col = bgcolor, as.list(u))) # colors the correlation box
    
    text(0.5, 0.75, round(r,2),cex=1) # prints correlation coefficient
    
  }
  
  # Creating the scatter plot matrix
  data <- dataset[,varname]
  png(filename, width=16, height=16, units = "cm", res=200)
  pairs(data,                       # Data frame of variables
        lower.panel = points,
        upper.panel = panel.cor,    # Disabling the upper panel
        #        diag.panel = panel.hist,    # Adding the histograms
        labels = varname,  # Variable names
        main = paste0("KPI's VKA data 2020 (n = ",nrow(dataset),")"),    # Title of the plot
        row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
        gap = 0.1,                  # Distance between subplots
  )
  dev.off()
  
}

#Samenvatting data
alg_columns <- c("melkprod","opp_totaal","melkperha")

kpi_columns <- c("kpi_n","kpi_nbenut","kpi_nh3","kpi_p","kpi_voedsel",
  "kpi_bkg","kpi_os","kpi_kg","kpi_blijgras","kpi_nl","kpi_nlw",
  "kpi_gb","kpi_gbw","kpi_weidmk")

summarystats(datasetKPI,c(alg_columns,kpi_columns,"weidpi_dgn","weidka_dgn"),"Summarystats.xlsx")

# Plots
makehistograms(datasetKPI,c("kpi_n","kpi_nbenut"))
makescatterplot(datasetKPI,"kpi_n","kpi_nbenut")




correlationplots(read_excel(output_file),c(alg_columns,kpi_columns),"Correlationplot_all.png")
correlationplots(read_excel(output_file),kpi_columns,"Correlationplot_kpi.png")
