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

#### Notitie 1 ####
# Functies
subsetKPIdata <- function(path_to_dataset,output_folder,output_file){
  
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
  
  #Inlezen van dataset
  if(file.exists(path_to_dataset)){
    dataset <- read_excel(path_to_dataset)
  } else {
    stop("Path naar dataset bestaat niet!")
  }

  outputToLog("Aantal observaties in volledige dataset", nrow(dataset))
  outputToLog("Aantal variables in volledige dataset", ncol(dataset))
  
  #KPI's
  dataset$kpi_n <- dataset$kring1_bedbal_afvtot / dataset$kring1_bedbal_aantot
  dataset$kpi_nh3 <- dataset$emnh3_tot_bdr / dataset$opp_totaal
  dataset$kpi_p <- dataset$kring2_bedbal_aantot - dataset$kring2_bedbal_afvtot
  dataset$kpi_akkerbouw <- dataset$opp_overig / dataset$opp_totaal

  dataset$bkg_totaal <- dataset$bkg_pens_vg1 + dataset$bkg_pens_gk1 + dataset$bkg_pens_sm1 + 
    dataset$bkg_pens_ovbp1 + dataset$bkg_pens_kvmp1 + dataset$bkg_pens_ovg1 + dataset$bkg_stop_meth1 + 
    dataset$bkg_stop_lach1 + dataset$bkg_voer_gras1 + dataset$bkg_voer_mais1 + dataset$bkg_voer_akker1 + 
    dataset$bkg_ene_elek1 + dataset$bkg_ene_gas1 + dataset$bkg_ene_dies1 + dataset$bkg_ene_ovfos1 + 
    dataset$bkg_ene_prod1 + dataset$bkg_aanv_ene1 + dataset$bkg_aanv_grsm1 + dataset$bkg_aanv_ovbp1 + 
    dataset$bkg_aanv_kvmp1 + dataset$bkg_aanv_mest1 + dataset$bkg_aanv_ove1
  dataset$kpi_bkg <- dataset$bkg_totaal / dataset$opp_totaal
  
  dataset$kpi_os <- dataset$osontw_bodem
  dataset$kpi_blijgras <- dataset$dzh_blijgras_aand
  dataset$kpi_weidmk <- dataset$weidmk_dgn
  
  dataset$melkperkoe <- dataset$melkprod / dataset$nkoe
  
  #Selectie subdata
  columns = c(
    "jaartal",
    "nkoe",
    "nkalf", #<1 jaar
    "npink", #>1 jaar
    "noverig", #overige graasdieren
    "nintensief",#varkens, pluimvee
    
    "melkprod",
    "melkperha",
    "melkperkoe",
    "opp_totaal", #totaal opp bedrijf
    "opp_prgras", #opp productie grasland
    "opp_natuur", #opp natuurgrasland
    "opp_mais", #opp snijmaisland
    "opp_overig", #opp overig bouwland (geen gras- of mais)
  
    "kring1_bedbal_afvtot", #Kringloop stikstof, bedrijfsbalans, afvoer stikstof totaal (kg/ha)
    "kring1_bedbal_aantot", #Kringloop stikstof, bedrijfsbalans, aanvoer stikstof totaal (kg/ha)
    "efficientie_N", #Efficiëntie veestapel stikstof (%)
    
    "emnh3_tot_bdr", #Ammoniakemissie totaal bedrijf (kg)
    
    "kring2_bedbal_aantot", #Kringloop fosfaat, bedrijfsbalans, aanvoer fosfaat totaal (kg/ha)
    "kring2_bedbal_afvtot", #Kringloop fosfaat, bedrijfsbalans, afvoer fosfaat totaal (kg/ha)
    
    "bkg_totaal", #Broeikasgassen bedrijf (kg CO2-eq)
  
    "osontw_bodem", #Aanvoer effectieve organische stof (kg/ha)
    
    "dzh_blijgras_aand", #Aandeel blijvend grasland (%)
    
    "weidmk_dgn", #Beweiding koeien, aantal dagen met weidegang
    "weidpi_dgn", #Beweiding pinken, aantal dagen met weidegang
    "weidka_dgn", #Beweiding kalveren, aantal dagen met weidegang
    
    "kpi_n",
    "kpi_nh3",
    "kpi_p",
    "kpi_akkerbouw",
    "kpi_bkg",
    "kpi_os",
    "kpi_blijgras",
    "kpi_weidmk"
  )

  if(all(columns %in% colnames(dataset))){
    dataset = dataset[, columns]
    dataset = dataset[complete.cases(dataset),]
    dataset = cbind("nr"=row.names(dataset),dataset)
  } else {
    stop("Input dataset heeft niet de juiste headers")
  }
  
  outputToLog("Aantal observaties in subset", nrow(dataset))
  outputToLog("Aantal variables in subset", ncol(dataset))
  
  #Write to Excel
  write_xlsx(dataset,output_file)
  
  outputToLog("Subset geschreven in",output_file)
  
}

makehistograms <- function(dataset,varname){
  
  #voor testen
  #dataset <- read_excel(output_file)
  #varname <- kpi_columns
  #ivar = 1
  
  kleuren_dm = c(rgb(0, 75, 35, maxColorValue = 255),rgb(220, 206, 22, maxColorValue = 255),
                 rgb(165, 165, 165, maxColorValue = 255),"black")
  ikleur = 0
  
  for (ivar in 1:length(varname)) {
    
    png(paste0("Histogram_",varname[ivar],".png"), width=12, height=12, units = "cm", res=200)
    
    if(ikleur < 3){
      ikleur = ikleur + 1
    }else{
      ikleur = 1
      }
    
    x <- matrix(unlist(dataset[,varname[ivar]]))
    hist(x, plot = T, col = kleuren_dm[ikleur],xlab=varname[ivar],ylab="Aantal",main=varname[ivar])
    
    dev.off()
  }
}

correlationplots <- function(dataset,varname,filename){
  
  #voor testen
  #dataset <- read_excel(output_file)
  #varname <- kpi_columns
  #filename <- "corrplots.pdf"
  
  kleuren_dm = c(rgb(0, 75, 35, maxColorValue = 255),rgb(220, 206, 22, maxColorValue = 255),
                 rgb(165, 165, 165, maxColorValue = 255),"black")
  
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

summarystats <- function(dataset,varname,output_file){
  #voor testen
  #dataset <- read_excel("VKAdata_notitie1.xlsx")
  #varname <- c(alg_columns,kpi_columns)
  #output_file <- "Summarystats.xlsx"
  
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
  
  #Write to Excel
  write_xlsx(sum.table[varname,stats],output_file)
  
  outputToLog("Summary statistics staan in",output_file)
}

# Selecteren subset KPI-k
path_to_dataset = "C:/Users/RiannevanBinsbergen/Wageningen University & Research/Brinke, Fleur - VKA_data/KLW_Database/dataset_VKA_2020_2020.xlsx"
output_folder = "C:/Users/RiannevanBinsbergen/OneDrive - Cooperatie De Marke U.A/Documenten/KPI-k gebiedspilot/Notitie1"
output_file = "VKAdata_notitie1.xlsx"

subsetKPIdata(path_to_dataset,output_folder,output_file)

#Samenvatting data
alg_columns <- c("melkprod","opp_totaal","melkperha")
kpi_columns <- c("kpi_n","kpi_nh3","kpi_p","kpi_akkerbouw",
  "kpi_bkg","kpi_os","kpi_blijgras","kpi_weidmk")

summarystats(read_excel(output_file),c(alg_columns,kpi_columns),"Summarystats.xlsx")

# Plots
makehistograms(read_excel(output_file),c(alg_columns,kpi_columns))

correlationplots(read_excel(output_file),c(alg_columns,kpi_columns),"Correlationplot_all.png")
correlationplots(read_excel(output_file),kpi_columns,"Correlationplot_kpi.png")
