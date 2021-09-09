#Jur Eekelder; 23-08-2021
#jur.eekelder@demarke.eu
#Script voor het maken van KLW databases op basis van KLW .dmpx files ÉN ledenlijsten VKA / VKO 

#INPUTS
#type --> string die aangeeft of het VKA of VKO is
#jaartal_start --> integer die beginjaar van dataset aangeeft
#jaartal_eind ---> integer die eindjaar van dataset aangeeft
#path_ledendata --> string met path naar VKA of VKO ledendata
#path_dunpfiles --> string met path naar mappen met .DMPX files vanuit KLW. .DMPX files per jaar in een aparte submap.
#kolommen_ledenlijst --> OPTIONEEL: welke kolommen uit de ledenlijst data hebben we nodig?
#In het geval dat het om 1 jaar gaat kunnen jaartal_start en jaartal_eind als hetzelfde ingevuld worden
#output_folder --> string met path naar output folder
#

#Voor testen:
#path_ledendata = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA/Ledenlijst"
#path_dumpfiles = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA/KLW_DUMP"
#output_folder = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA"
#opzettenDataBaseVKX(type = "VKA", 2013, 2020, path_ledendata = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA/Ledenlijst", path_dumpfiles = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA/KLW_DUMP", output_folder = "C:/Users/JurEekelder/Documents/analyseKLW_VKA_VKO/VKA")

#BENODIGDE FUNCTIES:
#getDataInFolder
#toevoegenKengetallenKLW
#vergelijkenKLWBestanden
#controleDatasetScript

opzettenDataBaseVKX <- function(type,
           jaartal_start,
           jaartal_eind,
           path_ledendata,
           path_dumpfiles,
           output_folder,
           kolommen_ledenlijst = c(
             "Lidmaatschapsnummer",
             "Studiegroep",
             "Tussenvoegsel",
             "Achternaam",
             "Straatnaam",
             "Huisnummer",
             "Huisnummertoevoeging",
             "Postcode",
             "Plaats"),
           path_klw_bestanden = NULL){


#Laden libraries
library(dplyr)  
library(openxlsx)
  
#Het laden van benodigde functies die ook op GIT staan.
library(devtools)
scripts_to_source = c("https://raw.githubusercontent.com/jureekelder/scriptsalgemeen/main/R/getDataInFolder.R",
                      "https://raw.githubusercontent.com/jureekelder/VKA/main/R/toevoegenKengetallenKLW.R",
                      "https://raw.githubusercontent.com/jureekelder/VKA/main/R/vergelijkenKLWBestanden.R",
                      "https://raw.githubusercontent.com/jureekelder/VKA/main/R/controleDatasetKLW.R")


for(script in scripts_to_source){
  source_url(script)
}
  
  
#Check inputs
if(!is.character(type)){
  stop("type (VKA/VKO/...) is niet gespecificeerd als character string")
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

#Welke sequence van jaartallen willen we in de dataset?  
jaartallen = seq(jaartal_start, jaartal_eind, 1)

#Algemene functies
outputToLog <- function(name, quantity) {
  cat(paste(name, ": \n"))
  cat(paste(toString(quantity), "\n"))
  cat("\n")
}

#Wat is de working directory:
if(file.exists(output_folder)){
  setwd(output_folder)
}
outputToLog("Working Directory is", getwd())

outputToLog("Functie gestart --> bouwen database voor", type)
outputToLog("Voor jaartallen", jaartallen)

#Functie om getallen uit string te krijgen.
getNumbersFromString <- function(x) {
  elements = unlist(strsplit(x, ""))
  numberString = NULL
  
  for (e in elements) {
    suppressWarnings({
      number = as.numeric(e)
    })
    if (is.na(number)) {
      
    } else {
      numberString = paste(numberString, number, sep = "")
    }
    
  }
  
  if (is.null(numberString)) {
    return(NA)
  } else {
    return(numberString)
  }
  
}

#Functie om letters uit string te krijgen.
getCharactersFromString <- function(x) {
  elements = unlist(strsplit(x, ""))
  characterString = NULL
  
  for (e in elements) {
    suppressWarnings({
      number = as.numeric(e)
    })
    if (is.na(number)) {
      characterString = paste(characterString, e, sep = "")
    } else {
      
    }
    
  }
  
  if (is.null(characterString)) {
    return(NA)
  } else {
    return(characterString)
  }
}


#Inlezen van Ledenlijst
if(file.exists(path_ledendata)){
data_VKX_input = getDataInFolder(path_ledendata)
} else {
  stop("Path naar Ledendata bestaat niet!")
}


if (all(kolommen_ledenlijst %in% colnames(data_VKX_input))) {
  data_VKX_input = data_VKX_input[, kolommen_ledenlijst]
} else {
  stop("Ledenbestand heeft niet de headers zoals gespecificeerd in functie-call")
}
outputToLog("Aantal observaties in volledige VKX data", nrow(data_VKX_input))

#Verzeker dat de Lidmaatschapnummers en studiegroepnummers gezien worden als numeric voor het joinen later.
data_VKX_input$Lidmaatschapsnummer = as.integer(data_VKX_input$Lidmaatschapsnummer)
data_VKX_input$Studiegroep = as.integer(data_VKX_input$Studiegroep)


data_VKX_input = data_VKX_input %>%
  dplyr::filter(Lidmaatschapsnummer > 0)

if(type == "VKA"){
  data_VKX_input = data_VKX_input %>% dplyr::filter(!Studiegroep %in% c(30)) #Groep 30 zijn de loonwerkers
}

outputToLog("Aantal leden in gefilterde VKX ledenbestand",
            nrow(data_VKX_input))

#Logica om van de postcode en huisnummer combinatie een primary key te maken, bijvoorbeeld Kapelweg 36, 7134 RJ wordt --> 7134RJ36
data_VKX_input$postcode_temp = gsub(" ", "", data_VKX_input$Postcode, fixed = T) #verwijderen van spaties
data_VKX_input$postcode_temp = toupper(data_VKX_input$postcode_temp) #naar hoofdletters!
data_VKX_input$huisnummer = sapply(data_VKX_input$Huisnummer, getNumbersFromString)
data_VKX_input$PK_VKX = paste(data_VKX_input$postcode_temp,
                              data_VKX_input$huisnummer,
                              sep = "")
data_VKX_input$PK = data_VKX_input$PK_VKX
data_VKX_input$postcodeVKX = data_VKX_input$postcode_temp


#Logica om van de naam en de plaats een secondary key te maken
data_VKX_input$naam_plaats = paste(
  ifelse(
    is.na(data_VKX_input$Tussenvoegsel),
    "",
    data_VKX_input$Tussenvoegsel
  ),
  data_VKX_input$Achternaam,
  data_VKX_input$Plaats,
  sep = ""
)
data_VKX_input$naam_plaats = tolower(gsub(" ", "", data_VKX_input$naam_plaats, fixed = T))
data_VKX_input$SK_VKX = data_VKX_input$naam_plaats


data_VKX_Meta = data_VKX_input %>%
  dplyr::select(Achternaam,
                Plaats,
                Lidmaatschapsnummer,
                Studiegroep,
                PK,
                PK_VKX,
                SK_VKX,
                postcodeVKX)


#Check of er niet 2 leden zijn met hetzelfde lidmaatschapsnummer:
data_VKX_Lidmaatschapnummers = data_VKX_Meta %>%
  dplyr::group_by(Lidmaatschapsnummer) %>%
  dplyr::summarise(n = dplyr::n())

if (any(data_VKX_Lidmaatschapnummers$n > 1)) {
  stop("ERROR, 2 VKX-leden hebben hetzelfde lidmaatschapsnummer!")
}


####Voorbereiden KLW data####
#KLW Data in zit in allemaal submappen

if(file.exists(path_dumpfiles)){
  
  folder_list = list.files(path_dumpfiles)
  
  data_KLW_all = NULL
  for(folder in folder_list){
    
    current_path = paste(path_dumpfiles, "/",folder , sep = "")
    
    #In de functie getDataInFolder worden de 6 .DMPX al horizontaal samengevoegd. 
    data_KLW_current = getDataInFolder(current_path, patternFile = ".dmp", fieldSeperator = "@")
    
    #De KLW data per map moet vervolgens verticaal onder elkaar gezet worden:
    data_KLW_all = rbind(data_KLW_all, data_KLW_current)
    
  }
  
} else {
  stop("Path naar DUMP files bestaat niet!")
}

data_KLW_input = data_KLW_all

#Samenvatting van KLW input
outputToLog("Samenvatting van ingelezen KLW data", str(data_KLW_input))

#CHECKS OP KLW DATA:

#Zorg dat numerieke kolommen als numeriek worden opgeslagen!
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(
    x
  ))))) & is.character(x)
}

data_KLW_input = data_KLW_input %>% dplyr::mutate_if(is_all_numeric, as.numeric)


if (length(unique(data_KLW_input$versie)) > 1) {
  stop("Verschillende KLW-versies in dataset! Haal alles door de dezelfde rekenmotor!")
}

outputToLog("Aantal observaties in volledige KLW data",
            nrow(data_KLW_input))
outputToLog("Aantal versienummers KLW-versie gevonden", length(unique(data_KLW_input$versie)))
outputToLog("Gevonden versienummers KLW-versie", (unique(data_KLW_input$versie)))
outputToLog("Jaartallen in KLW data", unique(data_KLW_input$jaartal))

#Logica om van de postcode en huisnummer combinatie een primary key te maken.
if (TRUE) {
  data_KLW_input$postcode = gsub(" ", "", data_KLW_input$postcode, fixed = T) #verwijder spaties in string
  data_KLW_input$postcode = toupper(data_KLW_input$postcode) #naar HOOFDLETTERS
  data_KLW_input$huisnummer = sapply(data_KLW_input$straat, getNumbersFromString)
  data_KLW_input$PK = paste(data_KLW_input$postcode, data_KLW_input$huisnummer, sep = "")
  data_KLW_input$PK_KLW = data_KLW_input$PK
}

#Logica om van de naaminvoer een secondary key te maken.
if (TRUE) {
  data_KLW_input$naaminv_geenjaar = sapply(data_KLW_input$naaminv, getCharactersFromString)
  data_KLW_input$naaminv_geenjaar = tolower(gsub(" ", "", data_KLW_input$naaminv_geenjaar, fixed = T))
  data_KLW_input$SK_KLW = data_KLW_input$naaminv_geenjaar
}

#Selecteer enkel de jaartallen die we willen hebben.
data_KLW_input = data_KLW_input %>% dplyr::filter(jaartal %in% jaartallen)

#Zitten er dubbele KringloopWijzers in de dataset?
data_KLW_input_duplicates = duplicated(data_KLW_input %>% dplyr::select(jaartal, PK_KLW))

if (any(data_KLW_input_duplicates)) {
  outputToLog("Dubbele KLW data in bestand: ", data_KLW_input[data_KLW_input_duplicates, c("naaminv", "jaartal")])
  
  data_KLW_input = distinct(data_KLW_input, jaartal, PK_KLW, .keep_all = T)
}


#Geef iedere KLW een uniek nummer
data_KLW_input$ID_KLW = seq(nrow(data_KLW_input))

#Welke "class" hebben de kolommen
data_KLW_input_columns_class = t(sapply(data_KLW_input, class))

data_KLW_input$pceigen_n = as.numeric(sapply(data_KLW_input$pceigen_n, getNumbersFromString))
data_KLW_input$kring1_benut_tot = as.numeric(sapply(data_KLW_input$kring1_benut_tot, getNumbersFromString))
data_KLW_input$kring2_benut_tot = as.numeric(sapply(data_KLW_input$kring2_benut_tot, getNumbersFromString))
data_KLW_input$kring1_benut_bod = as.numeric(sapply(data_KLW_input$kring1_benut_bod, getNumbersFromString))
data_KLW_input$kring2_benut_bod = as.numeric(sapply(data_KLW_input$kring2_benut_bod, getNumbersFromString))
data_KLW_input$kring1_benut_mst = as.numeric(sapply(data_KLW_input$kring1_benut_mst, getNumbersFromString))

#Toevoegen nieuwe kengetallen
data_KLW_input = toevoegenKengetallenKLW(data_KLW_input)

#Kolommen sorteren
data_KLW_input = data_KLW_input %>% dplyr::select(ID_KLW,
                                           PK_KLW,
                                           SK_KLW,
                                           jaartal,
                                           naaminv,
                                           postcode,
                                           huisnummer,
                                           everything())


#Vergelijken van database van ALLE KLW versus wat er in de DUMP files staat.
if (!is.null(path_klw_bestanden)) {
  if (file.exists(path_klw_bestanden)) {
    outputToLog("Zoeken naar bronbestanden KLW voor controle op basis van [naaminv]",
                NULL)
    
    data_KLW_bron = vergelijkenKLWBestanden(path_klw_bestanden)
    
    
    data_KLW_bron$naaminv_geenjaar = sapply(data_KLW_bron$naam_plaats, getCharactersFromString)
    data_KLW_bron$naaminv_geenjaar = tolower(gsub(" ", "", data_KLW_bron$naaminv_geenjaar, fixed = T))
  } else {
    stop("Path voor KLW bronbestanden bestaat niet")
  }
  
  data_KLW_naam_plaats = data_KLW_input %>% dplyr::select(naaminv_geenjaar, jaartal)
  data_KLW_bron = data_KLW_bron %>% dplyr::select(naaminv_geenjaar, jaartal)
  summary(comparedf(
    data_KLW_bron,
    data_KLW_naam_plaats,
    by = c("naaminv_geenjaar", "jaartal")
  ))
  
} else {
  outputToLog("Vergelijking met ruwe data in mappen wordt overgeslagen. Path_klw_bestanden is NULL.", NULL)
} 


data_KLW_Meta = data_KLW_input %>%
  dplyr::select(ID_KLW,
                naaminv,
                jaartal,
                PK,
                PK_KLW,
                SK_KLW,
                postcode,
                huisnummer)



#### Gebaseerd op naaminv, probeer KLW samen te voegen
data_KLW_SK_jaar = data_KLW_input %>% dplyr::select(ID_KLW, SK_KLW, jaartal)

data_KLW_SK_jaar_count = data_KLW_SK_jaar %>% dplyr::group_by(SK_KLW, jaartal) %>% dplyr::summarise(count = dplyr::n())

#MATCHEN VAN VKX DATA AAN KLW DATA OP BASIS VAN PK, SK, optioneel: KVK
#DIT GEBEURT PER JAARTAL:

#Initialiseren van totaal over alle jaren
data_KLW_VKX_Meta_Matched_Totaal = NULL

#Initialiseren van VKX bedrijven die niet gematched zijn
data_VKX_Mismatch = NULL

#Initialiseren van KLW data die niet gematched zijn
data_KLW_Mismatch = NULL

#Functie om NA kolommen te verwijderen
verwijderNAKolom <- function(x) {
  y = x[, colSums(is.na(x)) != nrow(x)]
  return(y)
}

for (j in jaartallen) {
  outputToLog("VKX leden koppelen aan KLW voor jaartal", j)
  
  #VKX ledenbestand aan KLW data koppelen op basis van de PK voor huidige jaar
  data_KLW_Meta_Jaar = data_KLW_Meta %>% dplyr::filter(jaartal == j)
  
  if(nrow(data_KLW_Meta_Jaar) > nrow(data_VKX_Meta)){
    outputToLog("Er zijn meer KLW observaties dan VKX leden", NULL)
  } else if(nrow(data_KLW_Meta_Jaar) < nrow(data_VKX_Meta)){
    outputToLog("Er zijn minder KLW observaties dan VKX leden", NULL)
  } else if(nrow(data_KLW_Meta_Jaar) == nrow(data_VKX_Meta)){
    outputToLog("Aantal KLW observaties is gelijk aan aantal VKX leden", NULL)
  }
  
  data_KLW_VKX_Meta = full_join(data_VKX_Meta, data_KLW_Meta_Jaar, by = "PK")
  
  #Door de full join zijn de regels: A) goed gematched B) VKX data is niet gematched, C) KLW dat niet gematched
  #In geval van B) --> dan is kolom SK_KLW gelijk aan NA
  #In geval van C) --> dan is kolom SK_VKX gelijk aan NA
  data_KLW_VKX_Meta_Join = data_KLW_VKX_Meta %>% dplyr::filter(!is.na(SK_VKX) &
                                                          !is.na(SK_KLW))
  
  #Achterhalen welke KLW en welke VKX leden nog niet gematched zijn.
  data_KLW_VKX_Meta_Not_Join_VKX = data_KLW_VKX_Meta %>% dplyr::filter(is.na(SK_KLW))
  data_KLW_VKX_Meta_Not_Join_KLW = data_KLW_VKX_Meta %>% dplyr::filter(is.na(SK_VKX))
  
  #Test om te kijken of het aantal regels gelijk is.
  numberOfRows = (
    nrow(data_KLW_VKX_Meta_Join) + nrow(data_KLW_VKX_Meta_Not_Join_VKX) + nrow(data_KLW_VKX_Meta_Not_Join_KLW)
  )
  if (numberOfRows != nrow(data_KLW_VKX_Meta)) {
    stop("Aantal rijen in KLW match niet gelijk!")
  }
  
  percentage_gematched = round(nrow(data_KLW_VKX_Meta_Join) / nrow(data_VKX_Meta) * 100, 1)
  outputToLog("Percentage VKX bedrijven dat gematched is op basis van PK", percentage_gematched)
  
  
  if (nrow(data_KLW_VKX_Meta_Not_Join_VKX) > 0) {
    outputToLog(
      "Aantal VKX bedrijven dat 1e keer NIET gematched is",
      nrow(data_KLW_VKX_Meta_Not_Join_VKX)
    )
    
    data_KLW_VKX_Meta_Not_Join_VKX$SK = data_KLW_VKX_Meta_Not_Join_VKX$SK_VKX
    data_KLW_VKX_Meta_Not_Join_VKX = verwijderNAKolom(data_KLW_VKX_Meta_Not_Join_VKX)
    
    if (length(unique(data_KLW_VKX_Meta_Not_Join_VKX$SK)) != nrow(data_KLW_VKX_Meta_Not_Join_VKX)) {
      warning("SK van VKA lijst is niet uniek, matchen zal fout kunnen gaan!", NULL)
    }
    
  } else {
    
    #Alle VKX data is gematched, maar welke KLW hebben we nog over?
    data_KLW_Mismatch = rbind(data_KLW_Mismatch, data_KLW_VKX_Meta_Not_Join_KLW)
    
  } 
  
  if (nrow(data_KLW_VKX_Meta_Not_Join_KLW) > 0) {
    outputToLog(
      "Aantal KLW records dat 1e keer niet gematched is",
      nrow(data_KLW_VKX_Meta_Not_Join_KLW)
    )
    
    
    data_KLW_VKX_Meta_Not_Join_KLW$SK = data_KLW_VKX_Meta_Not_Join_KLW$SK_KLW
    data_KLW_VKX_Meta_Not_Join_KLW = verwijderNAKolom(data_KLW_VKX_Meta_Not_Join_KLW)
    
    if (length(unique(data_KLW_VKX_Meta_Not_Join_KLW$SK)) != nrow(data_KLW_VKX_Meta_Not_Join_KLW)) {
      warning("SK van KLW lijst is niet uniek, matchen zal fout kunnen gaan!",
                  "")
    }
    
  }
  
  #Joinen van data die niet gematched is op basis van PK gaan we nu joinen op basis van SK
  
  if (nrow(data_KLW_VKX_Meta_Not_Join_VKX) > 0 &
      nrow(data_KLW_VKX_Meta_Not_Join_KLW) > 0) {
    
    
    outputToLog("Start joining op basis van SK", NULL)
    
    data_KLW_VKX_Meta_2 = full_join(data_KLW_VKX_Meta_Not_Join_VKX,
                                    data_KLW_VKX_Meta_Not_Join_KLW,
                                    by = "SK")
    
    data_KLW_VKX_Meta_Join_2 = data_KLW_VKX_Meta_2 %>% dplyr::filter(!is.na(SK_VKX) &
                                                                !is.na(SK_KLW))
    data_KLW_VKX_Meta_Not_Join_VKX_2 = data_KLW_VKX_Meta_2 %>% dplyr::filter(is.na(SK_KLW))
    data_KLW_VKX_Meta_Not_Join_KLW_2 = data_KLW_VKX_Meta_2 %>% dplyr::filter(is.na(SK_VKX))
    
    if (nrow(data_KLW_VKX_Meta_Not_Join_VKX_2) > 0) {
      
      #Toevoegen van jaartal aan dataset voor overzicht van mismatches.
      data_KLW_VKX_Meta_Not_Join_VKX_2$jaartal_mismatch = j
      data_VKX_Mismatch = rbind(data_VKX_Mismatch, data_KLW_VKX_Meta_Not_Join_VKX_2)
      
      outputToLog(
        "Aantal VKX bedrijven dat 2e keer niet gematched is",
        nrow(data_KLW_VKX_Meta_Not_Join_VKX_2)
      )
      print(data_KLW_VKX_Meta_Not_Join_VKX_2 %>% dplyr::select(Achternaam, Plaats, Lidmaatschapsnummer, PK_VKX, SK_VKX, postcodeVKX))
    }
    
    if (nrow(data_KLW_VKX_Meta_Not_Join_KLW_2) > 0) {
      
      #Alle VKX data is gematched, maar welke KLW hebben we nog over?
      data_KLW_Mismatch = rbind(data_KLW_Mismatch, data_KLW_VKX_Meta_Not_Join_KLW_2)
      
      
      outputToLog(
        "Aantal KLW records dat 2e keer niet gematched is",
        nrow(data_KLW_VKX_Meta_Not_Join_KLW_2)
      )
      
    }
    
    
    #Alle gematchede data samenvoegen
    data_KLW_VKX_Meta_Matched = rbind(
      data_KLW_VKX_Meta_Join %>% dplyr::select(ID_KLW, Lidmaatschapsnummer, jaartal),
      data_KLW_VKX_Meta_Join_2 %>% dplyr::select(ID_KLW, Lidmaatschapsnummer, jaartal)
    )
  } else {
    data_KLW_VKX_Meta_Matched = rbind(data_KLW_VKX_Meta_Join %>% dplyr::select(ID_KLW, Lidmaatschapsnummer, jaartal))
  }
  
  percentage_gematched_totaal = round(nrow(data_KLW_VKX_Meta_Matched) / nrow(data_VKX_Meta) * 100, 1)
  outputToLog("Uiteindelijke percentage VKX bedrijven dat gematched is op basis van PK of SK voor dit jaar", percentage_gematched_totaal)
  

  #Data toevoegen aan iteratie van alle jaren.
  data_KLW_VKX_Meta_Matched_Totaal = rbind(data_KLW_VKX_Meta_Matched_Totaal,
                                           data_KLW_VKX_Meta_Matched)
  
}

totaal_mogelijke_combinaties = length(jaartallen) * nrow(data_VKX_Meta)

percentage_VKX = round( nrow(data_KLW_VKX_Meta_Matched_Totaal) / totaal_mogelijke_combinaties * 100, 1)
outputToLog("Percentage bedrijven van VKX in samengestelde dataset: ", percentage_VKX)


data_KLW_VKX_Meta_Matched_Totaal = data_KLW_VKX_Meta_Matched_Totaal %>% dplyr::select(!jaartal)

#VKX Lidmaatschapsnummer koppelen aan KLW data
data_KLW_input_VKX = full_join(data_KLW_VKX_Meta_Matched_Totaal, data_KLW_input, by = "ID_KLW")
#KLW data met VKX lidmaatschapsnummer koppelen aan VKX data
data_KLW_VKX = full_join(data_VKX_input, data_KLW_input_VKX, by = "Lidmaatschapsnummer")
data_KLW_VKX = data_KLW_VKX %>% select(
  Studiegroep,
  ID_KLW,
  jaartal,
  PK_VKX,
  SK_VKX,
  Achternaam,
  Straatnaam,
  Huisnummer,
  Postcode,
  Plaats,
  everything()
)
data_KLW_VKX = data_KLW_VKX %>% arrange(Studiegroep, Lidmaatschapsnummer, jaartal)

#Wegschrijven van missende KLW voor VKX leden:
write.xlsx(data_VKX_Mismatch, "VKX_Leden_Mismatch.xlsx", T, overwrite = T)

#Wegschrijven van niet gematchede KLW
write.xlsx(data_KLW_Mismatch, "KLW_Mismatch.xlsx", T, overwrite = T)

#Controleren KLW dataset. Output van de functie is een list met ID KLW die "fout" zijn.
foute_KLW_inputs = controleDatasetKLW(data_KLW_VKX)

min_jaartal = min(jaartallen)
max_jaartal = max(jaartallen)

#Selecteren van de foute KLW en parameters selecteren waarop gecontroleerd is.
foute_KLW = data_KLW_VKX %>% dplyr::filter(ID_KLW %in% foute_KLW_inputs) %>% dplyr::select(
  "Lidmaatschapsnummer",
  "SK_VKX",
  "jaartal",
  "naaminv",
  "vet",
  "eiwit",
  "ureum",
  "opb_mais_ds",
  "opb_graspr_ds",
  "melkpkoe",
  "rants_geh_p",
  "rants_geh_re",
  "rants_geh_vem"
) %>% dplyr::arrange(Lidmaatschapsnummer, jaartal)

write.xlsx( foute_KLW,  paste("foutieve_KLW_dataset_VKA_",min_jaartal,"_",max_jaartal,".xlsx",sep = ""), asTable = T, overwrite = T)


#Negeren van foute KLW in de dataset.
data_KLW_VKX_Schoon = data_KLW_VKX %>% dplyr::filter(!ID_KLW %in% foute_KLW_inputs)

#Nu dat de foute KLW zijn verwijderd, zijn er bedrijven die niet voor álle jaartallen een KLW hebben.
#We groeperen per lidmaatschapsnummer (VKX Lid) en sommeren dan het aantal observaties (aantal reseterende KLW jaren).
#Deze sommatie moet gelijk zijn aan het aantal jaren in de dataset. 
#Bijvoorbeeld, als we jaren 2018,2019 en 2020 willen in de dataset, dan moeten er voor ieder VKX lid 3 observaties zijn.
#Hieronder selecteren 
data_KLW_VKX_Schoon_Totaal = data_KLW_VKX_Schoon %>% 
  dplyr::select(Lidmaatschapsnummer, jaartal) %>%
  dplyr::group_by(Lidmaatschapsnummer) %>% 
  dplyr::summarise(countBedrijf = n()) %>% 
  dplyr::filter(countBedrijf == length(jaartallen))

data_KLW_VKX_Compleet = data_KLW_VKX_Schoon %>% dplyr::filter(Lidmaatschapsnummer %in% data_KLW_VKX_Schoon_Totaal$Lidmaatschapsnummer)

outputToLog("Aantal bedrijven in deze opgeschoonde VKX dataset: ",
            nrow(data_KLW_VKX_Schoon_Totaal))

percentage_VKX = round( nrow(data_KLW_VKX_Schoon_Totaal) / nrow(data_VKX_Meta) * 100, 1)
outputToLog("Percentage bedrijven van VKX in opgeschoonde dataset: ", percentage_VKX)

data_KLW_VKX_Compleet = data_KLW_VKX_Compleet %>% dplyr::arrange(Lidmaatschapsnummer, jaartal)

#Wegschrijven dataset naar excel.
#Zorgen dat de kolomnamen uniek zijn
names_df = make.unique(colnames(data_KLW_VKX_Compleet),sep = "_")
colnames(data_KLW_VKX_Compleet) = names_df
write.xlsx(
  data_KLW_VKX_Compleet,
  paste("dataset_",type,"_", min_jaartal, "_", max_jaartal, ".xlsx", sep = ""),
  asTable = F, overwrite = T
)
}