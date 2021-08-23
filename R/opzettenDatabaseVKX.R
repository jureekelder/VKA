outputToLog <- function(name, quantity) {
  cat(paste(name, ": \n"))
  cat(paste(toString(quantity), "\n"))
  cat("\n")
  
}

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

getDataInFolder <-
  function(x,
           patternFile = NULL,
           sheetList = 1,
           allSheets = FALSE,
           fieldSeperator = ";") {
    setwd(x)
    outputToLog("Map voor inputdata is nu:", x)
    outputToLog("Zoeken naar bestanden die hetvolgende bevatten:", patternFile)
    
    if (is.null(patternFile)) {
      fileList = list.files()
    } else {
      fileList = list.files(pattern = patternFile)
    }
    
    outputToLog("Aantal gevonden bestanden:", length(fileList))
    print(fileList)
    
    if (length(fileList) > 0) {
      for (j in 1:length(fileList)) {
        excelsheet = FALSE
        fileName = fileList[j]
        #Als een bestand nog geopend is dan vind R ook een kopie ervan in de map, deze geeft problemen dus overslaan!
        if (grepl("~", fileName) || grepl("output", fileName)) {
          next
        }
        
        if (grepl(".xl", fileName)) {
          excelsheet = TRUE
        }
        
        if (excelsheet) {
          if (allSheets) {
            sheetList = excel_sheets(fileName)
            outputToLog("Gevonden sheets", sheetList)
          }
          
          sheetnameAsColum = FALSE
          if (length(sheetList) > 1) {
            sheetnameAsColum = TRUE
          }
          
        }
        
        
        #Voor het huidige bestand; lees de relevante tabbladen
        for (i in 1:length(sheetList)) {
          sheet = sheetList[i]
          
          if (excelsheet) {
            tempData = read_excel(fileName, sheet = sheet)
            
            
            if (sheetnameAsColum) {
              tempData$sheetname = sheet
            }
            
          } else {
            #tempData = read.csv(file = fileName, header = TRUE, sep = fieldSeperator, dec = ",", colClasses = c("aanleg_sm_hoev" = "integer"))
            tempData = read.delim(
              file = fileName,
              header = TRUE,
              sep = fieldSeperator,
              dec = ","
            )
            #tempData = utils::read.table(file= fileName, sep=fieldSeperator, dec=",", colClasses="character")
          }
          
          
          if (i == 1) {
            dataFile = tempData
          } else {
            dataFile = merge(dataFile, tempData, all = TRUE)
          }
          
        }
        
        if (j == 1) {
          dataAllFiles = dataFile
        } else {
          if (length(colnames(dataFile)) == length(colnames(dataAllFiles))) {
            if (all(colnames(dataFile) == colnames(dataAllFiles))) {
              dataAllFiles = rbind(dataAllFiles, dataFile)
            } else {
              dataAllFiles = cbind(dataAllFiles, dataFile)
            }
          } else {
            dataAllFiles = cbind(dataAllFiles, dataFile)
          }
        }
        
      }
    } else {
      dataAllFiles = NA
    }
    
    return(dataAllFiles)
  }



dataset = "VKA" #"VKA" of "VKO"
#jaartallen = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
jaartallen = c(2018,2019,2020)

#Zet working directory naar de locatie van dit script. Ervan uitgaande dat RStudio gebruikt wordt:
main_directory = dirname(rstudioapi::getActiveDocumentContext()$path)
main_directory = paste(main_directory, "/", dataset, sep = "")
setwd(main_directory)
file_list_main_directory = list.files()

####Voorbereiden VKX dataset####
VKX_ledenlijst_locatie = "Ledenlijst"

if (any(grepl(VKX_ledenlijst_locatie, file_list_main_directory))) {
  path = paste(main_directory, "/", VKX_ledenlijst_locatie, sep = "")
  data_VKX_input = getDataInFolder(path)
  
} else {
  stop("VKX ledenlijst niet kunnen vinden!")
}

#Welke kolommen van VKX ledenbestand willen we behouden?
kolommen_ledenlijst = c(
  "Lidmaatschapsnummer",
  "Studiegroep",
  "Tussenvoegsel",
  "Achternaam",
  "Straatnaam",
  "Huisnummer",
  "Huisnummertoevoeging",
  "Postcode",
  "Plaats",
  "BES"
)
if (all(kolommen_ledenlijst %in% colnames(data_VKX_input))) {
  data_VKX_input = data_VKX_input[, kolommen_ledenlijst]
} else {
  stop("Ledenbestand heeft niet de juiste headers")
}


outputToLog("Aantal observaties in Volledige VKX data", nrow(data_VKX_input))

#Verzeker dat de Lidmaatschapnummers en studiegroepnummers gezien worden als numeric voor het joinen later.
data_VKX_input$Lidmaatschapsnummer = as.integer(data_VKX_input$Lidmaatschapsnummer)
data_VKX_input$Studiegroep = as.integer(data_VKX_input$Studiegroep)

outputToLog("Negeren van loonwerkers en lege velden...", "")

data_VKX_input = data_VKX_input %>%
  dplyr::filter(Lidmaatschapsnummer > 0) %>%
  dplyr::filter(!Studiegroep %in% c(30)) #Groep 30 zijn de loonwerkers

outputToLog("Aantal leden in gefilterde VKX ledenbestand",
            nrow(data_VKX_input))

#Logica om van de postcode en huisnummer combinatie een primary key te maken, bijvoorbeeld Kapelweg 36, 7134 RJ wordt --> 7134RJ36
if (TRUE) {
  data_VKX_input$postcode_temp = gsub(" ", "", data_VKX_input$Postcode, fixed = T) #verwijderen van spaties
  data_VKX_input$postcode_temp = toupper(data_VKX_input$postcode_temp) #naar hoofdletters!
  data_VKX_input$huisnummer = sapply(data_VKX_input$Huisnummer, getNumbersFromString)
  data_VKX_input$PK_VKX = paste(data_VKX_input$postcode_temp,
                                data_VKX_input$huisnummer,
                                sep = "")
  data_VKX_input$PK = data_VKX_input$PK_VKX
  data_VKX_input$postcodeVKX = data_VKX_input$postcode_temp
}

#Logica om van de naam en de plaats een secondary key te maken
if (TRUE) {
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
}

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



#Functie om .DMPX bestanden in te lezen en voor te bereiden
voorbereidenKLWData <- function(folderLocation) {
  folderList = list.files(folderLocation)
  
  data_KLW_input = NULL
  
  for (folder in folderList) {
    path = paste(main_directory, "/", DUMP_files_locatie, "/", folder, sep =
                   "")
    data_KLW_input_folder = getDataInFolder(path, patternFile = ".dmp", fieldSeperator = "@")
    data_KLW_input_folder = data_KLW_input_folder[,!duplicated(colnames(data_KLW_input_folder))]
    
    data_KLW_input = rbind(data_KLW_input, data_KLW_input_folder)
    
  }
  
  is_all_numeric <- function(x) {
    !any(is.na(suppressWarnings(as.numeric(na.omit(
      x
    ))))) & is.character(x)
  }
  
  data_KLW_input = data_KLW_input %>% mutate_if(is_all_numeric, as.numeric)
  
  #columns_numeric = ("jaartal")
  #data_KLW_input = data_KLW_input %>% mutate_at(columns_numeric, as.numeric)
  
  
  if (length(unique(data_KLW_input$versie)) > 1) {
    stop("Verschillende KLW-versies in dataset! Haal alles door de dezelfde rekenmotor!")
  }
  
  outputToLog("Aantal observaties in volledige KLW data",
              nrow(data_KLW_input))
  outputToLog("Aantal versienummers KLW-versie gevonden", length(unique(data_KLW_input$versie)))
  outputToLog("Gevonden versienummers KLW-versie", (unique(data_KLW_input$versie)))
  
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
  
  data_KLW_input = data_KLW_input %>% filter(jaartal %in% jaartallen)
  
  data_KLW_input_duplicates = duplicated(data_KLW_input %>% select(jaartal, PK_KLW))
  
  
  if (any(data_KLW_input_duplicates)) {
    outputToLog("Dubbele KLW data in bestand: ", data_KLW_input[data_KLW_input_duplicates, c("naaminv", "jaartal")])
    
    data_KLW_input = distinct(data_KLW_input, jaartal, PK_KLW, .keep_all = T)
  }
  
  
  #Verzeker dat het jaartal gezien wordt als numeric voor het joinen later
  data_KLW_input$jaartal = as.integer(data_KLW_input$jaartal)
  outputToLog("Jaartallen in KLW data", unique(data_KLW_input$jaartal))
  
  #Geef iedere KLW een uniek nummer
  data_KLW_input$ID_KLW = seq(nrow(data_KLW_input))
  
  #Toevoegen nieuwe kengetallen
  
  Kengetallen_Script_locatie = "KengetallenScript"
  if (any(grepl(Kengetallen_Script_locatie, file_list_main_directory))) {
    path = paste(main_directory, "/", Kengetallen_Script_locatie, sep = "")
    files = list.files(path, pattern = ".R")
    source(paste(path, "/", files[1], sep = ""))
    
  } else {
    stop("ToevoegenKengetallenScript niet kunnen vinden!")
  }
  
  
  print(str(data_KLW_input))
  
  data_KLW_input_columns = t(sapply(data_KLW_input, class))
  
  
  data_KLW_input$pceigen_n = as.numeric(sapply(data_KLW_input$pceigen_n, getNumbersFromString))
  data_KLW_input$kring1_benut_tot = as.numeric(sapply(data_KLW_input$kring1_benut_tot, getNumbersFromString))
  data_KLW_input$kring2_benut_tot = as.numeric(sapply(data_KLW_input$kring2_benut_tot, getNumbersFromString))
  data_KLW_input$kring1_benut_bod = as.numeric(sapply(data_KLW_input$kring1_benut_bod, getNumbersFromString))
  data_KLW_input$kring2_benut_bod = as.numeric(sapply(data_KLW_input$kring2_benut_bod, getNumbersFromString))
  data_KLW_input$kring1_benut_mst = as.numeric(sapply(data_KLW_input$kring1_benut_mst, getNumbersFromString))
  
  data_KLW_input = toevoegenVariabelenKLW(data_KLW_input)
  
  
  data_KLW_input = data_KLW_input %>% select(ID_KLW,
                                             PK_KLW,
                                             SK_KLW,
                                             jaartal,
                                             naaminv,
                                             postcode,
                                             huisnummer,
                                             everything())
  
  return(data_KLW_input)
}



#Mapnaam voor .DMPX bestanden
DUMP_files_locatie = "KLW_DUMP"
if (any(grepl(DUMP_files_locatie, file_list_main_directory))) {
  path = paste(main_directory, "/", DUMP_files_locatie, sep = "")
  
  data_KLW_input = voorbereidenKLWData(path)
  
  
  
} else {
  stop("DUMP-files niet kunnen vinden!")
}

str(data_KLW_input)



#Vergelijken van database van ALLE KLW versus wat er in de DUMP files staat.
data_gerjan = vergelijkenKLWBestanden("C:/Users/JurEekelder/OneDrive - Cooperatie De Marke U.A/DatabaseVKA/VKA")

if (TRUE) {
  data_gerjan$naaminv_geenjaar = sapply(data_gerjan$naam_plaats, getCharactersFromString)
  data_gerjan$naaminv_geenjaar = tolower(gsub(" ", "", data_gerjan$naaminv_geenjaar, fixed = T))
}

data_KLW_naam_plaats = data_KLW_input %>% select(naaminv_geenjaar, jaartal)
data_gerjan = data_gerjan %>% select(naaminv_geenjaar, jaartal)
summary(comparedf(
  data_gerjan,
  data_KLW_naam_plaats,
  by = c("naaminv_geenjaar", "jaartal")
))

#Controleren KLW dataset

controleDataset_Script_locatie = "controleDatasetScript"
if (any(grepl(controleDataset_Script_locatie, file_list_main_directory))) {
  path = paste(main_directory, "/", controleDataset_Script_locatie, sep =
                 "")
  files = list.files(path, pattern = ".R")
  source(paste(path, "/", files[1], sep = ""))
  
} else {
  stop("controleDatasetScript niet kunnen vinden!")
}

foute_KLW_inputs = controleDataset(data_KLW_input)

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
data_KLW_SK_jaar = data_KLW_input %>% select(ID_KLW, SK_KLW, jaartal)

data_KLW_SK_jaar_count = data_KLW_SK_jaar %>% group_by(SK_KLW, jaartal) %>% dplyr::summarise(count = dplyr::n())


####KOPPELEN VAN VKX LIDMAATSCHAPSNUMMER AAN KLW####
verwijderNAKolom <- function(x) {
  y = x[, colSums(is.na(x)) != nrow(x)]
  return(y)
}

setwd(main_directory)

if (TRUE) {
  data_KLW_VKX_Meta_Matched_Totaal = NULL
  
  for (j in jaartallen) {
    outputToLog("VKA leden koppelen aan KLW voor jaartal", j)
    
    #VKX ledenbestand aan KLW data koppelen op basis van de PK
    data_KLW_Meta_Jaar = data_KLW_Meta %>% filter(jaartal == j)
    data_KLW_VKX_Meta = full_join(data_VKX_Meta, data_KLW_Meta_Jaar, by = "PK")
    
    data_KLW_VKX_Meta_Join = data_KLW_VKX_Meta %>% filter(!is.na(SK_VKX) &
                                                            !is.na(SK_KLW))
    data_KLW_VKX_Meta_Not_Join_VKX = data_KLW_VKX_Meta %>% filter(is.na(SK_KLW))
    data_KLW_VKX_Meta_Not_Join_KLW = data_KLW_VKX_Meta %>% filter(is.na(SK_VKX))
    
    numberOfRows = (
      nrow(data_KLW_VKX_Meta_Join) + nrow(data_KLW_VKX_Meta_Not_Join_VKX) + nrow(data_KLW_VKX_Meta_Not_Join_KLW)
    )
    if (numberOfRows != nrow(data_KLW_VKX_Meta)) {
      print("ERROR")
    }
    
    
    if (nrow(data_KLW_VKX_Meta_Not_Join_VKX) > 0) {
      outputToLog(
        "Aantal VKX bedrijven dat 1e keer niet gematched is",
        nrow(data_KLW_VKX_Meta_Not_Join_VKX)
      )
      
      data_KLW_VKX_Meta_Not_Join_VKX$SK = data_KLW_VKX_Meta_Not_Join_VKX$SK_VKX
      data_KLW_VKX_Meta_Not_Join_VKX = verwijderNAKolom(data_KLW_VKX_Meta_Not_Join_VKX)
      
      if (length(unique(data_KLW_VKX_Meta_Not_Join_VKX$SK)) != nrow(data_KLW_VKX_Meta_Not_Join_VKX)) {
        outputToLog("SK van VKA lijst is niet uniek, matchen zal fout kunnen gaan!",
                    "")
      }
      
    }
    
    if (nrow(data_KLW_VKX_Meta_Not_Join_KLW) > 0) {
      outputToLog(
        "Aantal KLW records dat 1e keer niet gematched is",
        nrow(data_KLW_VKX_Meta_Not_Join_KLW)
      )
      
      data_KLW_VKX_Meta_Not_Join_KLW$SK = data_KLW_VKX_Meta_Not_Join_KLW$SK_KLW
      data_KLW_VKX_Meta_Not_Join_KLW = verwijderNAKolom(data_KLW_VKX_Meta_Not_Join_KLW)
      
      if (length(unique(data_KLW_VKX_Meta_Not_Join_KLW$SK)) != nrow(data_KLW_VKX_Meta_Not_Join_KLW)) {
        outputToLog("SK van KLW lijst is niet uniek, matchen zal fout kunnen gaan!",
                    "")
      }
      
    }
    
    #Joinen van data die niet gematched is op basis van SK
    
    if (nrow(data_KLW_VKX_Meta_Not_Join_VKX) > 0 &
        nrow(data_KLW_VKX_Meta_Not_Join_KLW) > 0) {
      data_KLW_VKX_Meta_2 = full_join(data_KLW_VKX_Meta_Not_Join_VKX,
                                      data_KLW_VKX_Meta_Not_Join_KLW,
                                      by = "SK")
      
      data_KLW_VKX_Meta_Join_2 = data_KLW_VKX_Meta_2 %>% filter(!is.na(SK_VKX) &
                                                                  !is.na(SK_KLW))
      data_KLW_VKX_Meta_Not_Join_VKX_2 = data_KLW_VKX_Meta_2 %>% filter(is.na(SK_KLW))
      data_KLW_VKX_Meta_Not_Join_KLW_2 = data_KLW_VKX_Meta_2 %>% filter(is.na(SK_VKX))
      
      if (nrow(data_KLW_VKX_Meta_Not_Join_VKX_2) > 0) {
        outputToLog(
          "Aantal VKX bedrijven dat 2e keer niet gematched is",
          nrow(data_KLW_VKX_Meta_Not_Join_VKX_2)
        )
        print(data_KLW_VKX_Meta_Not_Join_VKX_2)
      }
      
      if (nrow(data_KLW_VKX_Meta_Not_Join_KLW_2) > 0) {
        outputToLog(
          "Aantal KLW records dat 2e keer niet gematched is",
          nrow(data_KLW_VKX_Meta_Not_Join_KLW_2)
        )
        
      }
      
      
      
      data_KLW_VKX_Meta_Matched = rbind(
        data_KLW_VKX_Meta_Join %>% select(ID_KLW, Lidmaatschapsnummer, jaartal),
        data_KLW_VKX_Meta_Join_2 %>% select(ID_KLW, Lidmaatschapsnummer, jaartal)
      )
    } else {
      data_KLW_VKX_Meta_Matched = rbind(data_KLW_VKX_Meta_Join %>% select(ID_KLW, Lidmaatschapsnummer, jaartal))
    }
    
    data_KLW_VKX_Meta_Matched_Totaal = rbind(data_KLW_VKX_Meta_Matched_Totaal,
                                             data_KLW_VKX_Meta_Matched)
    
  }
  
  write.xlsx(
    full_join(data_KLW_VKX_Meta_Matched_Totaal, data_VKX_Meta),
    "matches.xlsx",
    asTable = T
  )
  data_KLW_VKX_Meta_Matched_Totaal = data_KLW_VKX_Meta_Matched_Totaal %>% select(!jaartal)
  
  
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
  
  
  VKX_lid_nummers = data.frame(Lidmaatschapsnummer = unique(data_VKX_input$Lidmaatschapsnummer))
  VKX_lid_jaartal = merge(data.frame(jaartal = jaartallen), VKX_lid_nummers)
  
  data_VKX_KLW_Jaartal_ID = data_KLW_VKX %>% dplyr::select(Lidmaatschapsnummer, jaartal, ID_KLW)
  
  KLW_matched = left_join(VKX_lid_jaartal, data_VKX_KLW_Jaartal_ID)
  
  
  
  #Voor welke boeren zijn er in welk jaar geen KLW gevonden???
  if (any(is.na(KLW_matched$ID_KLW))) {
    #Voor een deelnemer is er voor een bepaald jaar geen KLW aanwezig
    
    VKX_lid_geen_KLW = KLW_matched %>% filter(is.na(ID_KLW))
    
    data_totaal = NULL
    
    for (j in unique(VKX_lid_geen_KLW$jaartal)) {
      outputToLog("Voor dit jaartal zijn er bij enkele boeren geen KLW gevonden",
                  j)
      
      VKX_lid_geen_KLW_jaar = VKX_lid_geen_KLW %>% filter(jaartal == j)
      
      outputToLog(
        "Voor dit jaartal zijn er bij zoveel boeren geen KLW gevonden",
        nrow(VKX_lid_geen_KLW_jaar)
      )
      
      VKX_lid_geen_KLW_jaar_VKX_Meta = left_join(VKX_lid_geen_KLW_jaar, data_VKX_Meta)
      
      #Welke VKX leden hebben geen KLW gekoppeld?
      VKX_lid_jaar_niet_gematched = VKX_lid_geen_KLW_jaar_VKX_Meta %>%
        dplyr::select(Studiegroep,
                      Achternaam,
                      Plaats,
                      jaartal,
                      Lidmaatschapsnummer,
                      SK_VKX) %>%
        dplyr::arrange(Studiegroep)
      
      outputToLog("Niet gematchte VKX leden met KLW", "")
      print(VKX_lid_jaar_niet_gematched)
      
      data_totaal = rbind(data_totaal, VKX_lid_jaar_niet_gematched)
      
    }
    
    write.xlsx(data_totaal, "missende_KLW.xlsx", asTable = T)
    
    missende_KLW_samenvatting = data_totaal %>% group_by(Lidmaatschapsnummer, SK_VKX) %>% dplyr::summarise(
      count = n(),
      maxJaar = max(jaartal),
      minJaar = min(jaartal)
    )
    
    #KLW die nog wel gematched zouden moeten worden:
    missende_KLW_samenvatting_selectie = missende_KLW_samenvatting %>% filter(minJaar > min(jaartallen))
    outputToLog(
      "Voor deze bedrijven is er wellicht wel een KLW:",
      nrow(missende_KLW_samenvatting_selectie)
    )
    
  }
  
  
}

#Elimineren van foutieve KLW

setwd(main_directory)
min_jaartal = min(jaartallen)
max_jaartal = max(jaartallen)

foute_KLW = data_KLW_VKX %>% filter(ID_KLW %in% foute_KLW_inputs) %>% select(
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
) %>% arrange(Lidmaatschapsnummer, jaartal)
write.xlsx(
  foute_KLW,
  paste(
    "foutieve_KLW_dataset_VKA_",
    min_jaartal,
    "_",
    max_jaartal,
    ".xlsx",
    sep = ""
  )
)

foute_KLW_Lid = unique.data.frame(foute_KLW %>% select(Lidmaatschapsnummer, SK_VKX))

data_KLW_VKX_Schoon = data_KLW_VKX %>% filter(!ID_KLW %in% foute_KLW_inputs)

data_KLW_VKX_Schoon_Totaal = data_KLW_VKX_Schoon %>% group_by(Lidmaatschapsnummer) %>% dplyr::summarise(countBedrijf = n()) %>% filter (countBedrijf == length(jaartallen))#%>% group_by(countBedrijf) %>% summarise(countJaar = n())

data_KLW_VKX_Compleet = data_KLW_VKX_Schoon %>% filter(Lidmaatschapsnummer %in% data_KLW_VKX_Schoon_Totaal$Lidmaatschapsnummer)

outputToLog("Aantal bedrijven in deze dataset: ",
            nrow(data_KLW_VKX_Schoon_Totaal))

data_KLW_VKX_Compleet = data_KLW_VKX_Compleet %>% arrange(Lidmaatschapsnummer, jaartal)


write.xlsx(
  data_KLW_VKX_Compleet,
  paste("dataset_VKA_", min_jaartal, "_", max_jaartal, ".xlsx", sep = "")
)